# pylint: disable=missing-module-docstring,missing-class-docstring,missing-function-docstring

from dataclasses import dataclass
from typing import List, Optional, Dict, Any, Tuple
import re
import lldb


LLGO_DEBUGGER_MARKER_PREFIX = "__llgo_debugger_marker_v"
LLGO_DEBUGGER_SCHEMAS = {
    "__llgo_debugger_marker_v1": (1, 1),
}
_TARGET_INFO_CACHE: Dict[Tuple[Any, ...], "LLGoTargetInfo"] = {}


@dataclass(frozen=True)
class LLGoTargetInfo:
    marker_versions: Tuple[int, ...]
    schema_version: Optional[int]
    runtime_layout_version: Optional[int]
    triple: str
    pointer_size: int
    byte_order: str

    @property
    def supported(self) -> bool:
        return self.schema_version is not None


def log(*args: Any, **kwargs: Any) -> None:
    print(*args, **kwargs, flush=True)


def __lldb_init_module(debugger: lldb.SBDebugger, _: Dict[str, Any]) -> None:
    register_commands(debugger)


def register_commands(debugger: lldb.SBDebugger) -> None:
    debugger.HandleCommand('command container add llgo')
    debugger.HandleCommand(
        'command script add -f llgo_plugin.print_target_status llgo status')
    debugger.HandleCommand(
        'command script add -f llgo_plugin.print_go_expression llgo print')
    debugger.HandleCommand(
        'command script add -f llgo_plugin.print_all_variables llgo vars')


def _marker_versions(target: lldb.SBTarget) -> Tuple[int, ...]:
    if not target or not target.IsValid():
        return ()

    versions = set()
    marker_pattern = re.compile(
        rf"^{re.escape(LLGO_DEBUGGER_MARKER_PREFIX)}([0-9]+)$")
    for module_index in range(target.GetNumModules()):
        module = target.GetModuleAtIndex(module_index)
        for symbol_index in range(module.GetNumSymbols()):
            name = module.GetSymbolAtIndex(symbol_index).GetName()
            match = marker_pattern.match(name or "")
            if match:
                versions.add(int(match.group(1)))
    return tuple(sorted(versions))


def _byte_order_name(byte_order: int) -> str:
    return {
        lldb.eByteOrderBig: "big",
        lldb.eByteOrderPDP: "pdp",
        lldb.eByteOrderLittle: "little",
    }.get(byte_order, "unknown")


def _target_cache_key(target: lldb.SBTarget) -> Tuple[Any, ...]:
    modules = []
    for index in range(target.GetNumModules()):
        module = target.GetModuleAtIndex(index)
        modules.append((
            module.GetUUIDString() or "",
            str(module.GetFileSpec()),
        ))
    return (
        target.GetTriple() or "",
        target.GetAddressByteSize(),
        target.GetByteOrder(),
        tuple(modules),
    )


def inspect_target(target: lldb.SBTarget) -> LLGoTargetInfo:
    if not target or not target.IsValid():
        return LLGoTargetInfo((), None, None, "", 0, "unknown")

    cache_key = _target_cache_key(target)
    cached = _TARGET_INFO_CACHE.get(cache_key)
    if cached is not None:
        return cached

    marker_versions = _marker_versions(target)
    schema_version: Optional[int] = None
    runtime_layout_version: Optional[int] = None
    # Multiple markers are ambiguous: do not select a runtime layout merely
    # because one of the advertised schema versions happens to be supported.
    if len(marker_versions) == 1:
        candidate = marker_versions[0]
        for supported_schema, supported_runtime_layout in (
                LLGO_DEBUGGER_SCHEMAS.values()):
            if candidate == supported_schema:
                schema_version = supported_schema
                runtime_layout_version = supported_runtime_layout
                break

    info = LLGoTargetInfo(
        marker_versions=marker_versions,
        schema_version=schema_version,
        runtime_layout_version=runtime_layout_version,
        triple=target.GetTriple() or "",
        pointer_size=target.GetAddressByteSize(),
        byte_order=_byte_order_name(target.GetByteOrder()),
    )
    _TARGET_INFO_CACHE[cache_key] = info
    return info


def target_status(info: LLGoTargetInfo) -> str:
    if not info.marker_versions:
        return "Not an LLGo target; raw LLDB debugging remains available."
    if not info.supported:
        versions = ", ".join(f"v{version}"
                             for version in info.marker_versions)
        return (
            f"Unsupported LLGo debugger marker version(s): {versions}; "
            "raw LLDB debugging remains available."
        )
    return (
        f"LLGo debugger schema v{info.schema_version} "
        f"(runtime layout v{info.runtime_layout_version}); "
        f"target {info.triple}; pointer size {info.pointer_size}; "
        f"byte order {info.byte_order}."
    )


def is_llgo_compiler(target: lldb.SBTarget) -> bool:
    return inspect_target(target).supported


def print_target_status(debugger: lldb.SBDebugger, _command: str, result: lldb.SBCommandReturnObject, _internal_dict: Dict[str, Any]) -> None:
    result.AppendMessage(target_status(
        inspect_target(debugger.GetSelectedTarget())))


def _require_supported_target(debugger: lldb.SBDebugger, result: lldb.SBCommandReturnObject) -> bool:
    info = inspect_target(debugger.GetSelectedTarget())
    if info.supported:
        return True
    result.SetError(target_status(info))
    return False


def _selected_stopped_frame(debugger: lldb.SBDebugger, result: lldb.SBCommandReturnObject) -> Optional[lldb.SBFrame]:
    target = debugger.GetSelectedTarget()
    if not target or not target.IsValid():
        result.SetError("LLGo command requires a valid target.")
        return None

    process = target.GetProcess()
    if (not process or not process.IsValid() or
            process.GetState() != lldb.eStateStopped):
        result.SetError("LLGo command requires a stopped process.")
        return None

    thread = process.GetSelectedThread()
    if not thread or not thread.IsValid():
        result.SetError("LLGo command requires a selected thread.")
        return None

    frame = thread.GetSelectedFrame()
    if not frame or not frame.IsValid():
        result.SetError("LLGo command requires a selected frame.")
        return None
    return frame


def _value_as_int(value: lldb.SBValue) -> Optional[int]:
    if not value or not value.IsValid():
        return None
    raw = value.GetValue()
    if raw is None:
        return None
    try:
        return int(raw, 0)
    except (TypeError, ValueError):
        return None


def get_indexed_value(value: lldb.SBValue, index: int) -> Optional[lldb.SBValue]:
    if not value or not value.IsValid():
        return None

    type_name = value.GetType().GetName()

    if type_name.startswith('[]'):  # Slice
        data_ptr = value.GetChildMemberWithName('data')
        element_type = data_ptr.GetType().GetPointeeType()
        element_size = element_type.GetByteSize()
        ptr_value = _value_as_int(data_ptr)
        if ptr_value is None:
            return None
        element_address = ptr_value + index * element_size
        target = value.GetTarget()
        return target.CreateValueFromAddress(
            f"element_{index}", lldb.SBAddress(element_address, target), element_type)
    elif value.GetType().IsArrayType():  # Array
        return value.GetChildAtIndex(index)
    else:
        return None


def find_variable(frame: lldb.SBFrame, name: str) -> lldb.SBValue:
    value = frame.FindVariable(name)
    if value and value.IsValid():
        return value
    target = frame.GetThread().GetProcess().GetTarget()
    return target.FindFirstGlobalVariable(name)


def evaluate_expression(frame: lldb.SBFrame, expression: str) -> Optional[lldb.SBValue]:
    parts = re.findall(r'\*|\w+|\(|\)|\[.*?\]|\.', expression)
    if not parts or "".join(parts) != re.sub(r"\s+", "", expression):
        return None

    def evaluate_part(i: int) -> Tuple[Optional[lldb.SBValue], int]:
        nonlocal parts
        value: Optional[lldb.SBValue] = None
        while i < len(parts):
            part = parts[i]

            if part == '*':
                sub_value, i = evaluate_part(i + 1)
                if sub_value and sub_value.IsValid():
                    value = sub_value.Dereference()
                else:
                    return None, i
            elif part == '(':
                depth = 1
                j = i + 1
                while j < len(parts) and depth > 0:
                    if parts[j] == '(':
                        depth += 1
                    elif parts[j] == ')':
                        depth -= 1
                    j += 1
                if depth != 0:
                    return None, j
                value, i = evaluate_part(i + 1)
                i = j - 1
            elif part == ')':
                return value, i + 1
            elif part == '.':
                if i + 1 >= len(parts) or not re.fullmatch(r'\w+', parts[i + 1]):
                    return None, i + 1
                if value is None:
                    value = find_variable(frame, parts[i+1])
                else:
                    value = value.GetChildMemberWithName(parts[i+1])
                i += 2
            elif part.startswith('['):
                try:
                    index = int(part[1:-1])
                except ValueError:
                    return None, i + 1
                value = get_indexed_value(value, index)
                i += 1
            else:
                if value is None:
                    value = find_variable(frame, part)
                else:
                    value = value.GetChildMemberWithName(part)
                i += 1

            if not value or not value.IsValid():
                return None, i

        return value, i

    value, _ = evaluate_part(0)
    return value


def print_go_expression(debugger: lldb.SBDebugger, command: str, result: lldb.SBCommandReturnObject, _internal_dict: Dict[str, Any]) -> None:
    if not _require_supported_target(debugger, result):
        return
    frame = _selected_stopped_frame(debugger, result)
    if frame is None:
        return
    value = evaluate_expression(frame, command)
    if value and value.IsValid():
        try:
            result.AppendMessage(format_value(value, debugger))
        except (IndexError, TypeError, ValueError) as error:
            result.SetError(f"Unable to format expression {command!r}: {error}")
    else:
        result.SetError(
            f"Error: Unable to evaluate expression '{command}'")


def print_all_variables(debugger: lldb.SBDebugger, _command: str, result: lldb.SBCommandReturnObject, _internal_dict: Dict[str, Any]) -> None:
    if not _require_supported_target(debugger, result):
        return

    frame = _selected_stopped_frame(debugger, result)
    if frame is None:
        return
    variables = frame.GetVariables(True, True, True, True)

    output: List[str] = []
    try:
        for var in variables:
            type_name = map_type_name(var.GetType().GetName())
            formatted = format_value(
                var, debugger, include_type=False, indent=0)
            output.append(f"var {var.GetName()} {type_name} = {formatted}")
    except (IndexError, TypeError, ValueError) as error:
        result.SetError(f"Unable to format LLGo variables: {error}")
        return

    result.AppendMessage("\n".join(output))


def is_pointer(frame: lldb.SBFrame, var_name: str) -> bool:
    var = find_variable(frame, var_name)
    return var.IsValid() and var.GetType().IsPointerType()


def format_value(var: lldb.SBValue, debugger: lldb.SBDebugger, include_type: bool = True, indent: int = 0) -> str:
    if not var.IsValid():
        return "<variable not available>"

    var_type = var.GetType()
    type_class = var_type.GetTypeClass()
    type_name = map_type_name(var_type.GetName())

    # Handle typedef types
    original_type_name = type_name
    while var_type.IsTypedefType():
        var_type = var_type.GetTypedefedType()
        type_name = map_type_name(var_type.GetName())
        type_class = var_type.GetTypeClass()

    if var_type.IsPointerType():
        return format_pointer(var, debugger, indent, original_type_name)

    if type_name.startswith('[]'):  # Slice
        return format_slice(var, debugger, indent)
    elif var_type.IsArrayType():
        return format_array(var, debugger, indent)
    elif type_name == 'string':  # String
        return format_string(var)
    elif type_class in [lldb.eTypeClassStruct, lldb.eTypeClassClass]:
        return format_struct(var, debugger, include_type, indent, original_type_name)
    else:
        value = var.GetValue()
        summary = var.GetSummary()
        if value is not None:
            return f"{value}" if include_type else str(value)
        elif summary is not None:
            return f"{summary}" if include_type else summary
        else:
            return "<variable not available>"


def format_slice(var: lldb.SBValue, debugger: lldb.SBDebugger, indent: int) -> str:
    length = var.GetChildMemberWithName('len').GetValue()
    if length is None:
        return "<variable not available>"
    length = int(length)
    data_ptr = var.GetChildMemberWithName('data')
    elements: List[str] = []

    ptr_value = _value_as_int(data_ptr)
    if ptr_value is None:
        return "<variable not available>"
    element_type = data_ptr.GetType().GetPointeeType()
    element_size = element_type.GetByteSize()

    target = debugger.GetSelectedTarget()
    indent_str = '  ' * indent
    next_indent_str = '  ' * (indent + 1)

    for i in range(length):
        element_address = ptr_value + i * element_size
        element = target.CreateValueFromAddress(
            f"element_{i}", lldb.SBAddress(element_address, target), element_type)
        value = format_value(
            element, debugger, include_type=False, indent=indent+1)
        elements.append(value)

    type_name = var.GetType().GetName()

    if len(elements) > 5:  # 如果元素数量大于5，则进行折行显示
        result = f"{type_name}{{\n{next_indent_str}" + \
            f",\n{next_indent_str}".join(elements) + f"\n{indent_str}}}"
    else:
        result = f"{type_name}{{{', '.join(elements)}}}"

    return result


def format_array(var: lldb.SBValue, debugger: lldb.SBDebugger, indent: int) -> str:
    elements: List[str] = []
    indent_str = '  ' * indent
    next_indent_str = '  ' * (indent + 1)

    for i in range(var.GetNumChildren()):
        value = format_value(var.GetChildAtIndex(
            i), debugger, include_type=False, indent=indent+1)
        elements.append(value)

    array_size = var.GetNumChildren()
    element_type = map_type_name(var.GetType().GetArrayElementType().GetName())
    type_name = f"[{array_size}]{element_type}"

    if len(elements) > 5:  # wrap line if too many elements
        return f"{type_name}{{\n{next_indent_str}" + f",\n{next_indent_str}".join(elements) + f"\n{indent_str}}}"
    else:
        return f"{type_name}{{{', '.join(elements)}}}"


def format_string(var: lldb.SBValue) -> str:
    summary = var.GetSummary()
    if summary is not None:
        return summary  # Keep the quotes
    else:
        data = _value_as_int(var.GetChildMemberWithName('data'))
        length = _value_as_int(var.GetChildMemberWithName('len'))
        if length == 0:
            return '""'
        if data is not None and length is not None:
            error = lldb.SBError()
            value = var.process.ReadCStringFromMemory(
                data, length + 1, error)
            if error.Success():
                return '"%s"' % value
    return "<variable not available>"


def format_struct(var: lldb.SBValue, debugger: lldb.SBDebugger, include_type: bool = True, indent: int = 0, type_name: str = "") -> str:
    children: List[str] = []
    indent_str = '  ' * indent
    next_indent_str = '  ' * (indent + 1)

    for i in range(var.GetNumChildren()):
        child = var.GetChildAtIndex(i)
        child_name = child.GetName()
        child_value = format_value(
            child, debugger, include_type=False, indent=indent+1)
        children.append(f"{child_name} = {child_value}")

    if len(children) > 5:  # 如果字段数量大于5，则进行折行显示
        struct_content = "{\n" + ",\n".join(
            [f"{next_indent_str}{child}" for child in children]) + f"\n{indent_str}}}"
    else:
        struct_content = f"{{{', '.join(children)}}}"

    if include_type:
        return f"{type_name}{struct_content}"
    else:
        return struct_content


def format_pointer(var: lldb.SBValue, _debugger: lldb.SBDebugger, _indent: int, _type_name: str) -> str:
    if not var.IsValid() or var.GetValueAsUnsigned() == 0:
        return "<variable not available>"
    return var.GetValue()  # Return the address as a string


def map_type_name(type_name: str) -> str:
    # Handle pointer types
    if type_name.endswith('*'):
        base_type = type_name[:-1].strip()
        mapped_base_type = map_type_name(base_type)
        return f"*{mapped_base_type}"

    # Map other types
    type_mapping: Dict[str, str] = {
        'long': 'int',
        'void': 'unsafe.Pointer',
        'char': 'byte',
        'short': 'int16',
        'int': 'int32',
        'long long': 'int64',
        'unsigned char': 'uint8',
        'unsigned short': 'uint16',
        'unsigned int': 'uint32',
        'unsigned long': 'uint',
        'unsigned long long': 'uint64',
        'float': 'float32',
        'double': 'float64',
    }

    for c_type, go_type in type_mapping.items():
        if type_name.startswith(c_type):
            return type_name.replace(c_type, go_type, 1)

    return type_name
