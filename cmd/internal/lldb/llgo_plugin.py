# pylint: disable=missing-module-docstring,missing-class-docstring,missing-function-docstring

from dataclasses import dataclass
from typing import List, Optional, Dict, Any, Tuple
import re
import lldb


LLGO_DEBUGGER_MARKER_PREFIX = "__llgo_debugger_marker_v"
LLGO_DEBUGGER_SCHEMAS = {
    "__llgo_debugger_marker_v1": (1, 1),
}
LLGO_TYPE_CATEGORY = "LLGo"
LLGO_MAX_STRING_SUMMARY_BYTES = 256
LLGO_DEFAULT_MAX_CHILDREN = 256
_TARGET_INFO_CACHE: Dict[Tuple[Any, ...], "LLGoTargetInfo"] = {}


@dataclass(frozen=True)
class LLGoRuntimeLayout:
    string_type: str
    string_data: str
    string_len: str
    slice_type_pattern: str
    slice_data: str
    slice_len: str
    slice_cap: str


@dataclass(frozen=True)
class LLGoSliceValue:
    address: int
    length: int
    capacity: int
    element_type: lldb.SBType
    element_size: int


LLGO_RUNTIME_LAYOUTS = {
    1: LLGoRuntimeLayout(
        string_type="string",
        string_data="data",
        string_len="len",
        slice_type_pattern=r"^\[\].+",
        slice_data="data",
        slice_len="len",
        slice_cap="cap",
    ),
}


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
    register_type_formatters(debugger)


def _type_options(hide_children: bool = False) -> int:
    options = lldb.eTypeOptionCascade
    options |= getattr(lldb, "eTypeOptionSkipPointers", 0)
    options |= getattr(lldb, "eTypeOptionSkipReferences", 0)
    if hide_children:
        options |= getattr(lldb, "eTypeOptionHideChildren", 0)
    return options


def register_type_formatters(debugger: lldb.SBDebugger) -> None:
    category = debugger.CreateCategory(LLGO_TYPE_CATEGORY)
    for layout in LLGO_RUNTIME_LAYOUTS.values():
        category.AddTypeSummary(
            lldb.SBTypeNameSpecifier(layout.string_type, False),
            lldb.SBTypeSummary.CreateWithFunctionName(
                "llgo_plugin.string_summary", _type_options(True)),
        )
        slice_specifier = lldb.SBTypeNameSpecifier(
            layout.slice_type_pattern, True)
        category.AddTypeSummary(
            slice_specifier,
            lldb.SBTypeSummary.CreateWithFunctionName(
                "llgo_plugin.slice_summary", _type_options()),
        )
        category.AddTypeSynthetic(
            slice_specifier,
            lldb.SBTypeSynthetic.CreateWithClassName(
                "llgo_plugin.SliceSyntheticProvider", _type_options()),
        )
    category.SetEnabled(True)


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


def _raw_value(value: lldb.SBValue) -> lldb.SBValue:
    if not value or not value.IsValid():
        return value
    raw = value.GetNonSyntheticValue()
    return raw if raw and raw.IsValid() else value


def _canonical_type_name(value: lldb.SBValue) -> str:
    value_type = _raw_value(value).GetType()
    while value_type and value_type.IsValid() and value_type.IsTypedefType():
        value_type = value_type.GetTypedefedType()
    return value_type.GetName() if value_type and value_type.IsValid() else ""


def _runtime_layout(value: lldb.SBValue) -> Optional[LLGoRuntimeLayout]:
    if not value or not value.IsValid():
        return None
    info = inspect_target(value.GetTarget())
    if not info.supported or info.runtime_layout_version is None:
        return None
    return LLGO_RUNTIME_LAYOUTS.get(info.runtime_layout_version)


def _string_fields(value: lldb.SBValue, layout: LLGoRuntimeLayout) -> Optional[Tuple[lldb.SBValue, int]]:
    raw = _raw_value(value)
    if _canonical_type_name(raw) != layout.string_type:
        return None
    data = raw.GetChildMemberWithName(layout.string_data)
    length = _value_as_int(raw.GetChildMemberWithName(layout.string_len))
    if not data or not data.IsValid() or length is None or length < 0:
        return None
    return data, length


def _slice_fields(value: lldb.SBValue,
                  layout: LLGoRuntimeLayout) -> Optional[LLGoSliceValue]:
    raw = _raw_value(value)
    if not re.fullmatch(layout.slice_type_pattern,
                        _canonical_type_name(raw)):
        return None
    data = raw.GetChildMemberWithName(layout.slice_data)
    length = _value_as_int(raw.GetChildMemberWithName(layout.slice_len))
    capacity = _value_as_int(raw.GetChildMemberWithName(layout.slice_cap))
    if (not data or not data.IsValid() or length is None or
            capacity is None or length < 0 or capacity < length):
        return None
    address = _value_as_int(data)
    element_type = data.GetType().GetPointeeType()
    if address is None:
        return None
    if not element_type or not element_type.IsValid():
        return None
    return LLGoSliceValue(
        address=address,
        length=length,
        capacity=capacity,
        element_type=element_type,
        element_size=element_type.GetByteSize(),
    )


def _slice_element(value: lldb.SBValue, index: int,
                   fields: LLGoSliceValue) -> Optional[lldb.SBValue]:
    if index < 0 or index >= fields.length or fields.address == 0:
        return None
    element_address = fields.address + index * fields.element_size
    target = value.GetTarget()
    return target.CreateValueFromAddress(
        f"[{index}]", lldb.SBAddress(element_address, target),
        fields.element_type)


def _quote_go_bytes(value: bytes) -> str:
    escapes = {
        "\a": r"\a",
        "\b": r"\b",
        "\f": r"\f",
        "\n": r"\n",
        "\r": r"\r",
        "\t": r"\t",
        "\v": r"\v",
        '"': r'\"',
        "\\": r"\\",
    }
    quoted: List[str] = ['"']
    for char in value.decode("utf-8", errors="surrogateescape"):
        escaped = escapes.get(char)
        if escaped is not None:
            quoted.append(escaped)
            continue
        code = ord(char)
        if 0xDC80 <= code <= 0xDCFF:
            quoted.append(f"\\x{code - 0xDC00:02x}")
        elif char.isprintable():
            quoted.append(char)
        elif code <= 0xFF:
            quoted.append(f"\\x{code:02x}")
        elif code <= 0xFFFF:
            quoted.append(f"\\u{code:04x}")
        else:
            quoted.append(f"\\U{code:08x}")
    quoted.append('"')
    return "".join(quoted)


def _utf8_bounded_prefix(value: bytes, limit: int) -> bytes:
    prefix = value[:limit]
    if len(value) <= limit or limit == 0:
        return prefix

    start = limit - 1
    lower_bound = max(0, limit - 4)
    while start >= lower_bound and value[start] & 0xC0 == 0x80:
        start -= 1
    if start < lower_bound:
        return prefix
    lead = value[start]
    if 0xC2 <= lead <= 0xDF:
        sequence_length = 2
    elif 0xE0 <= lead <= 0xEF:
        sequence_length = 3
    elif 0xF0 <= lead <= 0xF4:
        sequence_length = 4
    else:
        return prefix
    sequence_end = start + sequence_length
    if sequence_end <= limit or sequence_end > len(value):
        return prefix
    try:
        value[start:sequence_end].decode("utf-8")
    except UnicodeDecodeError:
        return prefix
    return value[:start]


def _format_runtime_string(value: lldb.SBValue,
                           layout: LLGoRuntimeLayout) -> Optional[str]:
    fields = _string_fields(value, layout)
    if fields is None:
        return None
    data, length = fields
    if length == 0:
        return '""'
    address = _value_as_int(data)
    process = value.GetProcess()
    if (address is None or address == 0 or not process or
            not process.IsValid()):
        return None
    display_length = min(length, LLGO_MAX_STRING_SUMMARY_BYTES)
    read_length = min(length, LLGO_MAX_STRING_SUMMARY_BYTES + 3)
    error = lldb.SBError()
    contents = process.ReadMemory(address, read_length, error)
    if not error.Success() or contents is None:
        return None
    if isinstance(contents, str):
        contents = contents.encode("latin-1", errors="surrogateescape")
    else:
        contents = bytes(contents)
    contents = _utf8_bounded_prefix(contents, display_length)
    summary = _quote_go_bytes(contents)
    return summary if display_length == length else summary + "..."


def string_summary(value: lldb.SBValue, _internal_dict: Dict[str, Any]) -> Optional[str]:
    layout = _runtime_layout(value)
    return _format_runtime_string(value, layout) if layout else None


def slice_summary(value: lldb.SBValue, _internal_dict: Dict[str, Any]) -> Optional[str]:
    layout = _runtime_layout(value)
    fields = _slice_fields(value, layout) if layout else None
    if fields is None:
        return None
    return f"len={fields.length} cap={fields.capacity}"


class SliceSyntheticProvider:
    def __init__(self, value: lldb.SBValue, _internal_dict: Dict[str, Any]) -> None:
        self.value = value
        self.raw = _raw_value(value)
        self.layout = _runtime_layout(self.raw)
        self.fields: Optional[LLGoSliceValue] = None
        self.update()

    def update(self) -> bool:
        self.raw = _raw_value(self.value)
        self.fields = (_slice_fields(self.raw, self.layout)
                       if self.layout else None)
        return False

    def num_children(self, max_children: Optional[int] = None) -> int:
        if self.fields is None:
            count = self.raw.GetNumChildren()
        else:
            count = self.fields.length
        if max_children is not None and max_children >= 0:
            count = min(count, max_children)
        return count

    def get_child_at_index(self, index: int) -> Optional[lldb.SBValue]:
        if self.fields is None:
            return self.raw.GetChildAtIndex(index)
        return _slice_element(self.raw, index, self.fields)

    def get_child_index(self, name: str) -> int:
        if self.fields is None:
            for index in range(self.raw.GetNumChildren()):
                if self.raw.GetChildAtIndex(index).GetName() == name:
                    return index
            return -1
        match = re.fullmatch(r"\[([0-9]+)\]", name or "")
        if match is None:
            return -1
        index = int(match.group(1))
        return index if index < self.num_children() else -1

    def has_children(self) -> bool:
        return self.num_children() != 0


def get_indexed_value(value: lldb.SBValue, index: int) -> Optional[lldb.SBValue]:
    if not value or not value.IsValid():
        return None

    if value.GetType().IsArrayType():
        return value.GetChildAtIndex(index)
    layout = _runtime_layout(value)
    fields = _slice_fields(value, layout) if layout else None
    return _slice_element(value, index, fields) if fields else None


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
    layout = _runtime_layout(var)
    fields = _slice_fields(var, layout) if layout else None
    if fields is None:
        return "<variable not available>"
    elements: List[str] = []

    indent_str = '  ' * indent
    next_indent_str = '  ' * (indent + 1)

    values = lldb.SBDebugger.GetInternalVariableValue(
        "target.max-children-count", debugger.GetInstanceName())
    max_children = LLGO_DEFAULT_MAX_CHILDREN
    if values.GetSize() != 0:
        try:
            max_children = max(0, int(values.GetStringAtIndex(0), 0))
        except (TypeError, ValueError):
            pass
    displayed = min(fields.length, max_children)
    for i in range(displayed):
        element = _slice_element(var, i, fields)
        if element is None or not element.IsValid():
            return "<variable not available>"
        value = format_value(
            element, debugger, include_type=False, indent=indent+1)
        elements.append(value)
    if displayed < fields.length:
        elements.append(f"... ({fields.length - displayed} more)")

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
    layout = _runtime_layout(var)
    value = _format_runtime_string(var, layout) if layout else None
    return value if value is not None else "<variable not available>"


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
