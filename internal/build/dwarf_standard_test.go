//go:build !llgo

package build

import (
	"debug/dwarf"
	"debug/elf"
	"debug/macho"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/goplus/llgo/internal/optlevel"
	llvmenv "github.com/goplus/llgo/xtool/env/llvm"
)

const dwarfLanguageGo = 0x16

type dwarfNode struct {
	entry    *dwarf.Entry
	children []*dwarfNode
}

func TestStandardDWARF(t *testing.T) {
	if runtime.GOOS != "darwin" && runtime.GOOS != "linux" {
		t.Skip("native DWARF integration test requires Mach-O or ELF")
	}

	for _, level := range []optlevel.Level{
		optlevel.O0,
		optlevel.O1,
		optlevel.O2,
		optlevel.O3,
		optlevel.Os,
		optlevel.Oz,
	} {
		t.Run(level.String(), func(t *testing.T) {
			bin := filepath.Join(t.TempDir(), "dwarf-standard")
			conf := NewDefaultConf(ModeBuild)
			conf.OutFile = bin
			conf.OptLevel = level
			conf.LinkOptions.DWARF = DWARFPreserve
			if _, err := Do([]string{"./testdata/dwarf"}, conf); err != nil {
				t.Fatalf("build DWARF fixture at %s: %v", level, err)
			}
			if got := runBinary(t, bin); got != "dwarf-ok\n" {
				t.Fatalf("DWARF fixture output at %s = %q", level, got)
			}

			data, dwarfPath := openNativeDWARF(t, bin)
			verifyDWARF(t, dwarfPath)
			roots := readDWARFTree(t, data)
			cu := findDWARFNode(roots, dwarf.TagCompileUnit, "main")
			if cu == nil {
				t.Fatal("main compile unit not found")
			}
			if got, _ := cu.entry.Val(dwarf.AttrLanguage).(int64); got != dwarfLanguageGo {
				t.Fatalf("DW_AT_language = %#x, want DW_LANG_Go", got)
			}
			if got, _ := cu.entry.Val(dwarf.AttrProducer).(string); got != "LLGo" {
				t.Fatalf("DW_AT_producer = %q, want LLGo", got)
			}

			assertDWARFCoreTypes(t, data, cu)
			if level == optlevel.O0 {
				assertDWARFProgramStructure(t, data, cu)
			}
			assertDWARFLines(t, data, cu, "helper.go", markerLineInFile(t, "testdata/dwarf/helper.go", "DWARF_LINE_MARKER"))
		})
	}
}

func openNativeDWARF(t *testing.T, bin string) (*dwarf.Data, string) {
	t.Helper()
	switch runtime.GOOS {
	case "linux":
		file, err := elf.Open(bin)
		if err != nil {
			t.Fatal(err)
		}
		t.Cleanup(func() { _ = file.Close() })
		data, err := file.DWARF()
		if err != nil {
			t.Fatal(err)
		}
		return data, bin
	case "darwin":
		dsym := filepath.Join(t.TempDir(), filepath.Base(bin)+".dSYM")
		if out, err := exec.Command("dsymutil", bin, "-o", dsym).CombinedOutput(); err != nil {
			t.Fatalf("dsymutil: %v\n%s", err, out)
		}
		dwarfPath := filepath.Join(dsym, "Contents", "Resources", "DWARF", filepath.Base(bin))
		file, err := macho.Open(dwarfPath)
		if err != nil {
			t.Fatal(err)
		}
		t.Cleanup(func() { _ = file.Close() })
		data, err := file.DWARF()
		if err != nil {
			t.Fatal(err)
		}
		return data, dwarfPath
	default:
		panic("unreachable")
	}
}

func verifyDWARF(t *testing.T, path string) {
	t.Helper()
	binDir := llvmenv.New("").BinDir()
	verifyPath := path
	if runtime.GOOS == "linux" {
		// ELF section GC leaves BFD tombstones in DIEs for discarded functions.
		// Normalize a verifier-only copy; semantic assertions still read the
		// original ELF because dwarfutil intentionally drops global variables.
		verifyPath = filepath.Join(t.TempDir(), filepath.Base(path)+".dwarf")
		tool := filepath.Join(binDir, "llvm-dwarfutil")
		if out, err := exec.Command(tool, "--garbage-collection", path, verifyPath).CombinedOutput(); err != nil {
			t.Fatalf("llvm-dwarfutil: %v\n%s", err, out)
		}
	}
	tool := filepath.Join(binDir, "llvm-dwarfdump")
	if out, err := exec.Command(tool, "--verify", verifyPath).CombinedOutput(); err != nil {
		t.Fatalf("llvm-dwarfdump --verify: %v\n%s", err, out)
	}
}

func readDWARFTree(t *testing.T, data *dwarf.Data) []*dwarfNode {
	t.Helper()
	reader := data.Reader()
	var roots []*dwarfNode
	var stack []*dwarfNode
	for {
		entry, err := reader.Next()
		if err != nil {
			t.Fatal(err)
		}
		if entry == nil {
			break
		}
		if entry.Tag == 0 {
			if len(stack) == 0 {
				t.Fatal("unbalanced DWARF child terminator")
			}
			stack = stack[:len(stack)-1]
			continue
		}
		node := &dwarfNode{entry: entry}
		if len(stack) == 0 {
			roots = append(roots, node)
		} else {
			parent := stack[len(stack)-1]
			parent.children = append(parent.children, node)
		}
		if entry.Children {
			stack = append(stack, node)
		}
	}
	return roots
}

func findDWARFNode(nodes []*dwarfNode, tag dwarf.Tag, name string) *dwarfNode {
	for _, node := range nodes {
		if node.entry.Tag == tag {
			if got, _ := node.entry.Val(dwarf.AttrName).(string); got == name || strings.HasSuffix(got, "."+name) {
				return node
			}
		}
		if found := findDWARFNode(node.children, tag, name); found != nil {
			return found
		}
	}
	return nil
}

func findDWARFNodes(nodes []*dwarfNode, tag dwarf.Tag, name string) (found []*dwarfNode) {
	for _, node := range nodes {
		if node.entry.Tag == tag {
			if got, _ := node.entry.Val(dwarf.AttrName).(string); got == name {
				found = append(found, node)
			}
		}
		found = append(found, findDWARFNodes(node.children, tag, name)...)
	}
	return found
}

func dwarfTypeOf(t *testing.T, data *dwarf.Data, entry *dwarf.Entry) dwarf.Type {
	t.Helper()
	offset, ok := entry.Val(dwarf.AttrType).(dwarf.Offset)
	if !ok {
		t.Fatalf("%v has no DW_AT_type", entry)
	}
	typ, err := data.Type(offset)
	if err != nil {
		t.Fatalf("read type at %#x: %v", offset, err)
	}
	return typ
}

func unwrapDWARFTypedef(typ dwarf.Type) dwarf.Type {
	for {
		typedef, ok := typ.(*dwarf.TypedefType)
		if !ok {
			return typ
		}
		typ = typedef.Type
	}
}

func assertDWARFCoreTypes(t *testing.T, data *dwarf.Data, cu *dwarfNode) {
	t.Helper()
	global := findDWARFNode(cu.children, dwarf.TagVariable, "GlobalInt")
	if global == nil {
		t.Fatal("GlobalInt DIE not found")
	}
	if _, ok := dwarfTypeOf(t, data, global.entry).(*dwarf.TypedefType); !ok {
		t.Fatalf("GlobalInt type = %T, want source NamedInt without a storage pointer", dwarfTypeOf(t, data, global.entry))
	}
	namedInt := findDWARFNode(cu.children, dwarf.TagTypedef, "NamedInt")
	if namedInt == nil {
		t.Fatal("NamedInt typedef DIE not found")
	}
	if got, _ := namedInt.entry.Val(dwarf.AttrDeclLine).(int64); got != int64(markerLineInFile(t, "testdata/dwarf/main.go", "type NamedInt")) {
		t.Errorf("NamedInt declaration line = %d, want its type declaration", got)
	}

	sampleNode := findDWARFNode(cu.children, dwarf.TagTypedef, "Sample")
	if sampleNode == nil {
		t.Fatal("Sample typedef DIE not found")
	}
	sample, ok := unwrapDWARFTypedef(dwarfTypeOf(t, data, sampleNode.entry)).(*dwarf.StructType)
	if !ok {
		t.Fatalf("Sample underlying type = %T, want struct", unwrapDWARFTypedef(dwarfTypeOf(t, data, sampleNode.entry)))
	}
	fields := make(map[string]dwarf.Type, len(sample.Field))
	for _, field := range sample.Field {
		fields[field.Name] = unwrapDWARFTypedef(field.Type)
	}
	checks := map[string]any{
		"Bool":     (*dwarf.BoolType)(nil),
		"I64":      (*dwarf.IntType)(nil),
		"U64":      (*dwarf.UintType)(nil),
		"F64":      (*dwarf.FloatType)(nil),
		"C64":      (*dwarf.ComplexType)(nil),
		"C128":     (*dwarf.ComplexType)(nil),
		"Text":     (*dwarf.StructType)(nil),
		"Values":   (*dwarf.StructType)(nil),
		"Fixed":    (*dwarf.ArrayType)(nil),
		"Lookup":   (*dwarf.PtrType)(nil),
		"Queue":    (*dwarf.PtrType)(nil),
		"Callback": (*dwarf.StructType)(nil),
		"Any":      (*dwarf.StructType)(nil),
		"Iface":    (*dwarf.StructType)(nil),
		"Pointer":  (*dwarf.PtrType)(nil),
		"Unsafe":   (*dwarf.PtrType)(nil),
		"Pair":     (*dwarf.StructType)(nil),
	}
	for name, want := range checks {
		got := fields[name]
		if got == nil || fmt.Sprintf("%T", got) != fmt.Sprintf("%T", want) {
			t.Errorf("Sample.%s type = %T, want %T", name, got, want)
		}
	}
	if got := fields["C64"].Size(); got != 8 {
		t.Errorf("complex64 size = %d, want 8", got)
	}
	if got := fields["C128"].Size(); got != 16 {
		t.Errorf("complex128 size = %d, want 16", got)
	}
	array := fields["Fixed"].(*dwarf.ArrayType)
	if array.Count != 3 || array.Type.Size() != 2 {
		t.Errorf("Fixed = count %d, element size %d; want 3 and 2", array.Count, array.Type.Size())
	}
	for _, name := range []string{"Lookup", "Queue", "Pointer", "Unsafe"} {
		if got := fields[name].Size(); got != int64(dataAddressSize()) {
			t.Errorf("Sample.%s size = %d, want pointer size %d", name, got, dataAddressSize())
		}
	}
	callback := fields["Callback"].(*dwarf.StructType)
	var codeType dwarf.Type
	for _, field := range callback.Field {
		if field.Name == "$f" {
			codeType = unwrapDWARFTypedef(field.Type)
			break
		}
	}
	codePointer, ok := codeType.(*dwarf.PtrType)
	if !ok {
		t.Fatalf("function value code field = %T, want pointer", codeType)
	}
	if _, ok := codePointer.Type.(*dwarf.FuncType); !ok {
		t.Errorf("function value code pointee = %T, want subroutine type", codePointer.Type)
	}

	for _, check := range []struct {
		name string
		want any
	}{
		{"GlobalMap", (*dwarf.PtrType)(nil)},
		{"GlobalChan", (*dwarf.PtrType)(nil)},
		{"GlobalFunc", (*dwarf.StructType)(nil)},
		{"GlobalInterface", (*dwarf.StructType)(nil)},
		{"GlobalUnsafe", (*dwarf.PtrType)(nil)},
	} {
		node := findDWARFNode(cu.children, dwarf.TagVariable, check.name)
		if node == nil {
			t.Errorf("%s DIE not found", check.name)
			continue
		}
		got := unwrapDWARFTypedef(dwarfTypeOf(t, data, node.entry))
		if fmt.Sprintf("%T", got) != fmt.Sprintf("%T", check.want) {
			t.Errorf("%s type = %T, want %T", check.name, got, check.want)
		}
	}

	recursiveNode := findDWARFNode(cu.children, dwarf.TagTypedef, "Recursive")
	if recursiveNode == nil {
		t.Fatal("Recursive typedef DIE not found")
	}
	recursive, ok := unwrapDWARFTypedef(dwarfTypeOf(t, data, recursiveNode.entry)).(*dwarf.StructType)
	if !ok || len(recursive.Field) != 2 {
		t.Fatalf("Recursive type = %#v, want two-field struct", recursive)
	}
	if _, ok := unwrapDWARFTypedef(recursive.Field[1].Type).(*dwarf.PtrType); !ok {
		t.Fatalf("Recursive.Next = %T, want pointer", recursive.Field[1].Type)
	}
}

func assertDWARFProgramStructure(t *testing.T, data *dwarf.Data, cu *dwarfNode) {
	t.Helper()
	inspect := findDWARFNode(cu.children, dwarf.TagSubprogram, "inspect")
	if inspect == nil {
		t.Fatal("inspect subprogram DIE not found")
	}
	if inspect.entry.Val(dwarf.AttrLowpc) == nil || inspect.entry.Val(dwarf.AttrHighpc) == nil {
		t.Fatal("inspect has no code range")
	}
	for _, name := range []string{"input", "pair", "values"} {
		param := findDWARFNode(inspect.children, dwarf.TagFormalParameter, name)
		if param == nil || param.entry.Val(dwarf.AttrLocation) == nil {
			t.Errorf("parameter %s has no located DIE", name)
		}
	}
	for _, name := range []string{"result", "local", "index", "value", "shadow", "closure", "boxed", "typed"} {
		variables := findDWARFNodes(inspect.children, dwarf.TagVariable, name)
		if len(variables) == 0 {
			t.Errorf("local %s DIE not found", name)
			continue
		}
		if variables[0].entry.Val(dwarf.AttrLocation) == nil {
			t.Errorf("local %s has no location", name)
		}
	}
	if countDWARFTag(inspect.children, dwarf.TagLexDwarfBlock) == 0 {
		t.Error("inspect has no lexical block DIE")
	}
	if findDWARFNodeMatching(cu.children, dwarf.TagSubprogram, func(name string) bool {
		return strings.Contains(name, ".identity[") && strings.HasSuffix(name, ".NamedInt]")
	}) == nil {
		t.Error("instantiated generic identity subprogram DIE not found")
	}
	if findDWARFNode(cu.children, dwarf.TagSubprogram, "main.(*Sample).Number") == nil &&
		findDWARFNode(cu.children, dwarf.TagSubprogram, "Number") == nil {
		t.Error("method subprogram DIE not found")
	}
	_ = data
}

func findDWARFNodeMatching(nodes []*dwarfNode, tag dwarf.Tag, match func(string) bool) *dwarfNode {
	for _, node := range nodes {
		if node.entry.Tag == tag {
			if name, _ := node.entry.Val(dwarf.AttrName).(string); match(name) {
				return node
			}
		}
		if found := findDWARFNodeMatching(node.children, tag, match); found != nil {
			return found
		}
	}
	return nil
}

func countDWARFTag(nodes []*dwarfNode, tag dwarf.Tag) int {
	count := 0
	for _, node := range nodes {
		if node.entry.Tag == tag {
			count++
		}
		count += countDWARFTag(node.children, tag)
	}
	return count
}

func assertDWARFLines(t *testing.T, data *dwarf.Data, cu *dwarfNode, fileSuffix string, line int) {
	t.Helper()
	reader, err := data.LineReader(cu.entry)
	if err != nil {
		t.Fatal(err)
	}
	var entry dwarf.LineEntry
	for {
		err := reader.Next(&entry)
		if err != nil {
			if err != io.EOF {
				t.Fatal(err)
			}
			break
		}
		if entry.File != nil && strings.HasSuffix(entry.File.Name, fileSuffix) && entry.Line == line && entry.Address != 0 {
			return
		}
	}
	t.Errorf("line table has no address for %s:%d", fileSuffix, line)
}

func markerLineInFile(t *testing.T, path, marker string) int {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	for i, line := range strings.Split(string(data), "\n") {
		if strings.Contains(line, marker) {
			return i + 1
		}
	}
	t.Fatalf("marker %q not found in %s", marker, path)
	return 0
}

func dataAddressSize() int {
	if ^uintptr(0)>>32 == 0 {
		return 4
	}
	return 8
}
