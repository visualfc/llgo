package pclnmap

import (
	"bytes"
	"encoding/binary"
	"testing"

	"github.com/goplus/llgo/internal/build/funcinfo"
)

func sampleData(t *testing.T) Data {
	t.Helper()
	table, err := funcinfo.EncodeWithPCLines(
		[]funcinfo.Record{{Symbol: "main.main", Name: "main.main", File: "/src/main.go", Line: 3}},
		[]funcinfo.PCLineRecord{{ID: 7, Symbol: "main.main", File: "/src/main.go", Line: 4}},
	)
	if err != nil {
		t.Fatal(err)
	}
	var identity [32]byte
	copy(identity[:], "one exact linked executable")
	return Data{
		GOOS:        "linux",
		GOARCH:      "amd64",
		PointerSize: 8,
		ImageBase:   0x400000,
		TextStart:   0x401000,
		TextEnd:     0x403000,
		Identity:    identity,
		Table:       table,
		SymbolIndex: []SymbolIndexEntry{{SymbolID: 11, FuncIndex: 1}},
		EntrySites:  []Site{{PCOffset: 0x1010, ID: 11}},
		StubSites:   []Site{{PCOffset: 0x1008, ID: 11}},
		PCSites:     []Site{{PCOffset: 0x1020, ID: 7}},
	}
}

func TestEncodeParse(t *testing.T) {
	raw, err := Encode(sampleData(t))
	if err != nil {
		t.Fatal(err)
	}
	view, err := Parse(raw)
	if err != nil {
		t.Fatal(err)
	}
	if view.GOOSCode != GOOSLinux || view.GOARCHCode != GOARCHAMD64 || view.PointerSize != 8 {
		t.Fatalf("target = (%d, %d, %d)", view.GOOSCode, view.GOARCHCode, view.PointerSize)
	}
	if view.TextStartOffset != 0x1000 || view.TextEndOffset != 0x3000 {
		t.Fatalf("text offsets = [%#x,%#x)", view.TextStartOffset, view.TextEndOffset)
	}
	if got := view.Sections[descRecords].Count; got != 1 {
		t.Fatalf("record count = %d", got)
	}
	if got := view.Sections[descPCSites].Count; got != 1 {
		t.Fatalf("pcsite count = %d", got)
	}
	if got := view.Sections[descStubSites].Count; got != 1 {
		t.Fatalf("stub-site count = %d", got)
	}
	stub := raw[view.Sections[descStubSites].Offset:]
	if pc, id := binary.LittleEndian.Uint64(stub), binary.LittleEndian.Uint64(stub[8:]); pc != 0x1008 || id != 11 {
		t.Fatalf("stub site = {%#x, %d}, want {%#x, %d}", pc, id, uint64(0x1008), 11)
	}
	raw2, err := Encode(sampleData(t))
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(raw, raw2) {
		t.Fatal("encoding is not deterministic")
	}
}

func TestParseRejectsCorruptionAndTruncation(t *testing.T) {
	raw, err := Encode(sampleData(t))
	if err != nil {
		t.Fatal(err)
	}
	corrupt := append([]byte(nil), raw...)
	corrupt[len(corrupt)-1] ^= 0xff
	if _, err := Parse(corrupt); err == nil {
		t.Fatal("Parse accepted corrupt payload")
	}
	if _, err := Parse(raw[:len(raw)-1]); err == nil {
		t.Fatal("Parse accepted truncated payload")
	}
	wrongABI := append([]byte(nil), raw...)
	wrongABI[headerABIVersion]++
	if _, err := Parse(wrongABI); err == nil {
		t.Fatal("Parse accepted an incompatible runtime ABI")
	}
}

func TestParseRejectsOverlappingSections(t *testing.T) {
	raw, err := Encode(sampleData(t))
	if err != nil {
		t.Fatal(err)
	}
	recordOffset := binary.LittleEndian.Uint64(raw[headerDescriptors:])
	stringDescriptor := headerDescriptors + descStrings*16
	binary.LittleEndian.PutUint64(raw[stringDescriptor:], recordOffset)
	if _, err := Parse(raw); err == nil {
		t.Fatal("Parse accepted overlapping record and string sections")
	}
}

func TestParseRejectsMisalignedSections(t *testing.T) {
	raw, err := Encode(sampleData(t))
	if err != nil {
		t.Fatal(err)
	}
	pclineDescriptor := headerDescriptors + descPCLines*16
	offset := binary.LittleEndian.Uint64(raw[pclineDescriptor:])
	binary.LittleEndian.PutUint64(raw[pclineDescriptor:], offset+1)
	if _, err := Parse(raw); err == nil {
		t.Fatal("Parse accepted a misaligned pcline section")
	}
}

func TestParseRejectsInvalidStubSection(t *testing.T) {
	raw, err := Encode(sampleData(t))
	if err != nil {
		t.Fatal(err)
	}
	stubDescriptor := headerDescriptors + descStubSites*16
	offset := binary.LittleEndian.Uint64(raw[stubDescriptor:])
	binary.LittleEndian.PutUint64(raw[stubDescriptor:], offset+1)
	if _, err := Parse(raw); err == nil {
		t.Fatal("Parse accepted a misaligned stub-site section")
	}

	raw, err = Encode(sampleData(t))
	if err != nil {
		t.Fatal(err)
	}
	binary.LittleEndian.PutUint64(raw[stubDescriptor:], uint64(len(raw)))
	binary.LittleEndian.PutUint64(raw[stubDescriptor+8:], 1)
	if _, err := Parse(raw); err == nil {
		t.Fatal("Parse accepted an out-of-bounds stub-site section")
	}
}

func TestEncodeRejectsUnsupportedTarget(t *testing.T) {
	data := sampleData(t)
	data.GOOS = "windows"
	if _, err := Encode(data); err == nil {
		t.Fatal("Encode accepted unsupported GOOS")
	}
	data = sampleData(t)
	data.PointerSize = 4
	if _, err := Encode(data); err == nil {
		t.Fatal("Encode accepted 32-bit pointer size")
	}
}
