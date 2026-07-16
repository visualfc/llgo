/*
 * Copyright (c) 2026 The XGo Authors (xgo.dev). All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package pclnpost

import (
	"bytes"
	"crypto/sha256"
	"encoding/binary"
	"os"
	"reflect"
	"sort"
	"testing"
)

func externalELFFixture(t *testing.T, identitySize int) string {
	t.Helper()
	entry := func(addrOf func(string) uint64) []byte {
		idA := fnv64("example.com/p.A")
		out := rec(addrOf("example.com/p.A")+4, idA)
		// LTO-style copy of A's body record in B: AnalyzeExternal must drop it.
		out = append(out, rec(addrOf("example.com/p.B")+8, idA)...)
		return append(out, rec(addrOf("example.com/p.B")+4, fnv64("example.com/p.B"))...)
	}
	const imageBase = uint64(0x10000)
	pcLine := rec(imageBase+8, 101)
	pcLine = append(pcLine, rec(imageBase+8, 101)...) // exact duplicate
	pcLine = append(pcLine, rec(imageBase+72, 202)...)
	pcLine = append(pcLine, rec(0xdead0000, 303)...) // outside text
	path := buildELFExternal(t, fixtureFns(), entry, fixtureStub, 256, 64,
		pcLine, make([]byte, identitySize))
	addELFLoadSegments(t, path, imageBase)
	return path
}

// addELFLoadSegments installs a fileless low segment plus a file-backed load
// segment into the fixture's otherwise unused text bytes. This verifies that
// image-base discovery ignores PT_LOAD segments that carry no file content.
func addELFLoadSegments(t *testing.T, path string, imageBase uint64) {
	t.Helper()
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	const phoff = uint64(64)
	const phsize = uint16(56)
	putLoad := func(dst []byte, vaddr, filesz uint64) {
		binary.LittleEndian.PutUint32(dst[0:], 1) // PT_LOAD
		binary.LittleEndian.PutUint32(dst[4:], 5) // PF_R|PF_X
		binary.LittleEndian.PutUint64(dst[16:], vaddr)
		binary.LittleEndian.PutUint64(dst[24:], vaddr)
		binary.LittleEndian.PutUint64(dst[32:], filesz)
		binary.LittleEndian.PutUint64(dst[40:], filesz)
		binary.LittleEndian.PutUint64(dst[48:], 0x1000)
	}
	putLoad(raw[phoff:phoff+uint64(phsize)], 0, 0)
	putLoad(raw[phoff+uint64(phsize):phoff+2*uint64(phsize)], imageBase, uint64(len(raw)))
	binary.LittleEndian.PutUint64(raw[32:], phoff)
	binary.LittleEndian.PutUint16(raw[54:], phsize)
	binary.LittleEndian.PutUint16(raw[56:], 2)
	if err := os.WriteFile(path, raw, 0755); err != nil {
		t.Fatal(err)
	}
}

func TestAnalyzeExternalELF(t *testing.T) {
	path := externalELFFixture(t, sha256.Size)
	before, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	analysis, err := AnalyzeExternal(path)
	if err != nil {
		t.Fatal(err)
	}
	if analysis.Format != ExternalFormatELF || analysis.PointerSize != 8 {
		t.Fatalf("target = %s/%d", analysis.Format, analysis.PointerSize)
	}
	if analysis.ImageBase != 0x10000 || analysis.TextStart != 0x10000 || analysis.TextEnd != 0x10090 {
		t.Fatalf("layout base=%#x text=[%#x,%#x)", analysis.ImageBase, analysis.TextStart, analysis.TextEnd)
	}
	if analysis.Identity != sha256.Sum256(before) {
		t.Fatal("identity is not the pre-mutation binary SHA-256")
	}
	if analysis.EntryRecords != 3 || analysis.StubRecords != 1 || analysis.InlineCopies != 1 || analysis.NoSymbol != 0 {
		t.Fatalf("analysis stats: %+v", analysis)
	}
	wantEntries := []ExternalSite{
		{PCOffset: 0, ID: fnv64("example.com/p.A")},
		{PCOffset: 64, ID: fnv64("example.com/p.B")},
		{PCOffset: 128, ID: fnv64("example.com/p.A")},
	}
	if !reflect.DeepEqual(analysis.EntrySites, wantEntries) {
		t.Fatalf("entry sites = %#v, want %#v", analysis.EntrySites, wantEntries)
	}
	wantPCLines := []ExternalSite{{PCOffset: 8, ID: 101}, {PCOffset: 72, ID: 202}}
	if !reflect.DeepEqual(analysis.PCLineSites, wantPCLines) {
		t.Fatalf("pcline sites = %#v, want %#v", analysis.PCLineSites, wantPCLines)
	}
	after, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(before, after) {
		t.Fatal("AnalyzeExternal modified its input")
	}
	again, err := AnalyzeExternal(path)
	if err != nil || !reflect.DeepEqual(analysis, again) {
		t.Fatalf("analysis is not deterministic: err=%v\nfirst=%+v\nsecond=%+v", err, analysis, again)
	}
}

func TestDetachExternalELF(t *testing.T) {
	path := externalELFFixture(t, sha256.Size)
	analysis, err := AnalyzeExternal(path)
	if err != nil {
		t.Fatal(err)
	}
	if err := DetachExternal(path, analysis.Identity); err != nil {
		t.Fatal(err)
	}
	info, err := load(path)
	if err != nil {
		t.Fatal(err)
	}
	for name, section := range map[string][]byte{
		"entry":  info.entrySec,
		"stub":   info.stubSec,
		"pcline": info.pcLineSec,
	} {
		if !bytes.Equal(section, make([]byte, len(section))) {
			t.Fatalf("%s section was not cleared", name)
		}
	}
	gotIdentity := info.raw[info.identityFileOff : info.identityFileOff+info.identityVMSize]
	if !bytes.Equal(gotIdentity, analysis.Identity[:]) {
		t.Fatalf("identity section = %x, want %x", gotIdentity, analysis.Identity)
	}
	if err := DetachExternal(path, analysis.Identity); err == nil {
		t.Fatal("stale pre-mutation identity unexpectedly accepted")
	}
}

func TestDetachExternalRejectsMismatchWithoutMutation(t *testing.T) {
	path := externalELFFixture(t, sha256.Size)
	before, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	wrong := sha256.Sum256([]byte("not this executable"))
	if err := DetachExternal(path, wrong); err == nil {
		t.Fatal("expected identity mismatch")
	}
	after, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(before, after) {
		t.Fatal("identity mismatch modified the executable")
	}
}

func TestDetachExternalRequiresExactIdentitySection(t *testing.T) {
	path := externalELFFixture(t, sha256.Size-1)
	analysis, err := AnalyzeExternal(path)
	if err != nil {
		t.Fatal(err)
	}
	before, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if err := DetachExternal(path, analysis.Identity); err == nil {
		t.Fatal("expected identity-section size error")
	}
	after, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(before, after) {
		t.Fatal("identity-section error modified the executable")
	}
}

func TestAnalyzeAndDetachExternalMachO(t *testing.T) {
	const imageBase = uint64(0x100000000)
	fns := []elfFn{
		{name: "example.com/p.A", size: 0x10},
		{name: "example.com/p.B", size: 0x40},
		{name: "__llgo_stub.example.com/p.A", size: 0x80},
	}
	addr := func(off uint64) uint64 { return imageBase + 0x1000 + off }
	idA, idB := fnv64("example.com/p.A"), fnv64("example.com/p.B")
	entry := rec(addr(0x10)+4, idA)
	entry = append(entry, rec(addr(0x40)+8, idA)...) // inline A copy in B
	entry = append(entry, rec(addr(0x40)+4, idB)...)
	stub := rec(addr(0x80)+4, idA)
	pcLine := rec(addr(0x10)+8, 11)
	pcLine = append(pcLine, rec(addr(0x40)+8, 22)...)
	path := buildMachOExternal(t, entry, stub, pcLine, make([]byte, sha256.Size), fns)
	pageStartOff := chainMachOSitePointers(t, path)

	analysis, err := AnalyzeExternal(path)
	if err != nil {
		t.Fatal(err)
	}
	if analysis.Format != ExternalFormatMachO || analysis.ImageBase != imageBase {
		t.Fatalf("format/base = %s/%#x", analysis.Format, analysis.ImageBase)
	}
	wantEntries := []ExternalSite{
		{PCOffset: 0x1010, ID: idA},
		{PCOffset: 0x1040, ID: idB},
		{PCOffset: 0x1080, ID: idA},
	}
	if !reflect.DeepEqual(analysis.EntrySites, wantEntries) || analysis.InlineCopies != 1 {
		t.Fatalf("entry analysis = %#v inline=%d", analysis.EntrySites, analysis.InlineCopies)
	}
	if err := DetachExternal(path, analysis.Identity); err != nil {
		t.Fatal(err)
	}
	info, err := load(path)
	if err != nil {
		t.Fatal(err)
	}
	for name, section := range map[string][]byte{
		"entry":  info.entrySec,
		"stub":   info.stubSec,
		"pcline": info.pcLineSec,
	} {
		if !bytes.Equal(section, make([]byte, len(section))) {
			t.Fatalf("Mach-O %s section was not cleared", name)
		}
	}
	gotIdentity := info.raw[info.identityFileOff : info.identityFileOff+info.identityVMSize]
	if !bytes.Equal(gotIdentity, analysis.Identity[:]) {
		t.Fatalf("Mach-O identity = %x, want %x", gotIdentity, analysis.Identity)
	}
	if got := binary.LittleEndian.Uint16(info.raw[pageStartOff:]); got != chainedPtrStartNone {
		t.Fatalf("Mach-O site fixups remain chained: page_start=%#x", got)
	}
}

// chainMachOSitePointers makes the fixture resemble lld output: every live
// site pointer is a DYLD_CHAINED_PTR_64 rebase node in one __DATA page. It
// returns the page_start field so the detach test can verify that the now
// empty chain was removed rather than merely zeroed in place.
func chainMachOSitePointers(t *testing.T, path string) uint64 {
	t.Helper()
	info, err := load(path)
	if err != nil {
		t.Fatal(err)
	}
	type slot struct {
		off, target uint64
	}
	var slots []slot
	for _, section := range []struct {
		off  uint64
		data []byte
	}{
		{info.entryFileOff, info.entrySec},
		{info.stubFileOff, info.stubSec},
		{info.pcLineFileOff, info.pcLineSec},
	} {
		for off := 0; off+16 <= len(section.data); off += 16 {
			if id := binary.LittleEndian.Uint64(section.data[off+8:]); id != 0 {
				slots = append(slots, slot{
					off:    section.off + uint64(off),
					target: binary.LittleEndian.Uint64(section.data[off:]),
				})
			}
		}
	}
	sort.Slice(slots, func(i, j int) bool { return slots[i].off < slots[j].off })
	if len(slots) == 0 {
		t.Fatal("fixture has no Mach-O site pointers")
	}
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	for i, current := range slots {
		var next uint64
		if i+1 < len(slots) {
			delta := slots[i+1].off - current.off
			if delta%4 != 0 || delta/4 > 0xfff {
				t.Fatalf("fixture chain delta %d is not encodable", delta)
			}
			next = delta / 4
		}
		binary.LittleEndian.PutUint64(raw[current.off:],
			(current.target&(1<<36-1))|(next<<51))
	}

	// Locate LC_DYLD_CHAINED_FIXUPS, then the __DATA (segment index 2)
	// starts_in_segment record installed by buildMachOExternal.
	ncmds := binary.LittleEndian.Uint32(raw[16:])
	cmdOff := uint64(32)
	var fixOff uint64
	for i := uint32(0); i < ncmds; i++ {
		cmd := binary.LittleEndian.Uint32(raw[cmdOff:])
		cmdSize := uint64(binary.LittleEndian.Uint32(raw[cmdOff+4:]))
		if cmd == lcDyldChainedFixups {
			fixOff = uint64(binary.LittleEndian.Uint32(raw[cmdOff+8:]))
			break
		}
		cmdOff += cmdSize
	}
	if fixOff == 0 {
		t.Fatal("fixture has no chained-fixups command")
	}
	startsOff := fixOff + uint64(binary.LittleEndian.Uint32(raw[fixOff+4:]))
	dataStarts := uint64(binary.LittleEndian.Uint32(raw[startsOff+4+2*4:]))
	pageStartOff := startsOff + dataStarts + 22
	pageStart := slots[0].off - info.entryFileOff
	if pageStart > 0x7fff {
		t.Fatalf("fixture page start %#x is too large", pageStart)
	}
	binary.LittleEndian.PutUint16(raw[pageStartOff:], uint16(pageStart))
	if err := os.WriteFile(path, raw, 0755); err != nil {
		t.Fatal(err)
	}
	return pageStartOff
}
