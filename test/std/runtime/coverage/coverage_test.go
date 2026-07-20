package coverage_test

import (
	"bytes"
	"os"
	"runtime/coverage"
	"strings"
	"testing"
)

func TestRuntimeCoverageWritersAgree(t *testing.T) {
	var meta bytes.Buffer
	metaErr := coverage.WriteMeta(&meta)
	metaDir := t.TempDir()
	metaDirErr := coverage.WriteMetaDir(metaDir)
	checkWriteResults(t, "metadata", "covmeta.", meta.Len(), metaErr, metaDir, metaDirErr)

	var counters bytes.Buffer
	counterErr := coverage.WriteCounters(&counters)
	counterDir := t.TempDir()
	counterDirErr := coverage.WriteCountersDir(counterDir)
	checkWriteResults(t, "counters", "covcounters.", counters.Len(), counterErr, counterDir, counterDirErr)

	clearErr := coverage.ClearCounters()
	if counterErr != nil && clearErr == nil {
		t.Fatalf("ClearCounters succeeded although WriteCounters is unavailable: %v", counterErr)
	}
}

func checkWriteResults(t *testing.T, name, prefix string, directBytes int, directErr error, dir string, dirErr error) {
	t.Helper()
	if (directErr == nil) != (dirErr == nil) {
		t.Fatalf("%s writer availability differs: direct=%v, directory=%v", name, directErr, dirErr)
	}
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatal(err)
	}
	if directErr != nil {
		if directBytes != 0 || len(entries) != 0 {
			t.Fatalf("unavailable %s writers produced %d bytes and %d files", name, directBytes, len(entries))
		}
		return
	}
	if directBytes == 0 {
		t.Fatalf("Write%s succeeded without writing data", name)
	}
	for _, entry := range entries {
		if strings.HasPrefix(entry.Name(), prefix) {
			info, err := entry.Info()
			if err != nil {
				t.Fatal(err)
			}
			if info.Size() == 0 {
				t.Fatalf("%s file %q is empty", name, entry.Name())
			}
			return
		}
	}
	t.Fatalf("Write%sDir produced no %q file: %v", name, prefix, entries)
}
