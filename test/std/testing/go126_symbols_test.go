//go:build go1.26

package testing_test

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"
)

func TestOutputAndArtifactDir(t *testing.T) {
	directory := t.ArtifactDir()
	if again := t.ArtifactDir(); again != directory {
		t.Fatalf("ArtifactDir changed from %q to %q", directory, again)
	}
	if err := os.WriteFile(filepath.Join(directory, "result.txt"), []byte("ok"), 0600); err != nil {
		t.Fatal(err)
	}
	t.Attr("go-version", "1.26")
	if _, err := fmt.Fprintln(t.Output(), "test output"); err != nil {
		t.Fatal(err)
	}
}

func TestBenchmarkOutput(t *testing.T) {
	result := testing.Benchmark(func(b *testing.B) {
		b.Attr("go-version", "1.26")
		fmt.Fprintln(b.Output(), "benchmark output")
		for range b.N {
		}
	})
	if result.N <= 0 {
		t.Fatalf("Benchmark ran %d iterations", result.N)
	}
}

func BenchmarkGo126ArtifactDir(b *testing.B) {
	directory := b.ArtifactDir()
	if again := b.ArtifactDir(); again != directory {
		b.Fatalf("ArtifactDir changed from %q to %q", directory, again)
	}
	if err := os.WriteFile(filepath.Join(directory, "benchmark.txt"), []byte("ok"), 0600); err != nil {
		b.Fatal(err)
	}
	b.Attr("go-version", "1.26")
	fmt.Fprintln(b.Output(), "benchmark output")
	for range b.N {
	}
}

func FuzzOutputAndArtifactDir(f *testing.F) {
	directory := f.ArtifactDir()
	if err := os.WriteFile(filepath.Join(directory, "fuzz.txt"), []byte("ok"), 0600); err != nil {
		f.Fatal(err)
	}
	f.Attr("go-version", "1.26")
	fmt.Fprintln(f.Output(), "fuzz output")
	f.Add("seed")
	f.Fuzz(func(t *testing.T, input string) {
		if input == "" {
			t.Skip()
		}
	})
}
