//go:build go1.26

package bytes_test

import (
	"bytes"
	"io"
	"testing"
)

func TestBufferPeek(t *testing.T) {
	buffer := bytes.NewBufferString("abcdef")
	got, err := buffer.Peek(3)
	if err != nil || string(got) != "abc" {
		t.Fatalf("Peek(3) = %q, %v; want %q, nil", got, err, "abc")
	}
	if buffer.Len() != 6 {
		t.Fatalf("Peek advanced the buffer: Len() = %d, want 6", buffer.Len())
	}

	got[0] = 'A'
	if next := buffer.Next(3); string(next) != "Abc" {
		t.Fatalf("Peek result does not alias the buffer: Next(3) = %q", next)
	}

	got, err = buffer.Peek(10)
	if err != io.EOF || string(got) != "def" {
		t.Fatalf("Peek(10) = %q, %v; want %q, io.EOF", got, err, "def")
	}
}
