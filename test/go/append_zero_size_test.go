package gotest

import "testing"

type appendZeroSize struct{}

func TestAppendZeroSizedElementsUpdatesSliceHeader(t *testing.T) {
	got := append([]appendZeroSize{}, make([]appendZeroSize, 2)...)
	if len(got) != 2 {
		t.Fatalf("len(append(empty, make(2)...)) = %d, want 2", len(got))
	}
	if cap(got) < len(got) {
		t.Fatalf("cap after zero-sized append = %d, want at least len %d", cap(got), len(got))
	}

	got = append(got, appendZeroSize{})
	if len(got) != 3 {
		t.Fatalf("len after single zero-sized append = %d, want 3", len(got))
	}
	if cap(got) < len(got) {
		t.Fatalf("cap after single zero-sized append = %d, want at least len %d", cap(got), len(got))
	}
}

func TestAppendZeroSizedElementsWithinCapacity(t *testing.T) {
	base := make([]appendZeroSize, 1, 4)
	got := append(base, make([]appendZeroSize, 2)...)
	if len(got) != 3 {
		t.Fatalf("len(append(len=1 cap=4, make(2)...)) = %d, want 3", len(got))
	}
	if cap(got) != 4 {
		t.Fatalf("cap after append within capacity = %d, want 4", cap(got))
	}
}
