package gotest

import "testing"

func TestMapUpdateRHSNilDerefOrder(t *testing.T) {
	tests := []struct {
		name string
		run  func(map[int]int)
	}{
		{
			name: "assign",
			run: func(m map[int]int) {
				var p *int
				m[0] = *p
			},
		},
		{
			name: "add assign",
			run: func(m map[int]int) {
				var p *int
				m[0] += *p
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			m := map[int]int{}
			expectPanic(t, func() {
				tt.run(m)
			})
			if len(m) != 0 {
				t.Fatalf("map insert happened before RHS panic: len=%d", len(m))
			}
		})
	}
}

func TestMultipleAssignmentMapUpdateBeforeNilStore(t *testing.T) {
	m := map[int]int{}
	var p *int

	expectPanic(t, func() {
		m[2], *p = 42, 2
	})
	if len(m) != 1 {
		t.Fatalf("map length after panic = %d, want 1", len(m))
	}
	if got := m[2]; got != 42 {
		t.Fatalf("m[2] after panic = %d, want 42", got)
	}
}

func expectPanic(t *testing.T, f func()) {
	t.Helper()
	defer func() {
		if recover() == nil {
			t.Fatal("expected panic")
		}
	}()
	f()
}
