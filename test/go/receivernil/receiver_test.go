package receivernil

import (
	"testing"

	"github.com/xgo-dev/llgo/test/go/receivernil/internal/receiverdep"
)

var trace string
var called chan struct{}

type leaf struct{ value int }

//go:noinline
func (*leaf) Invoke(int) {
	trace += "method;"
	if called != nil {
		called <- struct{}{}
	}
}

type outer struct {
	padding int
	leaf
}

type pointerOuter struct{ *leaf }
type nested struct{ *outer }

//go:noinline
func argument() int {
	trace += "arg;"
	return 1
}

//go:noinline
func invokeAndRecover(fn func()) (panicked bool) {
	defer func() { panicked = recover() != nil }()
	fn()
	return false
}

func TestPointerMethodReceiverEvaluation(t *testing.T) {
	var p *leaf
	var u *outer
	var nilPointerOuter *pointerOuter
	withNilLeaf := &pointerOuter{}
	withNilOuter := &nested{}
	var valueInvoker interface{ Invoke(int) } = u
	var pointerInvoker interface{ Invoke(int) } = nilPointerOuter
	var nilLeafInvoker interface{ Invoke(int) } = withNilLeaf
	tests := []struct {
		name  string
		call  func()
		trace string
		panic bool
	}{
		{"nil-pointer", func() { p.Invoke(argument()) }, "arg;method;", false},
		{"explicit-star", func() { (*p).Invoke(argument()) }, "arg;", true},
		{"promoted-value", func() { u.Invoke(argument()) }, "arg;", true},
		{"explicit-field", func() { u.leaf.Invoke(argument()) }, "arg;", true},
		{"promoted-pointer", func() { withNilLeaf.Invoke(argument()) }, "arg;method;", false},
		{"nil-promoted-pointer-base", func() { nilPointerOuter.Invoke(argument()) }, "optional-arg", true},
		{"nested-nil-value-base", func() { withNilOuter.Invoke(argument()) }, "arg;", true},
		{"method-expression", func() { (*leaf).Invoke(p, argument()) }, "arg;method;", false},
		{"promoted-value-expression", func() { (*outer).Invoke(u, argument()) }, "arg;", true},
		{"promoted-pointer-expression", func() { (*pointerOuter).Invoke(nilPointerOuter, argument()) }, "arg;", true},
		{"nil-embedded-pointer-expression", func() { (*pointerOuter).Invoke(withNilLeaf, argument()) }, "arg;method;", false},
		{"saved-promoted-value-expression", func() { f := (*outer).Invoke; f(u, argument()) }, "arg;", true},
		{"saved-promoted-pointer-expression", func() { f := (*pointerOuter).Invoke; f(nilPointerOuter, argument()) }, "arg;", true},
		{"promoted-value-interface", func() { valueInvoker.Invoke(argument()) }, "arg;", true},
		{"promoted-pointer-interface", func() { pointerInvoker.Invoke(argument()) }, "arg;", true},
		{"nil-embedded-pointer-interface", func() { nilLeafInvoker.Invoke(argument()) }, "arg;method;", false},
		{"saved-promoted-interface", func() { f := valueInvoker.Invoke; trace += "bound;"; f(argument()) }, "bound;arg;", true},
		{"explicit-address-expression", func() { (&*p).Invoke(argument()) }, "optional-arg", true},
		{"explicit-star-bound", func() { f := (*p).Invoke; trace += "bound;"; f(argument()) }, "", true},
		{"promoted-value-bound", func() { f := u.Invoke; trace += "bound;"; f(argument()) }, "", true},
		{"nil-pointer-bound", func() { f := p.Invoke; f(argument()) }, "arg;method;", false},
		{"promoted-pointer-bound", func() { f := withNilLeaf.Invoke; f(argument()) }, "arg;method;", false},
		{"explicit-star-defer", func() { defer (*p).Invoke(argument()); trace += "registered;" }, "arg;", true},
		{"promoted-value-defer", func() { defer u.Invoke(argument()); trace += "registered;" }, "arg;", true},
		{"nil-pointer-defer", func() { defer p.Invoke(argument()); trace += "registered;" }, "arg;registered;method;", false},
		{"promoted-expression-defer", func() { defer (*outer).Invoke(u, argument()); trace += "registered;" }, "arg;registered;", true},
		{"nil-field-store", func() { p.value = argument() }, "arg;", true},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			trace = ""
			panicked := invokeAndRecover(test.call)
			matches := trace == test.trace
			if test.trace == "optional-arg" {
				// Go does not order a pointer load relative to an argument's
				// function call. Either nil-check schedule is valid, but the
				// receiver load must not disappear just because M ignores it.
				matches = trace == "" || trace == "arg;"
			}
			if !matches || panicked != test.panic {
				t.Fatalf("trace=%q panic=%v, want trace=%q panic=%v", trace, panicked, test.trace, test.panic)
			}
		})
	}
}

func TestPointerMethodGoReceiverEvaluation(t *testing.T) {
	var p *leaf
	var u *outer
	called = make(chan struct{})
	defer func() { called = nil }()
	for _, test := range []struct {
		name  string
		start func()
		panic bool
	}{
		{"nil-pointer", func() { go p.Invoke(argument()); <-called }, false},
		{"explicit-star", func() { go (*p).Invoke(argument()); <-called }, true},
		{"promoted-value", func() { go u.Invoke(argument()); <-called }, true},
	} {
		t.Run(test.name, func(t *testing.T) {
			trace = ""
			panicked := invokeAndRecover(test.start)
			want := "arg;method;"
			if test.panic {
				want = "arg;"
			}
			if trace != want || panicked != test.panic {
				t.Fatalf("trace=%q panic=%v, want trace=%q panic=%v", trace, panicked, want, test.panic)
			}
		})
	}
}

//go:noinline
func receiver(p *leaf) *leaf {
	trace += "receiver;"
	return p
}

func TestPointerMethodReceiverSavedBeforeArguments(t *testing.T) {
	for _, initiallyNil := range []bool{true, false} {
		var p *leaf
		if !initiallyNil {
			p = &leaf{}
		}
		trace = ""
		panicked := invokeAndRecover(func() {
			(*receiver(p)).Invoke(func() int {
				trace += "arg;"
				if initiallyNil {
					p = &leaf{}
				} else {
					p = nil
				}
				return 1
			}())
		})
		want := "receiver;arg;method;"
		if initiallyNil {
			want = "receiver;arg;"
		}
		if trace != want || panicked != initiallyNil {
			t.Fatalf("initially nil=%v: trace=%q panic=%v, want trace=%q panic=%v", initiallyNil, trace, panicked, want, initiallyNil)
		}
	}
}

func TestImportedGenericPointerMethodReceiver(t *testing.T) {
	var nilLeaf *receiverdep.Leaf
	argument := func() func() {
		trace += "arg;"
		return func() { trace += "method;" }
	}
	for _, test := range []struct {
		name  string
		call  func()
		trace string
		panic bool
	}{
		{"nil-call", func() { receiverdep.Call(nilLeaf, 1, argument) }, "arg;", true},
		{"non-nil-call", func() { receiverdep.Call(&receiverdep.Leaf{}, 1, argument) }, "arg;method;", false},
		{"nil-safe-call", func() { receiverdep.NilSafe(nilLeaf, 1, argument) }, "arg;method;", false},
		{"nil-bound", func() { receiverdep.Bound(nilLeaf, 1)(argument()) }, "", true},
		{"non-nil-bound", func() { receiverdep.Bound(&receiverdep.Leaf{}, 1)(argument()) }, "arg;method;", false},
	} {
		t.Run(test.name, func(t *testing.T) {
			trace = ""
			panicked := invokeAndRecover(test.call)
			if trace != test.trace || panicked != test.panic {
				t.Fatalf("trace=%q panic=%v, want trace=%q panic=%v", trace, panicked, test.trace, test.panic)
			}
		})
	}
}
