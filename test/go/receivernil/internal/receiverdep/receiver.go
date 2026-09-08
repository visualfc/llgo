package receiverdep

type Leaf struct{ Value int }

//go:noinline
func (*Leaf) Invoke(callback func()) { callback() }

// These instantiated bodies are emitted in an importing package's module,
// but their nil-check source positions still belong to this package.
//
//go:noinline
func Call[A any](p *Leaf, _ A, argument func() func()) {
	(*p).Invoke(argument())
}

//go:noinline
func Bound[A any](p *Leaf, _ A) func(func()) { return (*p).Invoke }

//go:noinline
func NilSafe[A any](p *Leaf, _ A, argument func() func()) {
	p.Invoke(argument())
}
