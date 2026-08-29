// LITTEST
package main

import "reflect"

// SYMBOL-DAG: main{{.*}}Full{{.*}}M
// SYMBOL-NOT: main{{.*}}Full{{.*}}N
// SYMBOL-NOT: main{{.*}}Partial{{.*}}M
// SYMBOL-NOT: __typeid_go.method.i.

type Wide interface {
	M() int
	N() int
}

type Full struct{}

//go:noinline
func (Full) M() int { return 7 }

//go:noinline
func (Full) N() int { return 9 }

type Partial struct{}

//go:noinline
func (Partial) M() int { return 11 }

//go:noinline
func callWide(v any) int {
	return v.(Wide).M()
}

//go:noinline
func keepPartialDescriptor() bool {
	return reflect.TypeOf(Partial{}).Name() == "Partial"
}

type matcher interface {
	verify(string, func(string, string) (bool, error)) error
}

type simpleMatch struct{}

//go:noinline
func (simpleMatch) verify(name string, matchString func(string, string) (bool, error)) error {
	matched, err := matchString(name, "want")
	if err != nil || !matched {
		return err
	}
	return nil
}

//go:noinline
func callMatcher(v any) bool {
	err := v.(matcher).verify("want", func(name, pattern string) (bool, error) {
		return name == pattern, nil
	})
	return err == nil
}

func main() {
	println(callWide(Full{}), keepPartialDescriptor(), callMatcher(simpleMatch{}))
}
