package main

// This case groups small compiler/runtime regressions that need end-to-end
// execution but do not justify five separately compiled command packages.

type resultBox struct{ data any }

func (r *resultBox) read() any {
	data := r.data
	r.data = nil
	return data
}

type stringer interface{ String() string }
type text struct{ s string }

func (t *text) String() string    { return t.s }
func stringify(t stringer) string { return t.String() }

var closureMap = map[string]func(stringer) string{"demo": stringify}
var closureList = []func(stringer) string{stringify}

// Preserve the captured-result-before-field-reassignment input from #1608.
type resultState struct{ data []int }

func returnOne() []int {
	t := resultState{data: []int{1, 2}}
	result := t.data
	t.data = []int{1, 2, 3}
	return result
}

func returnTwo() ([]int, bool) {
	t := resultState{data: []int{1, 2}}
	result := t.data
	t.data = []int{1, 2, 3}
	return result, true
}

type stateFn func(*machine) stateFn
type machine struct {
	value int
	max   int
	state stateFn
}

func startState(*machine) stateFn {
	return countState
}

func countState(m *machine) stateFn {
	m.value++
	if m.value >= m.max {
		return endState
	}
	return countState
}

func endState(*machine) stateFn {
	return nil
}

func deferRecover() (captured, closed int) {
	value := 5
	defer func(v int) { captured = v }(value)
	defer func() {
		closed = value
		if recover() != "expected" {
			panic("recover")
		}
	}()
	value = 10
	panic("expected")
}

func main() {
	box := resultBox{data: 1}
	if box.read() != 1 || box.data != nil {
		panic("interface result order")
	}
	t := &text{"hello"}
	if closureMap["demo"](t) != closureList[0](t) {
		panic("function value container")
	}
	if got := returnOne(); len(got) != 2 || got[0] != 1 || got[1] != 2 {
		panic("single return capture")
	}
	if got, ok := returnTwo(); !ok || len(got) != 2 || got[0] != 1 || got[1] != 2 {
		panic("multi return capture")
	}
	m := &machine{max: 5, state: startState}
	for m.state != nil {
		m.state = m.state(m)
	}
	if m.value != 5 {
		panic("recursive named function")
	}
	if captured, closed := deferRecover(); captured != 5 || closed != 10 {
		panic("defer capture")
	}
	println("core regressions ok")
}
