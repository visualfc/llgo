package gotest

import "testing"

type rangeArrayPointerHolder struct {
	data *[3]int
}

//go:noinline
func nilRangeArrayPointer() *[3]int {
	return nil
}

func TestRangeOverNilArrayPointerUsesLength(t *testing.T) {
	var p *[3]int

	sum := 0
	for i := range *p {
		sum += i
	}
	if sum != 3 {
		t.Fatalf("range over nil *array sum = %d, want 3", sum)
	}
}

func TestRangeOverNilArrayPointerFieldUsesLength(t *testing.T) {
	holder := &rangeArrayPointerHolder{}

	sum := 0
	for i := range *holder.data {
		sum += i
	}
	if sum != 3 {
		t.Fatalf("range over nil *array field sum = %d, want 3", sum)
	}
}

func TestRangeOverNilArrayPointerCallIsEvaluated(t *testing.T) {
	calls, sum, panicValue := rangeOverNilArrayPointerCall()
	if calls != 1 {
		t.Fatalf("range expression calls = %d, want 1", calls)
	}
	if panicValue == nil {
		t.Fatalf("range over nil *array call did not panic; sum = %d", sum)
	}
}

func rangeOverNilArrayPointerCall() (calls, sum int, panicValue any) {
	defer func() {
		panicValue = recover()
	}()
	next := func() *[3]int {
		calls++
		return nil
	}
	for i := range *next() {
		sum += i
	}
	return
}

func TestLenOfNilArrayPointerValueUsesStaticLength(t *testing.T) {
	var p *[3]int
	if got := len(*p); got != 3 {
		t.Fatalf("len(*nil array pointer) = %d, want 3", got)
	}
	if got := len(p); got != 3 {
		t.Fatalf("len(nil array pointer) = %d, want 3", got)
	}
	if got := len(nilRangeArrayPointer()); got != 3 {
		t.Fatalf("len(nil array pointer call) = %d, want 3", got)
	}
	p = nilRangeArrayPointer()
	if got := len(*p); got != 3 {
		t.Fatalf("len(*assigned nil array pointer) = %d, want 3", got)
	}
	got, panicValue := lenOfNilArrayPointerCall()
	if panicValue == nil {
		t.Fatalf("len(*nil array pointer call) did not panic; got %d", got)
	}

	got, panicValue = capOfNilArrayPointerCall()
	if panicValue == nil {
		t.Fatalf("cap(*nil array pointer call) did not panic; got %d", got)
	}

	got, panicValue = lenOfNilArrayPointerReceive()
	if panicValue == nil {
		t.Fatalf("len(*nil array pointer receive) did not panic; got %d", got)
	}

	got, panicValue = lenOfNilArrayPointerFieldCall()
	if panicValue == nil {
		t.Fatalf("len(*nil array pointer field call) did not panic; got %d", got)
	}
}

func lenOfNilArrayPointerCall() (length int, panicValue any) {
	defer func() {
		panicValue = recover()
	}()
	length = len(*nilRangeArrayPointer())
	return
}

func capOfNilArrayPointerCall() (capacity int, panicValue any) {
	defer func() {
		panicValue = recover()
	}()
	capacity = cap(*nilRangeArrayPointer())
	return
}

func lenOfNilArrayPointerReceive() (length int, panicValue any) {
	values := make(chan *[3]int, 1)
	values <- nil
	defer func() {
		panicValue = recover()
	}()
	length = len(*<-values)
	return
}

func nilRangeArrayPointerHolder() rangeArrayPointerHolder {
	return rangeArrayPointerHolder{}
}

func lenOfNilArrayPointerFieldCall() (length int, panicValue any) {
	defer func() {
		panicValue = recover()
	}()
	length = len(*nilRangeArrayPointerHolder().data)
	return
}
