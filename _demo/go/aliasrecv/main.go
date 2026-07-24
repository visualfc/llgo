package main

func main() {
	var v any = &threadImpl{100}
	println(v.(interface{ String() string }).String())
}

type threadImpl struct {
	id int64
}

type Thread = *threadImpl

func (t *threadImpl) ID() int64 {
	return t.id
}

func (t Thread) String() string {
	return "thread"
}
