// LITTEST
// Scope: common
package main

import (
	"unsafe"

	"github.com/xgo-dev/llgo/cl/_testlibc/callbackmethod/local"
)

func visit(node local.Node, data unsafe.Pointer) int32 {
	*(*int32)(data) += node.Kind
	return node.Data * 2
}

func chooseVisitor(useFirst bool) func(local.Node, unsafe.Pointer) int32 {
	if useFirst {
		return visit
	}
	return func(node local.Node, data unsafe.Pointer) int32 {
		return visit(node, data)
	}
}

func rejectCaptured(node local.Node, data unsafe.Pointer, offset int32) {
	defer func() {
		err, ok := recover().(error)
		if !ok || err.Error() != "runtime error: C callback must not capture variables" {
			panic("capturing C callback was not rejected")
		}
	}()
	node.Visit(func(node local.Node, _ unsafe.Pointer) int32 {
		return node.Data + offset
	}, data)
}

// CHECK-LABEL: define void @main.main(){{.*}} {
// CHECK: call i32 @llgo_visit_node(
func main() {
	var seen int32
	node := local.Node{Kind: 21, Data: 21}
	data := unsafe.Pointer(&seen)
	var visitor local.VisitorAlias = visit
	for _, got := range []int32{
		local.VisitNode(node, visitor, data),
		node.Visit(visitor, data),
		local.Node.Visit(node, visitor, data),
		node.Visit(chooseVisitor(node.Kind != 0), data),
	} {
		if got != 43 {
			panic("C method callback returned incorrect values")
		}
	}
	bound := node.Visit
	if got := bound(visitor, data); got != 43 || seen != 105 {
		panic("bound C method callback returned incorrect values")
	}
	rejectCaptured(node, data, 7)
	if seen != 105 {
		panic("capturing callback reached C")
	}
	println("callback method ok")
}
