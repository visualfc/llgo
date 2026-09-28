package local

import "unsafe"

const (
	LLGoFiles   = "_wrap/visit.c"
	LLGoPackage = "link"
)

type Node struct {
	Kind int32
	Data int32
}

// llgo:type C
type Visitor = func(Node, unsafe.Pointer) int32

//go:linkname VisitNode C.llgo_visit_node
func VisitNode(node Node, visitor Visitor, data unsafe.Pointer) int32

// llgo:link Node.Visit C.llgo_visit_node
func (n Node) Visit(visitor Visitor, data unsafe.Pointer) int32 { return -1 }
