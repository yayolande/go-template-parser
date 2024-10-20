package main

import (
	"fmt"
)

type FacePal interface {
	getKind() Kind
	setKind(val Kind)
	getRange() []int
	setRange([]int)
}

type Kind int

type BaseNode struct {
	Kind	Kind
	Range	[]int
}

func (b *BaseNode) setRange(val []int) {
	b.Range = val
}

type ExtendedNode struct {
	Kind	int
	Range	[]int
}

func main() {
	fmt.Println("main for struct_sample.go")
	node := BaseNode{Range: []int{2, 3},}
	fmt.Println(node)
}

