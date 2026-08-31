package main

import _ "unsafe"

func main() {
	beforeAlias()
	after()
	crossFile()
	mainAlias()
}

func before() {
	println("before")
}

//go:linkname beforeAlias main.before
func beforeAlias()

func after()

//go:linkname after main.afterImpl

//go:linkname crossFile main.crossFileImpl

func mainAliasTarget() {
	println("main-alias")
}

//go:linkname mainAlias main.mainAliasTarget
func mainAlias()
