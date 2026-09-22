//go:build llgo

package dependency

func CoveredByLLGo() int {
	return 42
}

func NotCoveredByLLGo() int {
	return 0
}
