//go:build go1.26

package sync_test

import (
	"sync"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var group sync.WaitGroup
	_ = group.Go
}
