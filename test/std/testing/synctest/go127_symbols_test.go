//go:build go1.27

package synctest_test

import (
	"testing"
	"testing/synctest"
)

func TestSleep(t *testing.T) {
	returned := false
	synctest.Test(t, func(t *testing.T) {
		synctest.Sleep(0)
		returned = true
	})
	if !returned {
		t.Fatal("Sleep did not return")
	}
}
