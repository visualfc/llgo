package testfail

import "testing"

func TestBoom(t *testing.T) {
	t.Errorf("boom: expected failure to show this file:line")
}

func TestOK(t *testing.T) {
	if 1+1 != 2 {
		t.Fatal("math broke")
	}
}
