package demotest

import (
	"errors"
	"os"
	"path/filepath"
	"testing"
)

func TestCheckResultStdoutNormalizesOnlyCRLF(t *testing.T) {
	root := t.TempDir()
	path := filepath.Join(root, "want.txt")
	if err := os.WriteFile(path, []byte("first\nsecond\n"), 0o666); err != nil {
		t.Fatal(err)
	}
	if err := CheckResult(root, Check{Kind: "stdout", Golden: "want.txt"}, []byte("first\r\nsecond\r\n"), nil, nil); err != nil {
		t.Fatal(err)
	}
	if err := CheckResult(root, Check{Kind: "stdout", Golden: "want.txt"}, []byte("first\nsecond"), nil, nil); err == nil {
		t.Fatal("stdout check unexpectedly ignored a missing final newline")
	}
}

func TestCheckResultFailureMatchesFragmentsInOrder(t *testing.T) {
	check := Check{Kind: "failure", StderrContains: []string{"alpha", "omega"}}
	if err := CheckResult("", check, nil, []byte("alpha\nmiddle\nomega\n"), errors.New("exit 1")); err != nil {
		t.Fatal(err)
	}
	if err := CheckResult("", check, nil, []byte("omega then alpha"), errors.New("exit 1")); err == nil {
		t.Fatal("failure check unexpectedly accepted reversed fragments")
	}
	if err := CheckResult("", check, nil, nil, nil); err == nil {
		t.Fatal("failure check unexpectedly accepted successful exit")
	}
}
