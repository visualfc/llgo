package demotest

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

func CheckResult(root string, check Check, stdout, stderr []byte, runErr error) error {
	switch check.Kind {
	case "self", "exit":
		if runErr != nil {
			return fmt.Errorf("expected successful exit: %w", runErr)
		}
		return nil
	case "stdout":
		if runErr != nil {
			return fmt.Errorf("expected successful exit: %w", runErr)
		}
		want, err := os.ReadFile(filepath.Join(root, filepath.FromSlash(check.Golden)))
		if err != nil {
			return err
		}
		if !bytes.Equal(normalizeNewlines(stdout), normalizeNewlines(want)) {
			return fmt.Errorf("stdout mismatch\nwant:\n%s\ngot:\n%s", want, stdout)
		}
		return nil
	case "failure":
		if runErr == nil {
			return errors.New("expected command failure")
		}
		remaining := string(normalizeNewlines(stderr))
		for _, fragment := range check.StderrContains {
			index := strings.Index(remaining, fragment)
			if index < 0 {
				return fmt.Errorf("stderr does not contain %q in order", fragment)
			}
			remaining = remaining[index+len(fragment):]
		}
		return nil
	default:
		return fmt.Errorf("unsupported check kind %q", check.Kind)
	}
}

func normalizeNewlines(value []byte) []byte {
	return bytes.ReplaceAll(value, []byte("\r\n"), []byte("\n"))
}
