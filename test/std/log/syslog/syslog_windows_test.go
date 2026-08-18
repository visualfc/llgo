//go:build windows

package syslog_test

import (
	_ "log/syslog"
	"testing"
)

// The official Go package is intentionally documentation-only on Windows: it
// remains importable, but exposes no API. Keep it in the package coverage set
// without pretending that Unix syslog operations exist on this platform.
func TestDocumentationOnlyPackage(t *testing.T) {
	_ = t
}
