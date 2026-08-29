//go:build go1.27

package http_test

import (
	"net/http"
	"testing"
)

func TestDefaultMaxHeaderValueCount(t *testing.T) {
	if http.DefaultMaxHeaderValueCount != 500 {
		t.Fatalf("DefaultMaxHeaderValueCount = %d, want 500", http.DefaultMaxHeaderValueCount)
	}
}
