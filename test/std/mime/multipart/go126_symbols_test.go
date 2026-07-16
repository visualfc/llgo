//go:build go1.26

package multipart_test

import (
	"mime/multipart"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	_ = multipart.FileContentDisposition
}
