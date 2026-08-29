//go:build go1.27

package sql_test

import (
	"database/sql"
	"database/sql/driver"
	"testing"
)

func TestConvertAssign(t *testing.T) {
	var got string
	if err := sql.ConvertAssign(driver.ScanContext{}, &got, []byte("llgo")); err != nil {
		t.Fatal(err)
	}
	if got != "llgo" {
		t.Fatalf("ConvertAssign result = %q, want llgo", got)
	}
}
