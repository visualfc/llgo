//go:build go1.27

package driver_test

import (
	"database/sql/driver"
	"io"
	"testing"
)

type columnScanner struct{}

func (*columnScanner) Columns() []string                    { return []string{"value"} }
func (*columnScanner) Close() error                         { return nil }
func (*columnScanner) Next([]driver.Value) error            { return io.EOF }
func (*columnScanner) NextRow() error                       { return io.EOF }
func (*columnScanner) ScanColumn(driver.ScanContext, int, any) error { return nil }

func TestRowsColumnScanner(t *testing.T) {
	var rows driver.RowsColumnScanner = new(columnScanner)
	if got := rows.Columns(); len(got) != 1 || got[0] != "value" {
		t.Fatalf("Columns = %v", got)
	}
	if err := rows.ScanColumn(driver.ScanContext{}, 0, new(string)); err != nil {
		t.Fatal(err)
	}
}
