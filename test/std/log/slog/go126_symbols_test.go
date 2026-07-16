//go:build go1.26

package slog_test

import (
	"log/slog"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var handler slog.MultiHandler
	var record slog.Record
	_ = handler.Enabled
	_ = handler.Handle
	_ = handler.WithAttrs
	_ = handler.WithGroup
	_ = record.Source
}
