//go:build go1.26

package slog_test

import (
	"bytes"
	"context"
	"log/slog"
	"runtime"
	"strings"
	"testing"
	"time"
)

func TestMultiHandlerAndRecordSource(t *testing.T) {
	var first, second bytes.Buffer
	multi := slog.NewMultiHandler(
		slog.NewTextHandler(&first, nil),
		slog.NewJSONHandler(&second, nil),
	)
	ctx := context.Background()
	if !multi.Enabled(ctx, slog.LevelInfo) {
		t.Fatal("MultiHandler unexpectedly disabled info records")
	}

	direct := slog.NewRecord(time.Unix(1, 0), slog.LevelInfo, "direct", 0)
	if err := multi.Handle(ctx, direct); err != nil {
		t.Fatal(err)
	}
	withAttrs := multi.WithAttrs([]slog.Attr{slog.String("component", "compiler")})
	if err := withAttrs.Handle(ctx, slog.NewRecord(time.Unix(1, 0), slog.LevelInfo, "compiled", 0)); err != nil {
		t.Fatal(err)
	}
	withGroup := multi.WithGroup("details")
	record := slog.NewRecord(time.Unix(1, 0), slog.LevelInfo, "grouped", 0)
	record.Add("files", 2)
	if err := withGroup.Handle(ctx, record); err != nil {
		t.Fatal(err)
	}
	for name, output := range map[string]string{"text": first.String(), "json": second.String()} {
		if !strings.Contains(output, "direct") || !strings.Contains(output, "compiled") || !strings.Contains(output, "compiler") || !strings.Contains(output, "details") || !strings.Contains(output, "files") {
			t.Fatalf("%s handler output is incomplete: %q", name, output)
		}
	}

	pcs := make([]uintptr, 1)
	runtime.Callers(1, pcs)
	sourceRecord := slog.NewRecord(time.Time{}, slog.LevelInfo, "source", pcs[0])
	source := sourceRecord.Source()
	if source == nil || source.Function == "" || source.Line == 0 {
		t.Fatalf("Record.Source = %#v", source)
	}
}
