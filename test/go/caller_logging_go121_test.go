//go:build go1.21

package gotest

import (
	"log"
	"log/slog"
	"runtime"
	"strconv"
	"strings"
	"testing"
)

type callerLogSink struct {
	strings.Builder
}

func TestCallerLogging(t *testing.T) {
	previousWriter := log.Writer()
	previousFlags := log.Flags()
	previousPrefix := log.Prefix()
	previousDefault := slog.Default()
	t.Cleanup(func() {
		log.SetOutput(previousWriter)
		log.SetFlags(previousFlags)
		log.SetPrefix(previousPrefix)
		slog.SetDefault(previousDefault)
	})

	var sink callerLogSink
	log.SetOutput(&sink)
	log.SetFlags(log.Lshortfile)
	log.SetPrefix("")
	_, _, line, _ := runtime.Caller(0)
	log.Println("package logger")
	checkCallerLogLine(t, sink.String(), line+1)

	sink.Reset()
	logger := log.New(&sink, "", log.Lshortfile)
	_, _, line, _ = runtime.Caller(0)
	logger.Printf("new logger")
	checkCallerLogLine(t, sink.String(), line+1)

	sink.Reset()
	textLogger := slog.New(slog.NewTextHandler(&sink, &slog.HandlerOptions{AddSource: true}))
	_, _, line, _ = runtime.Caller(0)
	textLogger.Info("text")
	if want := "caller_logging_go121_test.go:" + strconv.Itoa(line+1) + " "; !strings.Contains(sink.String(), want) {
		t.Fatalf("slog text source does not contain %q: %s", want, sink.String())
	}

	sink.Reset()
	slog.SetDefault(slog.New(slog.NewJSONHandler(&sink, &slog.HandlerOptions{AddSource: true})))
	_, _, line, _ = runtime.Caller(0)
	slog.Warn("json")
	if want := "caller_logging_go121_test.go\",\"line\":" + strconv.Itoa(line+1); !strings.Contains(sink.String(), want) {
		t.Fatalf("slog JSON source does not contain %q: %s", want, sink.String())
	}
}

func checkCallerLogLine(t *testing.T, output string, line int) {
	t.Helper()
	want := "caller_logging_go121_test.go:" + strconv.Itoa(line) + ":"
	if !strings.HasPrefix(output, want) {
		t.Fatalf("log prefix = %q, want prefix %q", output, want)
	}
}
