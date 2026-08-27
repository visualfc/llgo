//go:build windows

package goroot

import (
	"fmt"
	"os"
	"os/exec"
	"testing"
)

func writeTimeoutFakeTool(t *testing.T, path string) {
	t.Helper()
	buildWindowsFakeTool(t, path, windowsFakeToolSource("timeout", "", false))
}

func writeRunOutputFakeTool(t *testing.T, path, logPath string, allowRun bool) {
	t.Helper()
	buildWindowsFakeTool(t, path, windowsFakeToolSource("runoutput", logPath, allowRun))
}

func windowsFakeToolSource(mode, logPath string, allowRun bool) string {
	return fmt.Sprintf(`package main

import (
	"fmt"
	"io"
	"os"
	"strings"
	"time"
)

const toolMode = %q
const logPath = %q
const allowRun = %t

func main() {
	if len(os.Args) == 1 {
		if toolMode == "timeout" {
			time.Sleep(200 * time.Millisecond)
		} else {
			fmt.Println("ok")
		}
		return
	}
	if toolMode == "timeout" {
		if os.Args[1] != "build" {
			os.Exit(26)
		}
		out := outputPath(os.Args[2:])
		if out == "" {
			os.Exit(24)
		}
		if err := copyFile(os.Args[0], out); err != nil {
			panic(err)
		}
		return
	}
	logFile, err := os.OpenFile(logPath, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0o666)
	if err != nil {
		panic(err)
	}
	_, err = fmt.Fprintln(logFile, strings.Join(os.Args, " "))
	closeErr := logFile.Close()
	if err != nil {
		panic(err)
	}
	if closeErr != nil {
		panic(closeErr)
	}
	switch os.Args[1] {
	case "run":
		if !allowRun {
			fmt.Fprintln(os.Stderr, "unexpected runoutput generator invocation")
			os.Exit(23)
		}
		fmt.Print("package main\n\nfunc main() {\n\tprint(\"ok\\n\")\n}\n")
	case "build":
		out := outputPath(os.Args[2:])
		if out == "" {
			fmt.Fprintln(os.Stderr, "missing -o")
			os.Exit(24)
		}
		last := os.Args[len(os.Args)-1]
		info, err := os.Stat(last)
		if err != nil || info.Size() == 0 {
			fmt.Fprintf(os.Stderr, "empty generated source: %%s\n", last)
			os.Exit(25)
		}
		if err := copyFile(os.Args[0], out); err != nil {
			panic(err)
		}
	default:
		fmt.Fprintf(os.Stderr, "unexpected command: %%s\n", strings.Join(os.Args[1:], " "))
		os.Exit(26)
	}
}

func outputPath(args []string) string {
	for i := 0; i+1 < len(args); i++ {
		if args[i] == "-o" {
			return args[i+1]
		}
	}
	return ""
}

func copyFile(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err := os.OpenFile(dst, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0o755)
	if err != nil {
		return err
	}
	_, copyErr := io.Copy(out, in)
	closeErr := out.Close()
	if copyErr != nil {
		return copyErr
	}
	return closeErr
}
`, mode, logPath, allowRun)
}

func buildWindowsFakeTool(t *testing.T, path, source string) {
	t.Helper()
	sourcePath := path + ".go"
	if err := os.WriteFile(sourcePath, []byte(source), 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("go", "build", "-o", path, sourcePath)
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("build Windows fake tool: %v\n%s", err, out)
	}
}
