//go:build !windows

package goroot

import (
	"fmt"
	"os"
	"testing"
)

func writeTimeoutFakeTool(t *testing.T, path string) {
	t.Helper()
	script := `#!/bin/sh
set -eu
out=""
prev=""
for arg in "$@"; do
	if [ "$prev" = "-o" ]; then
		out="$arg"
	fi
	prev="$arg"
done
cat > "$out" <<'EOF'
#!/bin/sh
sleep 0.2
EOF
chmod +x "$out"
`
	if err := os.WriteFile(path, []byte(script), 0o755); err != nil {
		t.Fatal(err)
	}
}

func writeRunOutputFakeTool(t *testing.T, path, logPath string, allowRun bool) {
	t.Helper()
	allowRunValue := "false"
	if allowRun {
		allowRunValue = "true"
	}
	script := fmt.Sprintf(`#!/bin/sh
set -eu
printf '%%s\n' "$0 $*" >> %[1]q
case "$1" in
run)
	if [ %[2]q != "true" ]; then
		echo "unexpected runoutput generator invocation" >&2
		exit 23
	fi
	cat <<'EOF'
package main

func main() {
	print("ok\n")
}
EOF
	;;
build)
	out=""
	last=""
	prev=""
	for arg in "$@"; do
		if [ "$prev" = "-o" ]; then
			out="$arg"
		fi
		last="$arg"
		prev="$arg"
	done
	if [ -z "$out" ]; then
		echo "missing -o" >&2
		exit 24
	fi
	if [ ! -s "$last" ]; then
		echo "empty generated source: $last" >&2
		exit 25
	fi
	cat > "$out" <<'EOF'
#!/bin/sh
printf 'ok\n'
EOF
	chmod +x "$out"
	;;
*)
	echo "unexpected command: $*" >&2
	exit 26
	;;
esac
`, logPath, allowRunValue)
	if err := os.WriteFile(path, []byte(script), 0o755); err != nil {
		t.Fatal(err)
	}
}
