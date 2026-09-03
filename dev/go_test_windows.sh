#!/usr/bin/env bash

set -euo pipefail

if [[ $# -eq 0 ]]; then
	echo "usage: $0 <go-test-command> [argument ...]" >&2
	exit 2
fi

# Some GitHub-hosted Windows/amd64 machines are affected by golang/go#81238:
# recovering a hardware exception can write below a goroutine stack and
# corrupt an unrelated heap object. A corrupted testing.T signal channel then
# produces this otherwise-impossible synctest fatal error. Keep the quarantine
# deliberately narrow so ordinary test failures and real synctest bugs remain
# visible.
is_known_runtime_corruption() {
	local log="$1"
	grep -Fq 'fatal error: receive on synctest channel from outside bubble' "${log}" &&
		grep -Fq 'testing.(*T).Run' "${log}" &&
		grep -Eq '^FAIL[[:space:]]+github\.com/xgo-dev/llgo/test(/|[[:space:]])' "${log}" &&
		! grep -Eq '^--- FAIL:|\[build failed\]|^panic:' "${log}"
}

log=
trap '[[ -z "${log}" ]] || rm -f "${log}"' EXIT
for attempt in 1 2; do
	log="$(mktemp "${TMPDIR:-/tmp}/llgo-go-test-windows.XXXXXX")"
	set +e
	"$@" 2>&1 | tee "${log}"
	status=${PIPESTATUS[0]}
	set -e

	if [[ ${status} -eq 0 ]]; then
		rm -f "${log}"
		log=
		exit 0
	fi
	if ! is_known_runtime_corruption "${log}"; then
		rm -f "${log}"
		log=
		exit "${status}"
	fi
	rm -f "${log}"
	log=

	if [[ ${attempt} -eq 1 ]]; then
		echo '::warning title=Retrying upstream Go runtime corruption::LLGO_CI_RETRY_GO_RUNTIME_CORRUPTION: matched golang/go#81238 signature'
		continue
	fi
	echo '::warning title=Quarantined upstream Go runtime corruption::LLGO_CI_QUARANTINED_GO_RUNTIME_CORRUPTION: matched golang/go#81238 signature twice'
	exit 0
done
