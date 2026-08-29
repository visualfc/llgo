#!/bin/bash
set -euo pipefail

jobs="${LLGO_DEMO_JOBS:-1}"
profile="host"
embedded=0
extra=()

while [ "$#" -gt 0 ]; do
  case "$1" in
    --embedded)
      embedded=1
      shift
      ;;
    --profile)
      if [ "$#" -lt 2 ]; then
        echo "test_demo.sh: --profile requires a value" >&2
        exit 2
      fi
      profile="$2"
      shift 2
      ;;
    *)
      extra+=("$1")
      shift
      ;;
  esac
done

run_profile() {
  if [ "${#extra[@]}" -eq 0 ]; then
    GOTOOLCHAIN=local GOWORK=off go run -mod=readonly ./chore/demorun \
      --profile "$1" \
      --jobs "$jobs" \
      --result result.md
  else
    GOTOOLCHAIN=local GOWORK=off go run -mod=readonly ./chore/demorun \
      --profile "$1" \
      --jobs "$jobs" \
      --result result.md \
      "${extra[@]}"
  fi
}

if [ "$embedded" -eq 0 ]; then
  run_profile "$profile"
  exit
fi

status=0
run_profile esp32 || status=1
run_profile esp32c3-basic || status=1
exit "$status"
