#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "${ROOT_DIR}"

module_path="$(go list -m)"

packages=()
while IFS= read -r pkg; do
  packages+=("${pkg}")
done < <(go list ./test/std/... | sort)

if [ "${#packages[@]}" -eq 0 ]; then
  echo "No stdlib test packages discovered under test/std" >&2
  exit 0
fi

args=()
covered_packages=()
for pkg in "${packages[@]}"; do
  rel_path="${pkg#${module_path}/}"
  if [[ "${rel_path}" != test/std/* ]]; then
    continue
  fi
  stdlib_pkg="${rel_path#test/std/}"
  covered_packages+=("${stdlib_pkg}")
  if [[ "${stdlib_pkg}" == "runtime" ]]; then
    continue
  fi
  args+=("-pkg" "${stdlib_pkg}")
done

expected_file="$(mktemp)"
covered_file="$(mktemp)"
trap 'rm -f "${expected_file}" "${covered_file}"' EXIT

# The std pattern also reports packages whose every source file is excluded by
# the current platform (for example plugin and log/syslog on Windows). Require
# coverage for the packages that actually have buildable Go or cgo sources.
go list -e -f '{{if or .GoFiles .CgoFiles}}{{.ImportPath}}{{end}}' std \
	| awk 'NF' \
  | awk '!/(^|\/)internal(\/|$)/ && !/(^|\/)vendor(\/|$)/' \
  | sort -u > "${expected_file}"
printf '%s\n' "${covered_packages[@]}" | sort -u > "${covered_file}"

missing_packages="$(comm -23 "${expected_file}" "${covered_file}")"
if [[ -n "${missing_packages}" ]]; then
  echo "Public standard-library packages missing test/std coverage:" >&2
  while IFS= read -r pkg; do
    echo "  - ${pkg}" >&2
  done <<< "${missing_packages}"
  exit 1
fi

expected_count="$(wc -l < "${expected_file}" | tr -d ' ')"
covered_count="$(wc -l < "${covered_file}" | tr -d ' ')"
echo "Public standard-library package coverage: ${covered_count}/${expected_count}"

printf '+ go run ./chore/check_std_symbols'
for arg in "${args[@]}"; do
  printf ' %q' "${arg}"
done
printf '\n'

go run ./chore/check_std_symbols "${args[@]}"
