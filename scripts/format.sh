#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 {check|apply}" >&2
  exit 1
}

if [ $# -ne 1 ]; then
  usage
fi

format_file="${FORMAT_FILE:-include/eurydice_glue.h}"

case "$1" in
  check)
    status=0
    if ! dune build @fmt; then
      status=1
    fi

    tmp_file="$(mktemp)"
    trap 'rm -f "$tmp_file"' EXIT

    clang-format "$format_file" > "$tmp_file"
    if ! diff "$format_file" "$tmp_file"; then
      status=1
    fi

    if [ "$status" -ne 0 ]; then
      echo -e "\033[0;31m⚠️⚠️⚠️ SUGGESTED: make format-apply\033[0;m"
    fi
    exit "$status"
    ;;
  apply)
    dune fmt >/dev/null || true
    clang-format -i "$format_file"
    ;;
  *)
    usage
    ;;
esac
