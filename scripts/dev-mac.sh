#!/bin/bash
# Local development on your own machine, no GitHub involved.
#
#   ./scripts/dev-mac.sh               run the app with live reload (edit, save, see it)
#   ./scripts/dev-mac.sh path/to.jpg   same, opening that photo in Develop
#   ./scripts/dev-mac.sh repl          a REPL with the live-reload helpers loaded
#   ./scripts/dev-mac.sh test          run the test suite
#   ./scripts/dev-mac.sh package       build the .dmg (same as scripts/package-mac.sh)
#
# Works on Intel and Apple-silicon Macs: the JavaFX / OpenCV / LibRaw natives are
# picked for the machine you run it on (see project.clj).
# One-time setup:  brew install --cask temurin@21 && brew install leiningen
set -euo pipefail
cd "$(dirname "$0")/.."

fail() { echo "dev-mac.sh: $*" >&2; exit 1; }

command -v java >/dev/null || fail "Java not found. Install it with: brew install --cask temurin@21"
JAVA_MAJOR=$(java -version 2>&1 | awk -F '"' '/version/ {split($2, v, "."); print (v[1] == "1" ? v[2] : v[1]); exit}')
[ "${JAVA_MAJOR:-0}" -ge 17 ] 2>/dev/null || fail "Java 17 or newer is required (found: ${JAVA_MAJOR:-none}). Install: brew install --cask temurin@21"
command -v lein >/dev/null || fail "Leiningen not found. Install it with: brew install leiningen"
[ "$(uname)" = "Darwin" ] || echo "note: not macOS ($(uname)); this runs, but the app is built for Macs."

# Lein profile arguments; override DEV_LEIN_PROFILE only if your setup needs it.
PROFILE=${DEV_LEIN_PROFILE:-with-profile +dev}

case "${1:-}" in
  repl)    shift; exec lein $PROFILE repl "$@" ;;
  test)    shift; exec lein $PROFILE test "$@" ;;
  package) shift; exec ./scripts/package-mac.sh "$@" ;;
  *)       exec lein $PROFILE run -m darkroom.dev "$@" ;;
esac
