#!/usr/bin/env bash

######################################################################
# Generates a cradle config for haskell-language-server. Use this if
# you want faster reloads and cross-package jump-to-definition. It
# needs to be run from the nix-shell.
######################################################################

set -euo pipefail

make_hie_bios() {
  ghci_flags
  list_modules
}

: "${HIE_BIOS_OUTPUT:=/dev/stdout}"
: "${HIE_BIOS_ARG:=${1:-}}"

cd "$(dirname "$0")"/.. || exit

source_file=$(echo "$HIE_BIOS_ARG" | sed "s=$(pwd)/==")

if [[ "$source_file" =~ ^lib/.*\.hs$ || -z "$source_file" ]]; then
  echo "Generating $HIE_BIOS_OUTPUT for source file $source_file."
  . "$(dirname "$0")/cabal-lib.sh"

  setup_cabal_plan
  make_hie_bios > "$HIE_BIOS_OUTPUT"

  echo "Finished."
else
  echo "Ignoring $source_file."
fi
