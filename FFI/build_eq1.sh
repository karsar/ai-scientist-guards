#!/usr/bin/env bash
# Build and run the Haskell -> verified Eq. 1 kernel FFI check (LordEq1FFI).
# Requires: alr (Alire/GNAT) in the lord_spark project, and cabal/ghc with the
# Monte_Carlo_validation dependencies (vector, mwc-random).
# The proof itself is run by CI (see .github/workflows/spark-verify.yml).
set -euo pipefail

SPARK="$(cd "$(dirname "$0")/../Formal_verification/lord_spark" && pwd)"
MC="$(cd "$(dirname "$0")/../Monte_Carlo_validation" && pwd)"
HERE="$(cd "$(dirname "$0")" && pwd)"
OBJ="$HERE/eq1_obj"
mkdir -p "$OBJ"

# 1. Compile the kernel and its C interface. Checks are suppressed (-gnatp)
#    because GNATprove proves them. Ghost code (Big_Real, lemmas) is not
#    compiled.
( cd "$SPARK" && alr build >/dev/null &&
  SPARKLIB_SRC="$(alr printenv | sed -n 's/^export SPARKLIB_ALIRE_PREFIX="\(.*\)"$/\1/p')/src" &&
  cd "$OBJ" &&
  for unit in lord_eq1 lord_eq1_capi; do
    ( cd "$SPARK" && alr exec -- gcc -c -O2 -gnatp -gnat2012 -I"$SPARKLIB_SRC" \
        src/$unit.adb -o "$OBJ/$unit.o" )
  done )

# 2. Link the Haskell harness against the kernel objects, using the package
#    environment of Monte_Carlo_validation (Lord.hs and its dependencies).
( cd "$MC" && cabal exec -- ghc -O2 -i"$MC/src" -outputdir "$OBJ" \
    "$HERE/LordEq1FFI.hs" "$OBJ/lord_eq1.o" "$OBJ/lord_eq1_capi.o" \
    -o "$HERE/lord_eq1_ffi_check" )

# 3. Run.
"$HERE/lord_eq1_ffi_check"
