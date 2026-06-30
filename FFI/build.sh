#!/usr/bin/env bash
# Build the Haskell -> verified-SPARK-kernel FFI demo.
# Requires: alr (Alire/GNAT) in the spark_lord project, and ghc.
set -euo pipefail

SPARK="$(cd "$(dirname "$0")/../Formal_verification/lord_spark" && pwd)"
HERE="$(cd "$(dirname "$0")" && pwd)"

# 1. Prove the C-API kernel (contracts discharged by GNATprove).
( cd "$SPARK" && alr exec -- gnatprove -P lord_spark.gpr --level=2 -u lord_capi )

# 2. Compile the exported kernel to a self-contained object (checks suppressed:
#    the pre/postconditions are already proved, so runtime checks are redundant).
( cd "$SPARK" && alr exec -- gcc -c -O2 -gnatp src/lord_capi.adb -o "$HERE/lord_capi.o" )

# 3. Link the Haskell orchestrator against the verified object.
ghc -O2 "$HERE/LordFFI.hs" "$HERE/lord_capi.o" -o "$HERE/lord_ffi_demo"

# 4. Run.
"$HERE/lord_ffi_demo"
