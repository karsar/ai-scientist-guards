#!/usr/bin/env bash
# Agreement check between the SPARK Eq. 1 kernel (Lord_Eq1), Lord.hs and
# case_study_rerun.py. Run from this directory. Needs cabal (with the
# Monte_Carlo_validation dependencies), Python with scikit-learn, and Alire.
# The summary is written to eq1_check_results.txt.
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p out
(cd ../../../Monte_Carlo_validation &&
   cabal run -v0 lord-eq1-reference -- \
     ../Formal_verification/lord_spark/test/out/steps_hs.csv \
     ../Formal_verification/lord_spark/test/out/gamma_hs.csv)
python3 ../../../Experiments/eq1_reference.py out/steps_hs.csv out/steps_py.csv
(cd .. && alr exec -- gprbuild -q -P test/lord_eq1_check.gpr)
./obj/lord_eq1_check | tee eq1_check_results.txt
