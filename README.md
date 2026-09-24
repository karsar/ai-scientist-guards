# ai-scientist-guards

[![Lean 4 Verification](https://github.com/karsar/ai-scientist-guards/actions/workflows/lean4-verify.yml/badge.svg)](https://github.com/karsar/ai-scientist-guards/actions/workflows/lean4-verify.yml)
[![SPARK Verification](https://github.com/karsar/ai-scientist-guards/actions/workflows/spark-verify.yml/badge.svg)](https://github.com/karsar/ai-scientist-guards/actions/workflows/spark-verify.yml)

Replication code for **"Structural Enforcement of Statistical Rigor in AI-Driven Discovery: A Functional Architecture"**

The repository contains the Haskell `Research` monad and declarative scaffold that enforce online FDR control in AI-driven discovery, the harness that performs the statistical tests, the machine-checked Lean 4 proofs, the SPARK/Ada verification of the floating-point wealth invariant, and the experiments.

## What's Inside

### 📐 Formal_verification/

Machine-checked proofs. Every Lean theorem is `sorry`-free and depends only on the three standard axioms (`propext`, `Classical.choice`, `Quot.sound`).

#### `lord_fdr_lean/` — LORD in Lean 4

The formalized procedure is LORD (Javanmard and Montanari 2018, version 2, reward alpha - w0 per discovery).

| File | Result |
|------|--------|
| `LordFDR/FundamentalLemma.lean` | `E[1{P≤α}/α \| F] = 1` for uniform, independent null p-values |
| `LordFDR/OnlineFDR.lean` | FDR bound from an assumed pathwise budget |
| `LordFDR/PathwiseBudget.lean` | `lordThreshold_sum_le`: the budget **derived** from Eq. (1), `Σα_t ≤ α·max(R,1)` |
| `LordFDR/MFDR.lean` | `lord_mfdr`: marginal FDR control for the reward-bearing procedure |
| `LordFDR/FDR.lean` | `fdr_le`: full `E[V/max(R,1)] ≤ α` via leave-one-out (independent, non-adaptive) |

```bash
cd Formal_verification/lord_fdr_lean
lake exe cache get      # download the prebuilt Mathlib cache (avoids a multi-hour build)
lake build
```

Toolchain and dependencies are pinned by `lean-toolchain`, `lakefile.lean`, and `lake-manifest.json`. To audit a theorem's trust base:

```bash
echo 'import LordFDR.FDR
#print axioms LordFDR.FDR.fdr_le' | lake env lean --stdin
```

#### `lord_spark/` — IEEE 754 verification

GNATprove proves properties of the IEEE 754 double-precision (`Long_Float`) code, under every rounding sequence.

| File | Contents |
|------|----------|
| `src/lord_pp.{ads,adb}` | Multiplicative alpha-investing wealth update (`alpha_t = gamma_t * W`) and sequence loop: `W(t) ≥ 0` (H4) |
| `src/lord_capi.{ads,adb}` | C-exported `lord_new_wealth`, `lord_alpha` for FFI |
| `src/lord_eq1.{ads,adb}` | The LORD thresholds of Eq. 1 (Javanmard and Montanari 2018, version 2, reward alpha - w0 per discovery), with the wealth update `W := W - alpha_t + (alpha - W0) * R_t` |
| `src/lord_eq1_exact.{ads,adb}` | Ghost proof that the Eq. 1 thresholds never exceed the wealth (no clamping), under a margin condition on the gamma table |
| `src/lord_eq1_capi.{ads,adb}` | C-exported `lord_eq1_*` procedures for FFI |
| `test/` | Agreement check of `Lord_Eq1` against `Lord.hs` and `case_study_rerun.py` (not part of the proof) |

```bash
cd Formal_verification/lord_spark
alr build
alr exec -- gnatprove -P lord_spark.gpr --level=2 --steps=100000 --timeout=60 --checks-as-errors=on
```

All 778 checks proved (716 by the provers, 62 by flow analysis), 0 unproved, 0 `pragma Assume`, no justifications: 30 for `lord_pp`, 5 for `lord_capi`, 138 for `lord_eq1`, 532 for `lord_eq1_exact`, 11 for `lord_eq1_capi`. CI runs this command and fails on any unproved check. Level 2 sets a 5-second limit per check; `--timeout=60` raises it, because some `lord_eq1_exact` checks take longer. `lord_eq1_exact` is compiled as Ada 2022 (for its ghost `Big_Real` code); the other units stay Ada 2012.

**What `Lord_Eq1` proves** (goals G1, G3, G4). For any gamma table with values in [0, 1]:

* no run-time errors (overflow, range, index), and `alpha_t ≥ 0`;
* `Advance` spends `Alpha_T = min(Eq1, W)`, so `Alpha_T ≤ W` before each update and `W ≥ 0` after it. `Clamp_Count` counts the steps where the clamp changes the value; at every other step `Alpha_T` equals `Threshold`, the Eq. 1 value computed in floating point, with the sum over discoveries added left to right (`Disc_Sum`);
* `Run_Sequence` carries these properties over a whole p-value sequence.

**What `Lord_Eq1_Exact` proves** (goal G2). Let `P(n)` be the exact real sum `gamma_1 + ... + gamma_n` of the floating-point table values. If

    W0 * (1 - P(n)) ≥ Min_Margin   for all n ≤ Max_T = 10 000,   Min_Margin = 10 000 * 10 005 * 2^-38 ≈ 3.64e-4,

then at every step the unclamped Eq. 1 value computed in floating point is at most the wealth, so the clamp never acts: `Advance` computes exactly Eq. 1 and `Clamp_Count` stays 0 (`Advance_Exact`, `Run_Sequence_Exact`). The proof carries a ghost invariant in exact real arithmetic: the floating-point wealth is at least its real-number value minus `T * Step_Err`. It bounds each rounding with the rounding-error lemmas of SPARKlib. All `Big_Real` code is ghost: the compiled kernel is `Long_Float` arithmetic only. For the paper's configuration (`W0 = 0.1 * alpha = 0.005`, `P(2000) ≈ 0.326`), `W0 * (1 - P) ≈ 3.4e-3`.

**What is not proved.** The margin is necessary: with floating-point prefix sums of gamma equal to 1.0, rounding can make the Eq. 1 value exceed the wealth by one unit in the last place (a concrete case is replayed by `test/lord_eq1_check.adb`). The gamma table is computed outside SPARK (with `log`, `exp`, `sqrt`); the margin condition is a precondition on that table, not checked at run time. SPARK proves the floating-point budget, not the FDR guarantee itself, which is the Lean part.

**Agreement check** (`test/run_eq1_check.sh`, results in `test/eq1_check_results.txt`). On the paper's Tables 1, 4 and 5 and on 100 Monte Carlo runs (N = 2000, alpha = 0.05, W0 = 0.1 alpha, fixed seeds), the `Lord_Eq1` thresholds are bit-identical to `Lord.hs` in all 200 020 steps. Against `case_study_rerun.py` no decision differs; the thresholds differ by at most 5.9e-16 (relative), because Python's `sum()` uses compensated summation. `Clamp_Count` is 0 in every run. Note: `Lord.hs` uses the gamma constant `c = 0.07720838` and `case_study_rerun.py` uses `c = 0.0772`; the check uses each program's own constant.

### 🧮 Research_monad/  (Monte_Carlo_validation)

The Haskell `Research` monad (an `ExceptT`-over-`StateT` stack) makes it impossible to test a hypothesis without updating the statistical state. The Monte Carlo driver reproduces the simulation: a naive approach inflates FDR to ~41%, LORD holds it at ~1.1% (N=2000).

```bash
cd Monte_Carlo_validation
cabal build && cabal run ai-scientist-validation
```

### 🧪 Harness/

The harness-controlled statistics and data separation.

- `verified_stats.py` — `paired_permutation_pvalue`: a paired sign-flip permutation test on held-out per-example losses; super-uniform under the null (condition H1).
- `make_disjoint_splits.py` — disjoint, stratified, pre-assigned per-hypothesis validation splits (independence by construction, condition H3).
- `harness_disjoint.py` — the generated harness wiring both together.

### 🔬 Experiments/

```bash
cd Experiments
python calibration_experiment.py   # H1: permutation super-uniform vs CV t-test anti-conservative
python case_study_rerun.py         # wine: CV t-test (spurious discoveries) vs permutation (none)
python case_study_large.py         # moons: valid pipeline discovers real effects, rejects nulls
```

### 🔁 FFI/

A Haskell harness drives the GNATprove-verified wealth update directly via the C ABI, demonstrating that the proved arithmetic can be called from Haskell rather than re-implemented. (The simulation and case-study drivers compute thresholds with the closed-form LORD update in `Lord.hs`; this harness shows the verified kernel is a drop-in for the multiplicative wealth step.)

```bash
cd FFI
bash build.sh    # proves lord_capi, compiles it, links it into Haskell, runs
```

A second harness, `LordEq1FFI.hs`, drives the verified Eq. 1 kernel (`lord_eq1_capi`) through the C ABI and checks it against `calculateNextAlpha` of `Lord.hs` on the same sequences (paper tables and 100 Monte Carlo runs): all thresholds are bit-identical. The paper's experiments still use `Lord.hs`.

```bash
cd FFI
bash build_eq1.sh    # compiles lord_eq1 + lord_eq1_capi, links them into Haskell, runs the check
```

### 🔐 Leakage_experiment/

Adversarial evaluation of the OS-level data-separation boundary (system-call-level leak detection). See `Leakage_experiment/README.md`.

### ⚙️ SVM case study/

The end-to-end SVM/Wine scaffolding workflow with LLM code generation.

## Dependencies

| Tool | Version |
|------|---------|
| Lean / Lake (via `elan`) | pinned by `lean-toolchain` |
| Mathlib | pinned by `lake-manifest.json` |
| GNAT / SPARK (via Alire) | `gnatprove` 15.1.x, `sparklib` 15.1.x |
| GHC / Cabal | GHC ≥ 9.6 |
| Python | ≥ 3.10 with `scikit-learn`, `numpy`, `scipy` |

## License

MIT License. See [LICENSE](LICENSE).
