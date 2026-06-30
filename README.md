# ai-scientist-guards

[![Lean 4 Verification](https://github.com/karsar/ai-scientist-guards/actions/workflows/lean4-verify.yml/badge.svg)](https://github.com/karsar/ai-scientist-guards/actions/workflows/lean4-verify.yml)
[![SPARK Verification](https://github.com/karsar/ai-scientist-guards/actions/workflows/spark-verify.yml/badge.svg)](https://github.com/karsar/ai-scientist-guards/actions/workflows/spark-verify.yml)

Replication code for **"Structural Enforcement of Statistical Rigor in AI-Driven Discovery: A Functional Architecture"**

The repository contains the Haskell `Research` monad and declarative scaffold that enforce online FDR control in AI-driven discovery, the harness that performs the statistical tests, the machine-checked Lean 4 proofs, the SPARK/Ada verification of the floating-point wealth invariant, and the experiments.

## What's Inside

### 📐 Formal_verification/

Machine-checked proofs. Every Lean theorem is `sorry`-free and depends only on the three standard axioms (`propext`, `Classical.choice`, `Quot.sound`).

#### `lord_fdr_lean/` — LORD++ in Lean 4

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

GNATprove proves the budget invariant `W(t) ≥ 0` over IEEE 754 double precision, under every rounding sequence.

| File | Contents |
|------|----------|
| `src/lord_pp.{ads,adb}` | Wealth update and sequence loop (the budget invariant, H4) |
| `src/lord_capi.{ads,adb}` | C-exported `lord_new_wealth`, `lord_alpha` for FFI |

```bash
cd Formal_verification/lord_spark
alr build
alr exec -- gnatprove -P lord_spark.gpr --level=2 --steps=100000 --report=statistics
```

All checks proved, 0 unproved, 0 `pragma Assume`: 30 for `lord_pp` (the H4 budget invariant), 5 for `lord_capi`. Lean proves correctness over the reals; SPARK proves IEEE 754 execution cannot violate the wealth invariant.

### 🧮 Research_monad/  (Monte_Carlo_validation)

The Haskell `Research` monad (an `ExceptT`-over-`StateT` stack) makes it impossible to test a hypothesis without updating the statistical state. The Monte Carlo driver reproduces the simulation: a naive approach inflates FDR to ~41%, LORD++ holds it at ~1.1% (N=2000).

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

The Haskell orchestrator calls the GNATprove-verified wealth update directly via the C ABI, rather than re-implementing it.

```bash
cd FFI
bash build.sh    # proves lord_capi, compiles it, links it into Haskell, runs
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
| GNAT / SPARK (via Alire) | `gnatprove` 15.1.x |
| GHC / Cabal | GHC ≥ 9.6 |
| Python | ≥ 3.10 with `scikit-learn`, `numpy`, `scipy` |

## License

MIT License. See [LICENSE](LICENSE).
