# ai-scientist-guards

Replication code for **"Structural Enforcement of Statistical Rigor in AI-Driven Discovery: A Functional Architecture"**

This repository contains the Haskell implementation of the Research monad and declarative scaffolding system that enforces FDR control in AI-driven scientific discovery, as well as complete formal proofs for online FDR control in Lean and additional SPARK-based verification of correctness for the transition from real numbers to double-precision floats.

## What's Inside

### 📊 Monte_Carlo_validation/
Contains code for the **large-scale simulation study** (Experiment 1 in the paper). This validates that:
- The monadic implementation of LORD++ behaves correctly
- FDR control is essential at scale (N=2000 hypotheses)
- The naive approach leads to massive FDR inflation

Run this to see the Research monad in action and reproduce results from the paper.

### 🔬 SVM case study/
Contains the **end-to-end case study** demonstrating the integrated architecture (Monad + Scaffolding) with real LLM interaction.

**What's in here:**
- **`baseline/`** - The initial suboptimal SVM Python code that serves as the starting point
- **`create_datasets.py`** - Downloads and splits the Wine dataset into exploration/validation sets (saves to `data/` folder)
- **`prompt.json`** - The base prompt we feed to the LLM to guide code generation
- **`seed_ideas.json`** - The five optimization hypotheses we test (in a more complete version, the LLM would generate these itself, but we're keeping it simple here)
- Main orchestration code that ties everything together

This reproduces the workflow from the paper. **Note:** Results will vary slightly due to LLM non-determinism.

### 🔐 Formal_verification/
Contains **machine-checked proofs** of the core FDR guarantees:

#### `lean4/` — Mathematical Proof
Lean 4 formalization proving the fundamental lemma underlying FDR control:
- P-value and predictability definitions using Mathlib
- Threshold non-negativity
- Supermartingale property of null discovery process
- FDR ≤ α guarantee

```bash
cd Formal_verification/lord_fdr_lean
lake exe cache get  # Download Mathlib cache
lake build
```

#### `spark/` — IEEE 754 Verification  
SPARK/Ada code proving budget soundness (H4) at the floating-point level:
- Wealth non-negativity preserved across all steps
- No overflow possible for bounded inputs
- 31 verification conditions, all discharged by GNATprove

```bash
cd Formal_verification/spark_lord
mkdir -p obj
gnatprove -P lord_spark.gpr --level=2
```

Together, Lean proves correctness in real arithmetic while SPARK proves IEEE 754 execution cannot violate the invariants.

## Dependencies

- **Haskell** (GHC + Stack or Cabal) — for Monte Carlo and case study
- **Python 3.x** — for case study execution
- **LLM API** (we used GPT-4o) — for case study
- **Lean 4 + Mathlib** — for formal proofs
- **GNAT/SPARK** — for floating-point verification, Monte-Carlo demonstration

## Quick Start

### Monte Carlo Validation
```bash
cd Monte_Carlo_validation
cabal build
cabal run ai-scientist-validation
```

### SVM Case Study
```bash
cd "SVM case study"
python create_datasets.py        # Create exploration/validation split
export OPENAI_API_KEY=...        # Set your API key
cabal build
cabal run ai-scientist-case-study
```

## License

MIT License. See [LICENSE](LICENSE).
