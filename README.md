# ai-scientist-guards

Replication code for **"Structural Enforcement of Statistical Rigor in AI-Driven Discovery: A Functional Architecture"**

This repository contains the Haskell implementation of the Research monad and declarative scaffolding system that enforces FDR control in AI-driven scientific discovery.

## What's Inside

### 📊 Monte_Carlo_validation/
Contains code for the **large-scale simulation study** (Experiment 1 in the paper). This validates that:
- The monadic implementation of LORD++ behaves correctly
- FDR control is essential at scale (N=2000 hypotheses)
- The naive approach leads to massive FDR inflation

Run this to see the Research monad in action and reproduce Table 1 from the paper.

### 🔬 SVM case study/
Contains the **end-to-end case study** demonstrating the integrated architecture (Monad + Scaffolding) with real LLM interaction.

**What's in here:**
- **`baseline/`** - The initial suboptimal SVM Python code that serves as the starting point
- **`create_datasets.py`** - Downloads and splits the Wine dataset into exploration/validation sets (saves to `data/` folder)
- **`prompt.json`** - The base prompt we feed to the LLM to guide code generation
- **`seed_ideas.json`** - The five optimization hypotheses we test (in a more complete version, the LLM would generate these itself, but we're keeping it simple here)
- Main orchestration code that ties everything together

This reproduces the workflow from Table 2 in the paper. **Note:** Results will vary slightly due to LLM non-determinism.

## Dependencies

- Haskell (GHC + Stack or Cabal)
- Python 3.x (for the case study execution)
- An LLM API (we used GPT-4o)
