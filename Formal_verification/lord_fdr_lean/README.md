# LORD++ FDR Control Formalization in Lean 4

Formal verification of the False Discovery Rate guarantees for the LORD++ online hypothesis testing protocol.

## Overview

This project aims to formalize the key mathematical result underlying LORD++ FDR control:

**Fundamental Lemma**: For a uniform p-value P independent of filtration ℱ, and ℱ-measurable threshold α > 0:

$$\mathbb{E}\left[\frac{\mathbf{1}\{P \leq \alpha\}}{\alpha} \,\Big|\, \mathcal{F}\right] = 1$$

This "perfect accounting" property implies that the null discovery process is a supermartingale, which in turn implies FDR control.

## Structure

```
LordFDR/
├── Basic.lean           -- Core definitions (PValue, LordState, predictability)
├── FundamentalLemma.lean -- Crucial lemmas and theorems
└── OnlineFDR.lean        -- Main theorems  
```

## Setup

1. Install elan:
   ```bash
   curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh
   ```

2. Download Mathlib cache (saves hours of compilation):
   ```bash
   lake exe cache get
   ```

3. Build:
   ```bash
   lake build
   ```

- Ramdas et al. (2017). "Online Control of the False Discovery Rate with Decaying Memory." NeurIPS.
- Javanmard & Montanari (2018). "Online Rules for Control of False Discovery Rate." Annals of Statistics.

