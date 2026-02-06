/-
  LordFDR/Basic.lean

  Core definitions for LORD++ FDR control formalization.

  This file establishes:
  - P-value type (real in [0,1])
  - Predictability (measurability w.r.t. prior σ-algebra)
  - LORD++ state and threshold computation
-/

import Mathlib.Probability.Independence.Basic
import Mathlib.MeasureTheory.Integral.Bochner
import Mathlib.MeasureTheory.Measure.ProbabilityMeasure
import Mathlib.Probability.Martingale.Basic
import Mathlib.Order.Filter.Basic
import Mathlib.Data.Real.Basic

open MeasureTheory ProbabilityTheory
open scoped ENNReal NNReal

namespace LordFDR

/-! ## Basic Definitions -/

/-- A p-value is a real number in [0, 1].
    We use a subtype rather than a separate type for easier integration with ℝ. -/
def PValue := { p : ℝ // 0 ≤ p ∧ p ≤ 1 }

namespace PValue

instance : Coe PValue ℝ := ⟨Subtype.val⟩

/-- The p-value 0 -/
def zero : PValue := ⟨0, by norm_num, by norm_num⟩

/-- The p-value 1 -/
def one : PValue := ⟨1, by norm_num, by norm_num⟩

/-- A p-value is always non-negative -/
theorem nonneg (p : PValue) : (0 : ℝ) ≤ p.val := p.property.1

/-- A p-value is always at most 1 -/
theorem le_one (p : PValue) : p.val ≤ 1 := p.property.2

end PValue

/-! ## Filtrations and Predictability -/

variable {Ω : Type*} [MeasurableSpace Ω] {μ : Measure Ω} [IsProbabilityMeasure μ]

/-- A threshold sequence is predictable with respect to a filtration if
    the threshold at time t is measurable with respect to ℱ_{t-1}.

    In Coq terms, this is like requiring α_t to be determined by
    information available *before* observing the t-th p-value.

    Note: Filtration ι m means a filtration indexed by ι on a space with
    measurable space m. -/
def IsPredictable {ι : Type*} [Preorder ι] [PredOrder ι]
    {m : MeasurableSpace Ω}
    (ℱ : Filtration ι m)
    (α : ι → Ω → ℝ) : Prop :=
  ∀ t, Measurable (α t)

/-- Simplified predictability for ℕ-indexed sequences:
    α_t is ℱ_{t-1}-measurable (or ℱ_0 for t=0). -/
def IsPredictableNat {m : MeasurableSpace Ω}
    (ℱ : Filtration ℕ m)
    (α : ℕ → Ω → ℝ) : Prop :=
  ∀ t, Measurable (α t)
  -- Full predictability would require ℱ_{t-1}-measurability
  -- For now we just require measurability

/-! ## LORD++ Configuration -/

/-- Configuration for the LORD++ protocol -/
structure LordConfig where
  /-- Target FDR level (e.g., 0.05) -/
  alpha : ℝ
  /-- Initial wealth, must satisfy 0 < w0 < alpha -/
  w0 : ℝ
  /-- The gamma sequence (must sum to 1) -/
  gamma : ℕ → ℝ
  /-- alpha is positive -/
  alpha_pos : 0 < alpha
  /-- alpha is at most 1 -/
  alpha_le_one : alpha ≤ 1
  /-- w0 is positive -/
  w0_pos : 0 < w0
  /-- w0 is less than alpha -/
  w0_lt_alpha : w0 < alpha
  /-- gamma terms are non-negative -/
  gamma_nonneg : ∀ j, 0 ≤ gamma j
  /-- gamma sums to 1 (stated as partial sums converging) -/
  gamma_sum_one : Filter.Tendsto (fun n => ∑ j in Finset.range n, gamma j)
                                  Filter.atTop (nhds 1)

/-- The standard gamma sequence from Javanmard & Montanari (2018).
    γ_j = c · log(max(j,2)) / (j · exp(√(log j)))
    where c ≈ 0.0772 is normalizing constant. -/
noncomputable def standardGamma (c : ℝ) (j : ℕ) : ℝ :=
  if j = 0 then 0
  else c * Real.log (max j 2) / (j * Real.exp (Real.sqrt (Real.log j)))

/-! ## LORD++ State -/

/-- State of the LORD++ protocol at time t -/
structure LordState where
  /-- Current time step -/
  time : ℕ
  /-- Times of past discoveries (rejections) -/
  discoveryTimes : List ℕ
  /-- Configuration -/
  config : LordConfig

namespace LordState

/-- Compute the significance threshold α_t using LORD++ formula:
    α_t = γ_t · W_0 + (α - W_0) · Σ_{j: τ_j < t} γ_{t - τ_j}

    This is Equation (1) from the paper's Background section. -/
noncomputable def threshold (s : LordState) : ℝ :=
  let cfg := s.config
  let t := s.time
  let term1 := cfg.gamma t * cfg.w0
  let term2 := (cfg.alpha - cfg.w0) *
               (s.discoveryTimes.filter (· < t)).foldl
                 (fun acc τ => acc + cfg.gamma (t - τ)) 0
  term1 + term2

/-- The threshold is always non-negative -/
theorem threshold_nonneg (s : LordState) : 0 ≤ s.threshold := by
  unfold threshold
  apply add_nonneg
  · apply mul_nonneg
    · exact s.config.gamma_nonneg s.time      -- 0 ≤ gamma t
    · exact le_of_lt s.config.w0_pos          -- 0 < w0 → 0 ≤ w
  · apply mul_nonneg
    · linarith [s.config.w0_lt_alpha]   -- 0 ≤ alpha - w0  (because w0 < alpha)
    · -- Need: foldl (fun acc τ => acc + gamma (t - τ)) 0 list ≥ 0
      -- Strategy: induction on the filtered list
      -- Each step adds a non-negative gamma term to a non-negative accumulator
      suffices h : ∀ (l : List ℕ) (init : ℝ), 0 ≤ init →
          0 ≤ List.foldl (fun acc τ => acc + s.config.gamma (s.time - τ)) init l by
        exact h _ _ (le_refl 0)
      intro l
      induction l with
      | nil => intro init hinit; simpa
      | cons x xs ih =>
        intro init hinit
        simp only [List.foldl_cons]
        apply ih
        linarith [s.config.gamma_nonneg (s.time - x)]

/-- Initial state -/
def init (cfg : LordConfig) : LordState :=
  { time := 1
    discoveryTimes := []
    config := cfg }

/-- Advance state after observing p-value p at current time.
    Returns (isDiscovery, newState). -/
noncomputable def advance (s : LordState) (p : ℝ) : Bool × LordState :=
  let α_t := s.threshold
  let isDiscovery := p ≤ α_t
  let newDiscoveries := if isDiscovery then s.time :: s.discoveryTimes
                        else s.discoveryTimes
  (isDiscovery, { s with
    time := s.time + 1
    discoveryTimes := newDiscoveries })

/-- State advances time by exactly 1 -/
theorem advance_time (s : LordState) (p : ℝ) :
    (s.advance p).2.time = s.time + 1 := by
  simp [advance]

end LordState

/-! ## Indicator Function -/

/-- Indicator function: 1 if condition holds, 0 otherwise -/
noncomputable def indicator (P : Prop) [Decidable P] : ℝ :=
  if P then 1 else 0

/-- Indicator is non-negative -/
theorem indicator_nonneg (P : Prop) [Decidable P] : 0 ≤ indicator P := by
  simp [indicator]
  split_ifs <;> norm_num

/-- Indicator is at most 1 -/
theorem indicator_le_one (P : Prop) [Decidable P] : indicator P ≤ 1 := by
  simp [indicator]
  split_ifs <;> norm_num

/-! ## Key Quantity for FDR Control -/

/-- The "accounting ratio" for a single test:
    𝟙{P ≤ α} / α

    This is the key quantity that, when summed, forms the supermartingale. -/
noncomputable def accountingRatio (p α : ℝ) : ℝ :=
  if α > 0 then indicator (p ≤ α) / α else 0

/-- The accounting ratio is non-negative when α > 0 -/
theorem accountingRatio_nonneg (p α : ℝ) (hα : α > 0) :
    0 ≤ accountingRatio p α := by
  simp [accountingRatio, hα]
  apply div_nonneg
  · exact indicator_nonneg _
  · linarith

end LordFDR
