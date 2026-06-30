/-
  LordFDR/PathwiseBudget.lean — The pathwise budget bound for LORD++.

  This file closes the gap left open in OnlineFDR.lean, where the budget
  hypothesis `∑ α_t ≤ q` was *assumed*. Here we *derive*, from the actual
  LORD++ threshold formula (Equation 1, `LordState.threshold` in Basic.lean)
  together with `∑ γ ≤ 1`, the pathwise inequality

      ∑_{t ≤ T} α_t  ≤  α · max (R, 1)

  where `R` is the number of discoveries. This is the bound that, combined
  with `E[V] = ∑ E[α_t]` (OnlineFDR.lean), yields machine-checked mFDR
  control for the *reward-bearing* procedure that the SPARK kernel runs.

  Unlike a pathwise `∑ α_t ≤ W₀`, which fails once a discovery refunds
  wealth, this bound holds with rewards switched on.
-/

import LordFDR.Basic

open scoped BigOperators

namespace LordFDR

/-- The LORD++ threshold (Equation 1) in pathwise form, parameterised by the
    finite set `D` of discovery times. This is `LordState.threshold` with the
    discovery list represented as a `Finset`:
    `α_t = γ_t · W₀ + (α − W₀) · ∑_{τ ∈ D, τ < t} γ_(t−τ)`. -/
noncomputable def lordThreshold (cfg : LordConfig) (D : Finset ℕ) (t : ℕ) : ℝ :=
  cfg.gamma t * cfg.w0
    + (cfg.alpha - cfg.w0) * ∑ τ ∈ D.filter (· < t), cfg.gamma (t - τ)

/-- Partial sums of the γ-sequence never exceed 1: the terms are non-negative
    and the full series converges to 1, so every partial sum is ≤ 1. -/
lemma gamma_sum_range_le_one (cfg : LordConfig) (n : ℕ) :
    ∑ j ∈ Finset.range n, cfg.gamma j ≤ 1 := by
  have hmono : Monotone (fun n => ∑ j ∈ Finset.range n, cfg.gamma j) := by
    intro a b hab
    exact Finset.sum_le_sum_of_subset_of_nonneg
      (Finset.range_subset.mpr hab) (fun j _ _ => cfg.gamma_nonneg j)
  exact hmono.ge_of_tendsto cfg.gamma_sum_one n

/-- Any finite sum of γ over `Icc 1 T` is ≤ 1. -/
lemma gamma_sum_Icc_le_one (cfg : LordConfig) (T : ℕ) :
    ∑ t ∈ Finset.Icc 1 T, cfg.gamma t ≤ 1 := by
  refine le_trans (Finset.sum_le_sum_of_subset_of_nonneg ?_
    (fun j _ _ => cfg.gamma_nonneg j)) (gamma_sum_range_le_one cfg (T + 1))
  intro x hx
  rw [Finset.mem_Icc] at hx
  exact Finset.mem_range.mpr (by omega)

/-- For a fixed discovery time `τ`, the contribution it makes across all later
    steps is `∑_{t > τ, t ≤ T} γ_(t−τ)`, which is ≤ 1 because `t ↦ t − τ` is
    injective into the γ-sequence. -/
lemma discovery_contribution_le_one (cfg : LordConfig) (T τ : ℕ) :
    ∑ t ∈ (Finset.Icc 1 T).filter (fun t => τ < t), cfg.gamma (t - τ) ≤ 1 := by
  have hinj : ∀ x ∈ (Finset.Icc 1 T).filter (fun t => τ < t),
      ∀ y ∈ (Finset.Icc 1 T).filter (fun t => τ < t),
      x - τ = y - τ → x = y := by
    intro x hx y hy hxy
    simp only [Finset.mem_filter, Finset.mem_Icc] at hx hy
    omega
  rw [← Finset.sum_image hinj]
  refine le_trans (Finset.sum_le_sum_of_subset_of_nonneg ?_
    (fun j _ _ => cfg.gamma_nonneg j)) (gamma_sum_range_le_one cfg (T + 1))
  intro j hj
  simp only [Finset.mem_image, Finset.mem_filter, Finset.mem_Icc] at hj
  obtain ⟨t, ⟨⟨_, htT⟩, _⟩, rfl⟩ := hj
  exact Finset.mem_range.mpr (by omega)

/-- Swapping the order of summation: the total discovery contribution equals a
    sum over discovery times of each one's later contribution, hence ≤ `|D|`. -/
lemma double_sum_le_card (cfg : LordConfig) (D : Finset ℕ) (T : ℕ) :
    ∑ t ∈ Finset.Icc 1 T, ∑ τ ∈ D.filter (· < t), cfg.gamma (t - τ)
      ≤ (D.card : ℝ) := by
  have hswap :
      ∑ t ∈ Finset.Icc 1 T, ∑ τ ∈ D.filter (· < t), cfg.gamma (t - τ)
        = ∑ τ ∈ D, ∑ t ∈ (Finset.Icc 1 T).filter (fun t => τ < t),
            cfg.gamma (t - τ) := by
    simp only [Finset.sum_filter]
    rw [Finset.sum_comm]
  rw [hswap]
  calc ∑ τ ∈ D, ∑ t ∈ (Finset.Icc 1 T).filter (fun t => τ < t), cfg.gamma (t - τ)
      ≤ ∑ _τ ∈ D, (1 : ℝ) :=
        Finset.sum_le_sum (fun τ _ => discovery_contribution_le_one cfg T τ)
    _ = (D.card : ℝ) := by rw [Finset.sum_const, nsmul_eq_mul, mul_one]

/-- Arithmetic core: with `0 < W₀ < α`, the affine-in-`R` budget bound
    `W₀ + (α − W₀)·R` never exceeds `α · max(R, 1)`. This is where the reward
    `(α − W₀)` per discovery is absorbed. -/
lemma budget_arith {alpha w0 : ℝ} {R : ℕ} (hw0 : 0 < w0) (hlt : w0 < alpha) :
    w0 + (alpha - w0) * (R : ℝ) ≤ alpha * max (R : ℝ) 1 := by
  rcases Nat.eq_zero_or_pos R with hR | hR
  · subst hR
    simp only [Nat.cast_zero, mul_zero, add_zero, max_eq_right (zero_le_one), mul_one]
    linarith
  · have h1 : (1 : ℝ) ≤ (R : ℝ) := by exact_mod_cast hR
    rw [max_eq_left h1]
    nlinarith [h1, hw0, hlt]

/-- **Pathwise budget bound for LORD++.** For any set of discovery times
    `D ⊆ Icc 1 T`, the LORD++ thresholds over the horizon satisfy
    `∑_{t} α_t ≤ α · max(|D|, 1)`. The budget is therefore a *consequence* of
    the threshold formula and `∑ γ ≤ 1`, not an assumption — and it holds with
    the reward term live. -/
theorem lordThreshold_sum_le (cfg : LordConfig) (D : Finset ℕ) (T : ℕ) :
    ∑ t ∈ Finset.Icc 1 T, lordThreshold cfg D t
      ≤ cfg.alpha * max (D.card : ℝ) 1 := by
  have hexp :
      ∑ t ∈ Finset.Icc 1 T, lordThreshold cfg D t
        = cfg.w0 * (∑ t ∈ Finset.Icc 1 T, cfg.gamma t)
          + (cfg.alpha - cfg.w0)
            * ∑ t ∈ Finset.Icc 1 T, ∑ τ ∈ D.filter (· < t), cfg.gamma (t - τ) := by
    unfold lordThreshold
    rw [Finset.sum_add_distrib, ← Finset.sum_mul, ← Finset.mul_sum]
    ring
  rw [hexp]
  have hge : 0 ≤ cfg.alpha - cfg.w0 := by linarith [cfg.w0_lt_alpha]
  have hbound :
      cfg.w0 * (∑ t ∈ Finset.Icc 1 T, cfg.gamma t)
        + (cfg.alpha - cfg.w0)
          * ∑ t ∈ Finset.Icc 1 T, ∑ τ ∈ D.filter (· < t), cfg.gamma (t - τ)
        ≤ cfg.w0 * 1 + (cfg.alpha - cfg.w0) * (D.card : ℝ) := by
    apply add_le_add
    · exact mul_le_mul_of_nonneg_left (gamma_sum_Icc_le_one cfg T)
        (le_of_lt cfg.w0_pos)
    · exact mul_le_mul_of_nonneg_left (double_sum_le_card cfg D T) hge
  refine le_trans hbound ?_
  rw [mul_one]
  exact budget_arith cfg.w0_pos cfg.w0_lt_alpha

end LordFDR
