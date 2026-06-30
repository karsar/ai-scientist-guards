/-
  LordFDR/MFDR.lean — Machine-checked mFDR control for reward-bearing LORD++.

  OnlineFDR.lean proves `FDR ≤ q` *assuming* a pathwise budget `∑ α_t ≤ q`.
  That hypothesis is false for LORD++ once a discovery refunds wealth, so the
  abstract theorem does not apply to the procedure the SPARK kernel runs.

  Here we close that gap honestly. We prove

      E[V]  ≤  α · E[max(R, 1)]        (i.e. mFDR ≤ α)

  using the *derived* pathwise budget `∑_{t∈S} α_t ω ≤ α · max(R(ω), 1)`
  (PathwiseBudget.lean, `lordThreshold_sum_le`) instead of an assumed one.
  This is the marginal-FDR guarantee; it holds with the reward term live.
  The stronger `FDR ≤ α` is the Ramdas et al. pen-and-paper result and is not
  re-proved here.
-/

import LordFDR.OnlineFDR
import LordFDR.PathwiseBudget

open MeasureTheory Filter

namespace LordFDR.MFDR

open LordFDR.OnlineFDR LordFDR.FundamentalLemma

variable {Ω : Type*} {mΩ : MeasurableSpace Ω} {μ : Measure Ω}

/-- Each rejection indicator `Rej (P t) (α t)` is integrable. -/
private lemma rej_integrable
    {P α : ℕ → Ω → ℝ} {F : ℕ → MeasurableSpace Ω}
    (hF : ∀ t, F t ≤ mΩ)
    (hPm : ∀ t, @Measurable Ω ℝ mΩ _ (P t))
    (hαp : ∀ t, @Measurable Ω ℝ (F t) _ (α (t + 1)))
    [IsProbabilityMeasure μ] (s : ℕ) :
    Integrable (Rej (P (s + 1)) (α (s + 1))) μ := by
  apply (integrable_const (1 : ℝ)).mono'
  · exact (Measurable.ite (measurableSet_le (hPm (s + 1)) ((hαp s).mono (hF s) le_rfl))
      measurable_const measurable_const).aestronglyMeasurable
  · exact Eventually.of_forall fun ω => by
      simp only [Rej, Real.norm_eq_abs]; split_ifs <;> norm_num

/-- `sumRej` over `S` is measurable (each summand is). -/
private lemma sumRej_measurable
    {P α : ℕ → Ω → ℝ} {F : ℕ → MeasurableSpace Ω}
    (hF : ∀ t, F t ≤ mΩ)
    (hPm : ∀ t, @Measurable Ω ℝ mΩ _ (P t))
    (hαp : ∀ t, @Measurable Ω ℝ (F t) _ (α (t + 1)))
    (S : Finset ℕ) (hS : ∀ t ∈ S, ∃ s, t = s + 1) :
    Measurable (sumRej P α S) := by
  unfold sumRej Rej
  apply Finset.measurable_sum
  intro t ht
  obtain ⟨s, hs⟩ := hS t ht; subst hs
  exact Measurable.ite (measurableSet_le (hPm (s + 1)) ((hαp s).mono (hF s) le_rfl))
    measurable_const measurable_const

/-- `0 ≤ sumRej ≤ |S|` pointwise, so `max (sumRej) 1` is bounded by `|S| + 1`. -/
private lemma sumRej_le_card {P α : ℕ → Ω → ℝ} (S : Finset ℕ) (ω : Ω) :
    sumRej P α S ω ≤ (S.card : ℝ) := by
  unfold sumRej
  calc ∑ t ∈ S, Rej (P t) (α t) ω
      ≤ ∑ _t ∈ S, (1 : ℝ) :=
        Finset.sum_le_sum (fun t _ => by simp only [Rej]; split_ifs <;> norm_num)
    _ = (S.card : ℝ) := by rw [Finset.sum_const, nsmul_eq_mul, mul_one]

/-- `max (sumRej) 1` is integrable: it is non-negative, measurable, and bounded. -/
private lemma max_sumRej_integrable
    {P α : ℕ → Ω → ℝ} {F : ℕ → MeasurableSpace Ω}
    (hF : ∀ t, F t ≤ mΩ)
    (hPm : ∀ t, @Measurable Ω ℝ mΩ _ (P t))
    (hαp : ∀ t, @Measurable Ω ℝ (F t) _ (α (t + 1)))
    [IsProbabilityMeasure μ]
    (S : Finset ℕ) (hS : ∀ t ∈ S, ∃ s, t = s + 1) :
    Integrable (fun ω => max (sumRej P α S ω) 1) μ := by
  have hmeas : Measurable (fun ω => max (sumRej P α S ω) 1) :=
    (sumRej_measurable hF hPm hαp S hS).max measurable_const
  apply (integrable_const ((S.card : ℝ) + 1)).mono'
    hmeas.aestronglyMeasurable
  refine Eventually.of_forall fun ω => ?_
  rw [Real.norm_eq_abs, abs_of_nonneg (le_trans zero_le_one (le_max_right _ _))]
  rcases le_or_lt (sumRej P α S ω) 1 with h | h
  · rw [max_eq_right h]
    have : (0 : ℝ) ≤ (S.card : ℝ) := Nat.cast_nonneg _
    linarith
  · rw [max_eq_left (le_of_lt h)]
    linarith [sumRej_le_card (P := P) (α := α) S ω]

/-- **Marginal FDR control for LORD++ (mFDR ≤ a).** Given the four structural
    conditions (via `expected_false_disc`) and the *pathwise* budget bound
    `∑_{t∈S} α_t ω ≤ a · max(R(ω), 1)` — which `lordThreshold_sum_le` supplies
    for the real LORD++ thresholds — the expected number of false discoveries
    satisfies `E[V] ≤ a · E[max(R, 1)]`. -/
theorem mfdr_le
    [IsProbabilityMeasure μ]
    {P α : ℕ → Ω → ℝ} {F : ℕ → MeasurableSpace Ω} {ε : ℝ}
    (hε : 0 < ε) (hF : ∀ t, F t ≤ mΩ)
    (hPm : ∀ t, @Measurable Ω ℝ mΩ _ (P t))
    (hU : ∀ t, IsUniformPValue μ (P t))
    (hαp : ∀ t, @Measurable Ω ℝ (F t) _ (α (t + 1)))
    (hαlo : ∀ t, ∀ᵐ ω ∂μ, ε ≤ α t ω)
    (hαhi : ∀ t, ∀ᵐ ω ∂μ, α t ω ≤ 1)
    (hI : ∀ t, IsIndepOfSubalgebra μ (P (t + 1)) (F t))
    (H₀ S : Finset ℕ)
    (hH₀ : ∀ t ∈ H₀, ∃ s, t = s + 1) (hS : ∀ t ∈ S, ∃ s, t = s + 1)
    (hH₀S : H₀ ⊆ S)
    {a : ℝ} (ha : 0 ≤ a)
    (hpath : ∀ᵐ ω ∂μ, ∑ t ∈ S, α t ω ≤ a * max (sumRej P α S ω) 1) :
    ∫ ω, sumRej P α H₀ ω ∂μ ≤ a * ∫ ω, max (sumRej P α S ω) 1 ∂μ := by
  -- Integrability of the per-step thresholds.
  have hα_int : ∀ s, Integrable (α (s + 1)) μ := by
    intro s
    apply (integrable_const (1 : ℝ)).mono'
    · exact ((hαp s).mono (hF s) le_rfl).aestronglyMeasurable
    · filter_upwards [hαlo (s + 1), hαhi (s + 1)] with ω h1 h2
      rw [Real.norm_eq_abs, abs_le]; exact ⟨by linarith [hε], h2⟩
  have hα_nn : ∀ s, 0 ≤ ∫ ω, α (s + 1) ω ∂μ := by
    intro s
    have hnn : ∀ᵐ ω ∂μ, (0 : ℝ) ≤ α (s + 1) ω := by
      filter_upwards [hαlo (s + 1)] with ω hω; linarith [hε]
    calc (0 : ℝ) = ∫ _, (0 : ℝ) ∂μ := by rw [integral_zero]
      _ ≤ ∫ ω, α (s + 1) ω ∂μ := integral_mono_ae (integrable_const _) (hα_int s) hnn
  have hsumα_int : Integrable (fun ω => ∑ t ∈ S, α t ω) μ :=
    integrable_finset_sum _ (fun t ht => by
      obtain ⟨s, hs⟩ := hS t ht; subst hs; exact hα_int s)
  have hmax_int := max_sumRej_integrable (μ := μ) (P := P) (α := α) hF hPm hαp S hS
  -- E[V] = Σ_{t∈H₀} E[α_t]
  rw [expected_false_disc hε hF hPm hU hαp hαlo hαhi hI H₀ hH₀]
  calc ∑ t ∈ H₀, ∫ ω, α t ω ∂μ
      ≤ ∑ t ∈ S, ∫ ω, α t ω ∂μ := by
        apply Finset.sum_le_sum_of_subset_of_nonneg hH₀S
        intro t ht _; obtain ⟨s, hs⟩ := hS t ht; subst hs; exact hα_nn s
    _ = ∫ ω, ∑ t ∈ S, α t ω ∂μ := by
        rw [integral_finset_sum _ (fun t ht => by
          obtain ⟨s, hs⟩ := hS t ht; subst hs; exact hα_int s)]
    _ ≤ ∫ ω, a * max (sumRej P α S ω) 1 ∂μ :=
        integral_mono_ae hsumα_int (hmax_int.const_mul a) hpath
    _ = a * ∫ ω, max (sumRej P α S ω) 1 ∂μ := integral_mul_left a _

/-- The LORD++ threshold is non-negative (used to extend a sum from `S` to a
    horizon `Icc 1 T`). -/
lemma lordThreshold_nonneg (cfg : LordConfig) (D : Finset ℕ) (t : ℕ) :
    0 ≤ lordThreshold cfg D t := by
  unfold lordThreshold
  refine add_nonneg (mul_nonneg (cfg.gamma_nonneg t) (le_of_lt cfg.w0_pos)) ?_
  exact mul_nonneg (by linarith [cfg.w0_lt_alpha])
    (Finset.sum_nonneg (fun τ _ => cfg.gamma_nonneg _))

/-- **Discharge of the pathwise hypothesis.** For a fixed realization `ω`, if
    the thresholds on `S` are the LORD++ thresholds of the rejections realized
    in `S`, then the pathwise budget `∑_{t∈S} α_t ω ≤ α · max(R(ω), 1)` holds —
    by `lordThreshold_sum_le`. The reward term is live; nothing is assumed about
    the wealth staying below `W₀`. -/
theorem pathwise_budget_realized
    {Ω : Type*} (cfg : LordConfig) (P α : ℕ → Ω → ℝ) (S : Finset ℕ) (ω : Ω)
    (T : ℕ) (hST : S ⊆ Finset.Icc 1 T)
    (hcons : ∀ t ∈ S, α t ω
        = lordThreshold cfg (S.filter (fun t => P t ω ≤ α t ω)) t) :
    ∑ t ∈ S, α t ω ≤ cfg.alpha * max (sumRej P α S ω) 1 := by
  set D := S.filter (fun t => P t ω ≤ α t ω) with hD
  have hcard : sumRej P α S ω = (D.card : ℝ) := by
    rw [hD]
    simp only [sumRej, Rej, Finset.card_filter, Nat.cast_sum]
    exact Finset.sum_congr rfl (fun t _ => by by_cases h : P t ω ≤ α t ω <;> simp [h])
  rw [Finset.sum_congr rfl hcons, hcard]
  calc ∑ t ∈ S, lordThreshold cfg D t
      ≤ ∑ t ∈ Finset.Icc 1 T, lordThreshold cfg D t :=
        Finset.sum_le_sum_of_subset_of_nonneg hST
          (fun t _ _ => lordThreshold_nonneg cfg D t)
    _ ≤ cfg.alpha * max (D.card : ℝ) 1 := lordThreshold_sum_le cfg D T

/-- **mFDR control for the real LORD++ procedure (capstone).** Under the four
    structural conditions and the fact that the thresholds are the LORD++
    thresholds of the realized rejections, the marginal FDR is controlled:
    `E[V] ≤ α · E[max(R, 1)]`. No pathwise budget is assumed — it is derived. -/
theorem lord_mfdr
    [IsProbabilityMeasure μ] (cfg : LordConfig)
    {P α : ℕ → Ω → ℝ} {F : ℕ → MeasurableSpace Ω} {ε : ℝ}
    (hε : 0 < ε) (hF : ∀ t, F t ≤ mΩ)
    (hPm : ∀ t, @Measurable Ω ℝ mΩ _ (P t))
    (hU : ∀ t, IsUniformPValue μ (P t))
    (hαp : ∀ t, @Measurable Ω ℝ (F t) _ (α (t + 1)))
    (hαlo : ∀ t, ∀ᵐ ω ∂μ, ε ≤ α t ω)
    (hαhi : ∀ t, ∀ᵐ ω ∂μ, α t ω ≤ 1)
    (hI : ∀ t, IsIndepOfSubalgebra μ (P (t + 1)) (F t))
    (H₀ S : Finset ℕ)
    (hH₀ : ∀ t ∈ H₀, ∃ s, t = s + 1) (hS : ∀ t ∈ S, ∃ s, t = s + 1)
    (hH₀S : H₀ ⊆ S)
    (T : ℕ) (hST : S ⊆ Finset.Icc 1 T)
    (hcons : ∀ᵐ ω ∂μ, ∀ t ∈ S, α t ω
        = lordThreshold cfg (S.filter (fun t => P t ω ≤ α t ω)) t) :
    ∫ ω, sumRej P α H₀ ω ∂μ ≤ cfg.alpha * ∫ ω, max (sumRej P α S ω) 1 ∂μ := by
  refine mfdr_le hε hF hPm hU hαp hαlo hαhi hI H₀ S hH₀ hS hH₀S
    (le_of_lt cfg.alpha_pos) ?_
  filter_upwards [hcons] with ω hω
  exact pathwise_budget_realized cfg P α S ω T hST hω

end LordFDR.MFDR
