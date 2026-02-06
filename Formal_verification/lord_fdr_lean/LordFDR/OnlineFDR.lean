/-
  LordFDR/OnlineFDR.lean — LORD++ Online FDR Control
  Built on FundamentalLemma.lean (0 sorry, 660 lines).
-/

import LordFDR.FundamentalLemma

open MeasureTheory Measure Filter
open scoped ENNReal NNReal

namespace LordFDR.OnlineFDR
open LordFDR.FundamentalLemma

/-! ## Part 1: E[R] = E[α] -/

variable {Ω : Type*} {m₀ mΩ : MeasurableSpace Ω} {μ : Measure Ω}

theorem expected_rejection
    [IsProbabilityMeasure μ]
    (hm : m₀ ≤ mΩ)
    {P α : Ω → ℝ} {ε : ℝ} (hε : 0 < ε)
    (hP_unif : IsUniformPValue μ P)
    (hP_meas : @Measurable Ω ℝ mΩ _ P)
    (hα_meas : @Measurable Ω ℝ m₀ _ α)
    (hα_bdd : ∀ᵐ ω ∂μ, ε ≤ α ω)
    (hα_le : ∀ᵐ ω ∂μ, α ω ≤ 1)
    (hP_indep : IsIndepOfSubalgebra μ P m₀) :
    ∫ ω, (if P ω ≤ α ω then (1 : ℝ) else 0) ∂μ = ∫ ω, α ω ∂μ := by
  haveI : IsFiniteMeasure (μ.trim hm) := isFiniteMeasure_trim hm
  have hα_mΩ : @Measurable Ω ℝ mΩ _ α := hα_meas.mono hm le_rfl
  set g : Ω → ℝ := fun ω => (if P ω ≤ α ω then (1 : ℝ) else 0) / α ω
  set R : Ω → ℝ := fun ω => if P ω ≤ α ω then (1 : ℝ) else 0
  have hg_int : Integrable g μ := integrable_indicator_div hm hε hP_meas hα_meas hα_bdd
  have hcg : condexp m₀ μ g =ᵐ[μ] fun _ => (1 : ℝ) :=
    fundamental_lemma_conditional hm hε hP_unif hP_meas hα_meas hα_bdd hα_le hP_indep
  have hRαg : R =ᵐ[μ] α * g := by
    filter_upwards [hα_bdd] with ω hle
    have hne : α ω ≠ 0 := ne_of_gt (lt_of_lt_of_le hε hle)
    show (if P ω ≤ α ω then (1 : ℝ) else 0) =
         α ω * ((if P ω ≤ α ω then 1 else 0) / α ω)
    rw [mul_div_cancel₀ _ hne]
  have hR_int : Integrable R μ := by
    apply (integrable_const (1 : ℝ)).mono'
    · exact (Measurable.ite (measurableSet_le hP_meas hα_mΩ)
        measurable_const measurable_const).aestronglyMeasurable
    · exact Eventually.of_forall fun ω => by simp only [R]; split_ifs <;> simp
  have hpull : condexp m₀ μ (α * g) =ᵐ[μ] α * condexp m₀ μ g :=
    condexp_stronglyMeasurable_mul hα_meas.stronglyMeasurable
      (hR_int.congr hRαg) hg_int
  have hcR : condexp m₀ μ R =ᵐ[μ] α := by
    -- Need explicit m₀ for condexp_congr_ae
    have h1 : condexp m₀ μ R =ᵐ[μ] condexp m₀ μ (α * g) :=
      @condexp_congr_ae Ω ℝ _ _ _ m₀ mΩ μ R (α * g) hRαg
    have h2 : (α * condexp m₀ μ g) =ᵐ[μ] α := by
      filter_upwards [hcg] with ω hω
      show α ω * condexp m₀ μ g ω = α ω; rw [hω, mul_one]
    exact h1.trans (hpull.trans h2)
  calc ∫ ω, R ω ∂μ
      = ∫ ω, (condexp m₀ μ R) ω ∂μ := (integral_condexp hm).symm
    _ = ∫ ω, α ω ∂μ := integral_congr_ae hcR

/-! ## Part 2: Definitions -/

noncomputable def Rej {Ω : Type*} (P α : Ω → ℝ) (ω : Ω) : ℝ :=
  if P ω ≤ α ω then 1 else 0

noncomputable def sumRej {Ω : Type*} (P α : ℕ → Ω → ℝ) (S : Finset ℕ) (ω : Ω) : ℝ :=
  ∑ t ∈ S, Rej (P t) (α t) ω

theorem sumRej_nonneg {Ω : Type*} (P α : ℕ → Ω → ℝ) (S : Finset ℕ) (ω : Ω) :
    0 ≤ sumRej P α S ω :=
  Finset.sum_nonneg (fun t _ => by simp only [Rej]; split_ifs <;> linarith)

/-! ## Part 3: E[V] = Σ E[α_t] -/

theorem expected_false_disc
    [IsProbabilityMeasure μ]
    {P α : ℕ → Ω → ℝ} {F : ℕ → MeasurableSpace Ω} {ε : ℝ}
    (hε : 0 < ε) (hF : ∀ t, F t ≤ mΩ)
    (hPm : ∀ t, @Measurable Ω ℝ mΩ _ (P t))
    (hU : ∀ t, IsUniformPValue μ (P t))
    (hαp : ∀ t, @Measurable Ω ℝ (F t) _ (α (t + 1)))
    (hαlo : ∀ t, ∀ᵐ ω ∂μ, ε ≤ α t ω)
    (hαhi : ∀ t, ∀ᵐ ω ∂μ, α t ω ≤ 1)
    (hI : ∀ t, IsIndepOfSubalgebra μ (P (t + 1)) (F t))
    (H₀ : Finset ℕ) (hH₀ : ∀ t ∈ H₀, ∃ s, t = s + 1) :
    ∫ ω, sumRej P α H₀ ω ∂μ = ∑ t ∈ H₀, ∫ ω, α t ω ∂μ := by
  have hrej_int : ∀ s, Integrable (Rej (P (s+1)) (α (s+1))) μ := by
    intro s
    apply (integrable_const (1:ℝ)).mono'
    · exact (Measurable.ite (measurableSet_le (hPm (s+1)) ((hαp s).mono (hF s) le_rfl))
        measurable_const measurable_const).aestronglyMeasurable
    · exact Eventually.of_forall fun ω => by
        simp only [Rej, Real.norm_eq_abs]; split_ifs <;> norm_num
  unfold sumRej
  rw [integral_finset_sum _ (fun t ht => by
    obtain ⟨s, hs⟩ := hH₀ t ht; subst hs; exact hrej_int s)]
  apply Finset.sum_congr rfl
  intro t ht
  obtain ⟨s, hs⟩ := hH₀ t ht; subst hs
  exact expected_rejection (hF s) hε (hU (s+1)) (hPm (s+1))
    (hαp s) (hαlo (s+1)) (hαhi (s+1)) (hI s)

/-! ## Part 4: E[V] ≤ q -/

theorem fdr_numerator_le
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
    {q : ℝ} (hbudget : ∀ᵐ ω ∂μ, ∑ t ∈ S, α t ω ≤ q) :
    ∫ ω, sumRej P α H₀ ω ∂μ ≤ q := by
  have hα_int : ∀ s, Integrable (α (s + 1)) μ := by
    intro s
    apply (integrable_const (1:ℝ)).mono'
    · exact ((hαp s).mono (hF s) le_rfl).aestronglyMeasurable
    · filter_upwards [hαlo (s+1), hαhi (s+1)] with ω h1 h2
      rw [Real.norm_eq_abs, abs_le]
      exact ⟨by linarith [hε], h2⟩
  have hα_nn : ∀ s, 0 ≤ ∫ ω, α (s+1) ω ∂μ := by
    intro s
    have : ∀ᵐ ω ∂μ, (0 : ℝ) ≤ α (s+1) ω := by
      filter_upwards [hαlo (s+1)] with ω (hω : ε ≤ α (s+1) ω)
      linarith [hε]
    calc (0 : ℝ) = ∫ _, (0 : ℝ) ∂μ := by rw [integral_zero]
      _ ≤ ∫ ω, α (s+1) ω ∂μ :=
          integral_mono_ae (integrable_const _) (hα_int s) this
  rw [expected_false_disc hε hF hPm hU hαp hαlo hαhi hI H₀ hH₀]
  calc ∑ t ∈ H₀, ∫ ω, α t ω ∂μ
      ≤ ∑ t ∈ S, ∫ ω, α t ω ∂μ := by
        apply Finset.sum_le_sum_of_subset_of_nonneg hH₀S
        intro t ht _
        obtain ⟨s, hs⟩ := hS t ht; subst hs
        exact hα_nn s
    _ = ∫ ω, ∑ t ∈ S, α t ω ∂μ := by
        rw [integral_finset_sum _ (fun t ht => by
          obtain ⟨s, hs⟩ := hS t ht; subst hs; exact hα_int s)]
    _ ≤ ∫ ω, q ∂μ :=
        integral_mono_ae
          (integrable_finset_sum _ (fun t ht => by
            obtain ⟨s, hs⟩ := hS t ht; subst hs; exact hα_int s))
          (integrable_const _) hbudget
    _ = q := by rw [integral_const, measure_univ, ENNReal.one_toReal, one_smul]

/-! ## Part 5: LORD++ FDR Theorem -/

theorem lord_fdr
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
    {q : ℝ} (hbudget : ∀ᵐ ω ∂μ, ∑ t ∈ S, α t ω ≤ q) :
    ∫ ω, sumRej P α H₀ ω / max (sumRej P α S ω) 1 ∂μ ≤ q := by
  have hrej_int : ∀ s, Integrable (Rej (P (s+1)) (α (s+1))) μ := by
    intro s
    apply (integrable_const (1:ℝ)).mono'
    · exact (Measurable.ite (measurableSet_le (hPm (s+1)) ((hαp s).mono (hF s) le_rfl))
        measurable_const measurable_const).aestronglyMeasurable
    · exact Eventually.of_forall fun ω => by
        simp only [Rej, Real.norm_eq_abs]; split_ifs <;> norm_num
  have hV_int : Integrable (sumRej P α H₀) μ := by
    unfold sumRej
    exact integrable_finset_sum _ (fun t ht => by
      obtain ⟨s, hs⟩ := hH₀ t ht; subst hs; exact hrej_int s)
  have hmax_pos : ∀ ω, (1 : ℝ) ≤ max (sumRej P α S ω) 1 :=
    fun ω => le_max_right _ _
  calc ∫ ω, sumRej P α H₀ ω / max (sumRej P α S ω) 1 ∂μ
      ≤ ∫ ω, sumRej P α H₀ ω ∂μ := by
        apply integral_mono_of_nonneg
        · exact Eventually.of_forall fun ω =>
            div_nonneg (sumRej_nonneg P α H₀ ω)
              (le_of_lt (lt_of_lt_of_le one_pos (hmax_pos ω)))
        · exact hV_int
        · exact Eventually.of_forall fun ω =>
            div_le_self (sumRej_nonneg P α H₀ ω) (hmax_pos ω)
    _ ≤ q := fdr_numerator_le hε hF hPm hU hαp hαlo hαhi hI H₀ S hH₀ hS hH₀S hbudget

end LordFDR.OnlineFDR
