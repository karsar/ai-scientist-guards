/-
  LordFDR/FDR.lean — machine-checked FDR ≤ α via leave-one-out.

  This proves the *full* FDR (not mFDR): `E[V / max(R,1)] ≤ α`, under explicit
  independence/predictability conditions:

    * each null p-value is uniform and independent of a "leave-t-out" σ-algebra
      G_t  (hU, hI);
    * the threshold α_t is G_t-measurable  (hαG, predictability); and
    * the leave-t-out rejection count R₋ₜ = sumRej (S.erase t) is G_t-measurable
      (hRerase) — i.e. removing test t does not change the others' decisions.

  The last condition is exactly what mutual independence with NON-ADAPTIVE
  thresholds provides — the regime the disjoint-split design (H3) creates. It is
  what fails for reward-bearing LORD++, whose rewards make future thresholds
  react to past rejections; that adaptive case keeps the cited Ramdas et al.
  monotone-coupling proof. The argument here reuses the pathwise budget
  (PathwiseBudget) and the fundamental lemma (FundamentalLemma).
-/

import LordFDR.OnlineFDR
import LordFDR.PathwiseBudget

open MeasureTheory Filter

namespace LordFDR.FDR

open LordFDR.OnlineFDR LordFDR.FundamentalLemma

variable {Ω : Type*} {m₀ mΩ : MeasurableSpace Ω} {μ : Measure Ω}

/-- Conditional super-uniformity in usable form: `E[Rej | G] = α` a.e. Identical
    derivation to `expected_rejection`, but exposing the conditional expectation
    against an arbitrary sub-σ-algebra `m₀` (here a leave-one-out algebra). -/
theorem condexp_rej_eq
    [IsProbabilityMeasure μ]
    (hm : m₀ ≤ mΩ)
    {P α : Ω → ℝ} {ε : ℝ} (hε : 0 < ε)
    (hP_unif : IsUniformPValue μ P)
    (hP_meas : @Measurable Ω ℝ mΩ _ P)
    (hα_meas : @Measurable Ω ℝ m₀ _ α)
    (hα_bdd : ∀ᵐ ω ∂μ, ε ≤ α ω)
    (hα_le : ∀ᵐ ω ∂μ, α ω ≤ 1)
    (hP_indep : IsIndepOfSubalgebra μ P m₀) :
    condexp m₀ μ (Rej P α) =ᵐ[μ] α := by
  have hα_mΩ : @Measurable Ω ℝ mΩ _ α := hα_meas.mono hm le_rfl
  set g : Ω → ℝ := fun ω => (if P ω ≤ α ω then (1 : ℝ) else 0) / α ω with hg
  have hg_int : Integrable g μ :=
    integrable_indicator_div hm hε hP_meas hα_meas hα_bdd
  have hcg : condexp m₀ μ g =ᵐ[μ] fun _ => (1 : ℝ) :=
    fundamental_lemma_conditional hm hε hP_unif hP_meas hα_meas hα_bdd hα_le hP_indep
  have hRαg : Rej P α =ᵐ[μ] α * g := by
    filter_upwards [hα_bdd] with ω hle
    have hne : α ω ≠ 0 := ne_of_gt (lt_of_lt_of_le hε hle)
    show (if P ω ≤ α ω then (1 : ℝ) else 0) =
         α ω * ((if P ω ≤ α ω then 1 else 0) / α ω)
    rw [mul_div_cancel₀ _ hne]
  have hR_int : Integrable (Rej P α) μ := by
    apply (integrable_const (1 : ℝ)).mono'
    · exact (Measurable.ite (measurableSet_le hP_meas hα_mΩ)
        measurable_const measurable_const).aestronglyMeasurable
    · exact Eventually.of_forall fun ω => by simp only [Rej]; split_ifs <;> simp
  have hpull : condexp m₀ μ (α * g) =ᵐ[μ] α * condexp m₀ μ g :=
    condexp_stronglyMeasurable_mul hα_meas.stronglyMeasurable
      (hR_int.congr hRαg) hg_int
  have h1 : condexp m₀ μ (Rej P α) =ᵐ[μ] condexp m₀ μ (α * g) :=
    @condexp_congr_ae Ω ℝ _ _ _ m₀ mΩ μ (Rej P α) (α * g) hRαg
  have h2 : (α * condexp m₀ μ g) =ᵐ[μ] α := by
    filter_upwards [hcg] with ω hω
    show α ω * condexp m₀ μ g ω = α ω; rw [hω, mul_one]
  exact h1.trans (hpull.trans h2)

/-- Weighted conditional identity: for a `G`-measurable weight `h`,
    `∫ h·Rej = ∫ h·α` when `E[Rej | G] = α`. -/
theorem integral_rej_weight
    [IsProbabilityMeasure μ]
    (hm : m₀ ≤ mΩ)
    {P α h : Ω → ℝ}
    (hcR : condexp m₀ μ (Rej P α) =ᵐ[μ] α)
    (hh : @Measurable Ω ℝ m₀ _ h)
    (hRej_int : Integrable (Rej P α) μ)
    (hprod_int : Integrable (fun ω => h ω * Rej P α ω) μ) :
    ∫ ω, h ω * Rej P α ω ∂μ = ∫ ω, h ω * α ω ∂μ := by
  have hmul : condexp m₀ μ (fun ω => h ω * Rej P α ω)
      =ᵐ[μ] fun ω => h ω * condexp m₀ μ (Rej P α) ω :=
    condexp_stronglyMeasurable_mul hh.stronglyMeasurable hprod_int hRej_int
  have hce : condexp m₀ μ (fun ω => h ω * Rej P α ω) =ᵐ[μ] fun ω => h ω * α ω := by
    filter_upwards [hmul, hcR] with ω e1 e2
    rw [e1]; show h ω * condexp m₀ μ (Rej P α) ω = h ω * α ω; rw [e2]
  calc ∫ ω, h ω * Rej P α ω ∂μ
      = ∫ ω, condexp m₀ μ (fun ω => h ω * Rej P α ω) ω ∂μ := (integral_condexp hm).symm
    _ = ∫ ω, h ω * α ω ∂μ := integral_congr_ae hce

/-- **Machine-checked FDR ≤ α via leave-one-out.** Under independence of each
    null p-value from a leave-one-out σ-algebra `G t`, predictability
    (`α t` is `G t`-measurable), and the non-adaptivity condition that the
    leave-`t`-out rejection count is `G t`-measurable, the false discovery rate
    `E[V / max(R,1)]` is bounded by the budget level `a`. The pathwise budget
    `∑_{t∈S} α_t ≤ a·max(R,1)` (PathwiseBudget) supplies the recombination. -/
theorem fdr_le
    [IsProbabilityMeasure μ]
    {P α : ℕ → Ω → ℝ} {G : ℕ → MeasurableSpace Ω} {ε : ℝ}
    (hε : 0 < ε)
    (hGle : ∀ t, G t ≤ mΩ)
    (hPm : ∀ t, @Measurable Ω ℝ mΩ _ (P t))
    (hU : ∀ t, IsUniformPValue μ (P t))
    (hαG : ∀ t, @Measurable Ω ℝ (G t) _ (α t))
    (hαlo : ∀ t, ∀ᵐ ω ∂μ, ε ≤ α t ω)
    (hαhi : ∀ t, ∀ᵐ ω ∂μ, α t ω ≤ 1)
    (hI : ∀ t, IsIndepOfSubalgebra μ (P t) (G t))
    (H₀ S : Finset ℕ) (hH₀S : H₀ ⊆ S)
    (hRerase : ∀ t ∈ H₀, @Measurable Ω ℝ (G t) _ (sumRej P α (S.erase t)))
    {a : ℝ}
    (hbudget : ∀ᵐ ω ∂μ, ∑ t ∈ S, α t ω ≤ a * max (sumRej P α S ω) 1) :
    ∫ ω, sumRej P α H₀ ω / max (sumRej P α S ω) 1 ∂μ ≤ a := by
  classical
  set D : Ω → ℝ := fun ω => max (sumRej P α S ω) 1 with hD
  set h : ℕ → Ω → ℝ := fun t ω => 1 / (sumRej P α (S.erase t) ω + 1) with hh
  -- measurability scaffolding
  have hαmΩ : ∀ t, @Measurable Ω ℝ mΩ _ (α t) := fun t => (hαG t).mono (hGle t) le_rfl
  have hRejmΩ : ∀ t, @Measurable Ω ℝ mΩ _ (Rej (P t) (α t)) := fun t =>
    Measurable.ite (measurableSet_le (hPm t) (hαmΩ t)) measurable_const measurable_const
  have hsum : ∀ (T : Finset ℕ), @Measurable Ω ℝ mΩ _ (sumRej P α T) := by
    intro T; unfold sumRej; exact Finset.measurable_sum _ (fun t _ => hRejmΩ t)
  have hDmeas : @Measurable Ω ℝ mΩ _ D := (hsum S).max measurable_const
  have herase_pos : ∀ t ω, (0 : ℝ) < sumRej P α (S.erase t) ω + 1 := fun t ω =>
    by linarith [sumRej_nonneg P α (S.erase t) ω]
  have hhmΩ : ∀ t, @Measurable Ω ℝ mΩ _ (h t) := fun t =>
    measurable_const.div ((hsum _).add measurable_const)
  have hh_mem : ∀ t ω, h t ω ∈ Set.Ioc (0 : ℝ) 1 := by
    intro t ω
    have hp := herase_pos t ω
    rw [hh, Set.mem_Ioc]
    exact ⟨div_pos one_pos hp,
      by rw [div_le_one hp]; linarith [sumRej_nonneg P α (S.erase t) ω]⟩
  -- pointwise: Rej_t / D = Rej_t * h t   (on S)
  have hrej_div : ∀ t ∈ S, ∀ ω, Rej (P t) (α t) ω / D ω = Rej (P t) (α t) ω * h t ω := by
    intro t ht ω
    have hsplit : sumRej P α S ω = Rej (P t) (α t) ω + sumRej P α (S.erase t) ω := by
      unfold sumRej
      exact (Finset.add_sum_erase S (fun u => Rej (P u) (α u) ω) ht).symm
    have hRe := sumRej_nonneg P α (S.erase t) ω
    simp only [hD, hh]
    by_cases hr : P t ω ≤ α t ω
    · have hRej1 : Rej (P t) (α t) ω = 1 := by simp [Rej, hr]
      have hDval : sumRej P α S ω ⊔ 1 = sumRej P α (S.erase t) ω + 1 := by
        rw [hsplit, hRej1, sup_eq_left.mpr (by linarith)]; ring
      rw [hRej1, hDval]; ring
    · have hRej0 : Rej (P t) (α t) ω = 0 := by simp [Rej, hr]
      rw [hRej0]; simp
  -- a.e. pointwise: α_t * h t ≤ α_t / D   (on S), using α_t ≥ 0
  have halpha_div : ∀ t ∈ S, ∀ᵐ ω ∂μ, α t ω * h t ω ≤ α t ω / D ω := by
    intro t ht
    filter_upwards [hαlo t] with ω hlo
    have hαnn : (0 : ℝ) ≤ α t ω := le_trans (le_of_lt hε) hlo
    have hsplit : sumRej P α S ω = Rej (P t) (α t) ω + sumRej P α (S.erase t) ω := by
      unfold sumRej
      exact (Finset.add_sum_erase S (fun u => Rej (P u) (α u) ω) ht).symm
    have hRe := sumRej_nonneg P α (S.erase t) ω
    have hDpos : (0 : ℝ) < D ω := lt_of_lt_of_le one_pos le_sup_right
    have hDle : D ω ≤ sumRej P α (S.erase t) ω + 1 := by
      simp only [hD]
      apply sup_le
      · rw [hsplit]
        have hr1 : Rej (P t) (α t) ω ≤ 1 := by simp only [Rej]; split_ifs <;> norm_num
        linarith
      · linarith
    have hone : 1 / (sumRej P α (S.erase t) ω + 1) ≤ 1 / D ω :=
      one_div_le_one_div_of_le hDpos hDle
    calc α t ω * h t ω
        = α t ω * (1 / (sumRej P α (S.erase t) ω + 1)) := by rw [hh]
      _ ≤ α t ω * (1 / D ω) := mul_le_mul_of_nonneg_left hone hαnn
      _ = α t ω / D ω := by rw [mul_one_div]
  -- integrability helpers
  have bdd_int : ∀ f : Ω → ℝ, @Measurable Ω ℝ mΩ _ f → (∀ᵐ ω ∂μ, |f ω| ≤ 1) →
      Integrable f μ := by
    intro f hf hb
    exact (integrable_const (1 : ℝ)).mono' hf.aestronglyMeasurable
      (by filter_upwards [hb] with ω hω; rwa [Real.norm_eq_abs])
  have hRejh_int : ∀ t, Integrable (fun ω => Rej (P t) (α t) ω * h t ω) μ := by
    intro t
    refine bdd_int _ ((hRejmΩ t).mul (hhmΩ t)) (Eventually.of_forall fun ω => ?_)
    have h1 : Rej (P t) (α t) ω ∈ Set.Icc (0:ℝ) 1 := by
      simp only [Rej]; split_ifs <;> norm_num
    have h2 := hh_mem t ω
    rw [abs_le]; constructor <;> nlinarith [h1.1, h1.2, h2.1, h2.2]
  have hαh_int : ∀ t, Integrable (fun ω => α t ω * h t ω) μ := by
    intro t
    refine bdd_int _ ((hαmΩ t).mul (hhmΩ t)) ?_
    filter_upwards [hαlo t, hαhi t] with ω hlo hhi
    have h2 := hh_mem t ω
    have hαnn : (0:ℝ) ≤ α t ω := le_trans (le_of_lt hε) hlo
    rw [abs_le]; constructor <;> nlinarith [h2.1, h2.2, hαnn, hhi]
  have hαD_int : ∀ t, Integrable (fun ω => α t ω / D ω) μ := by
    intro t
    refine bdd_int _ ((hαmΩ t).div hDmeas) ?_
    filter_upwards [hαlo t, hαhi t] with ω hlo hhi
    have hDpos : (0:ℝ) < D ω := lt_of_lt_of_le one_pos le_sup_right
    have hD1 : (1:ℝ) ≤ D ω := le_sup_right
    have hαnn : (0:ℝ) ≤ α t ω := le_trans (le_of_lt hε) hlo
    rw [abs_le]; constructor
    · apply le_trans (by norm_num : (-1:ℝ) ≤ 0); positivity
    · rw [div_le_one hDpos]; linarith
  have hsumαh_int : Integrable (fun ω => ∑ t ∈ H₀, α t ω * h t ω) μ :=
    integrable_finset_sum _ (fun t _ => hαh_int t)
  have hsumαD_int : Integrable (fun ω => ∑ t ∈ H₀, α t ω / D ω) μ :=
    integrable_finset_sum _ (fun t _ => hαD_int t)
  -- per-term: ∫ Rej_t/D = ∫ α_t * h t
  have hterm : ∀ t ∈ H₀, ∫ ω, Rej (P t) (α t) ω / D ω ∂μ = ∫ ω, α t ω * h t ω ∂μ := by
    intro t ht
    have hcR := condexp_rej_eq (hGle t) hε (hU t) (hPm t) (hαG t) (hαlo t) (hαhi t) (hI t)
    have hhG : @Measurable Ω ℝ (G t) _ (h t) :=
      measurable_const.div ((hRerase t ht).add measurable_const)
    have e1 : ∫ ω, Rej (P t) (α t) ω / D ω ∂μ = ∫ ω, h t ω * Rej (P t) (α t) ω ∂μ := by
      apply integral_congr_ae; filter_upwards with ω
      rw [hrej_div t (hH₀S ht) ω, mul_comm]
    have hRej_int : Integrable (Rej (P t) (α t)) μ := by
      refine bdd_int _ (hRejmΩ t) (Eventually.of_forall fun ω => ?_)
      simp only [Rej]; split_ifs <;> norm_num
    have hprod_int : Integrable (fun ω => h t ω * Rej (P t) (α t) ω) μ := by
      simp_rw [mul_comm (h t _)]; exact hRejh_int t
    rw [e1, integral_rej_weight (hGle t) hcR hhG hRej_int hprod_int]
    apply integral_congr_ae; filter_upwards with ω; rw [mul_comm]
  -- main chain
  calc ∫ ω, sumRej P α H₀ ω / D ω ∂μ
      = ∫ ω, ∑ t ∈ H₀, Rej (P t) (α t) ω / D ω ∂μ := by
        apply integral_congr_ae; filter_upwards with ω
        unfold sumRej; rw [Finset.sum_div]
    _ = ∑ t ∈ H₀, ∫ ω, Rej (P t) (α t) ω / D ω ∂μ := by
        rw [integral_finset_sum]
        intro t ht
        have : (fun ω => Rej (P t) (α t) ω / D ω)
            = (fun ω => Rej (P t) (α t) ω * h t ω) := by
          funext ω; rw [hrej_div t (hH₀S ht) ω]
        rw [this]; exact hRejh_int t
    _ = ∑ t ∈ H₀, ∫ ω, α t ω * h t ω ∂μ := Finset.sum_congr rfl hterm
    _ = ∫ ω, ∑ t ∈ H₀, α t ω * h t ω ∂μ := by
        rw [integral_finset_sum _ (fun t _ => hαh_int t)]
    _ ≤ ∫ ω, ∑ t ∈ H₀, α t ω / D ω ∂μ := by
        apply integral_mono_ae hsumαh_int hsumαD_int
        have hpt : ∀ᵐ ω ∂μ, ∀ t ∈ H₀, α t ω * h t ω ≤ α t ω / D ω :=
          (eventually_all_finset H₀).mpr (fun t ht => halpha_div t (hH₀S ht))
        filter_upwards [hpt] with ω hω
        exact Finset.sum_le_sum (fun t ht => hω t ht)
    _ = ∫ ω, (∑ t ∈ H₀, α t ω) / D ω ∂μ := by
        apply integral_congr_ae; filter_upwards with ω; rw [Finset.sum_div]
    _ ≤ ∫ ω, a ∂μ := by
        apply integral_mono_ae _ (integrable_const a)
        · filter_upwards [hbudget, ae_all_iff.mpr hαlo] with ω hbud hlo
          have hDpos : (0:ℝ) < D ω := lt_of_lt_of_le one_pos le_sup_right
          have hsub : ∑ t ∈ H₀, α t ω ≤ ∑ t ∈ S, α t ω :=
            Finset.sum_le_sum_of_subset_of_nonneg hH₀S
              (fun t _ _ => le_trans (le_of_lt hε) (hlo t))
          rw [div_le_iff₀ hDpos]
          exact le_trans hsub hbud
        · exact (integrable_finset_sum _ (fun t _ => hαD_int t)).congr
            (by filter_upwards with ω; rw [Finset.sum_div])
    _ = a := by rw [integral_const, measure_univ, ENNReal.one_toReal, one_smul]

end LordFDR.FDR
