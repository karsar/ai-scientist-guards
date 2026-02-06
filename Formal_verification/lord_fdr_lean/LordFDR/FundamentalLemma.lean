/-
  LordFDR/FundamentalLemma.lean (v27)

  The Fundamental Lemma for LORD++ FDR Control.

  ## Status — FULLY PROVED (0 sorry)
  - Steps 1–5 (constant threshold): PROVED ✓
  - Step 6 (integral identity, random α): PROVED ✓ (DCT + partition + independence)
  - Step 7 (integrability): PROVED ✓ (using ε lower bound)
  - Step 8 (conditional expectation): PROVED ✓ (from Steps 6–7)
  - h_P_ne_α (uniform has no atoms): PROVED ✓ (2/N → 0 argument)

  ## Key Design Decisions
  - Steps 1–5: single [MeasurableSpace Ω], no m₀
  - Steps 6–8: section with {m₀ mΩ : MeasurableSpace Ω} (Mathlib convention)
  - Added ε lower bound: ∀ᵐ ω, ε ≤ α ω (always holds in LORD++ where α
    comes from a deterministic positive γ-sequence)
  - This makes integrability trivial: |f| ≤ 1/ε on a probability space
-/

import LordFDR.Basic
import Mathlib.MeasureTheory.Integral.SetIntegral
import Mathlib.MeasureTheory.Integral.DominatedConvergence
import Mathlib.MeasureTheory.Function.ConditionalExpectation.Basic
import Mathlib.Topology.Algebra.Order.LiminfLimsup

open MeasureTheory
open scoped ENNReal NNReal

namespace LordFDR.FundamentalLemma

/-! ## Definitions -/

def IsUniformPValue {Ω : Type*} [MeasurableSpace Ω] (μ : Measure Ω) (P : Ω → ℝ) : Prop :=
  ∀ x : ℝ, 0 ≤ x → x ≤ 1 → μ {ω | P ω ≤ x} = ENNReal.ofReal x

def IsIndepOfSubalgebra {Ω : Type*} [MeasurableSpace Ω] (μ : Measure Ω) (P : Ω → ℝ)
    (m₀ : MeasurableSpace Ω) : Prop :=
  ∀ (A : Set Ω) (B : Set ℝ), @MeasurableSet Ω m₀ A → MeasurableSet B →
    μ (A ∩ P ⁻¹' B) = μ A * μ (P ⁻¹' B)

def SetIndepOf {Ω : Type*} [MeasurableSpace Ω] (μ : Measure Ω) (P : Ω → ℝ)
    (A : Set Ω) : Prop :=
  ∀ B : Set ℝ, MeasurableSet B → μ (A ∩ P ⁻¹' B) = μ A * μ (P ⁻¹' B)


/-! ================================================================
    STEPS 1–5: PROVED (single MeasurableSpace, no m₀)
    ================================================================ -/

theorem measure_inter_eq
    {Ω : Type*} [MeasurableSpace Ω] {μ : Measure Ω}
    {P : Ω → ℝ} (hP_unif : IsUniformPValue μ P)
    {A : Set Ω} (hIndepA : SetIndepOf μ P A)
    {c : ℝ} (hc_nonneg : 0 ≤ c) (hc_le_one : c ≤ 1) :
    μ (A ∩ {ω | P ω ≤ c}) = μ A * ENNReal.ofReal c := by
  have h1 : μ (A ∩ {ω | P ω ≤ c}) = μ A * μ {ω | P ω ≤ c} := by
    have hset : {ω | P ω ≤ c} = P ⁻¹' Set.Iic c := rfl
    rw [hset]
    exact hIndepA (Set.Iic c) measurableSet_Iic
  rw [h1, hP_unif c hc_nonneg hc_le_one]

theorem setIntegral_indicator_const
    {Ω : Type*} [MeasurableSpace Ω] {μ : Measure Ω}
    {P : Ω → ℝ} (hP_meas : Measurable P)
    {A : Set Ω} {c : ℝ} (hc_nonneg : 0 ≤ c)
    (hMeas : μ (A ∩ {ω | P ω ≤ c}) = μ A * ENNReal.ofReal c) :
    ∫ ω in A, (if P ω ≤ c then (1 : ℝ) else 0) ∂μ = (μ A).toReal * c := by
  have hS : MeasurableSet {ω | P ω ≤ c} := hP_meas measurableSet_Iic
  suffices hsuff : ∫ ω in A, (if P ω ≤ c then (1 : ℝ) else 0) ∂μ =
      (μ (A ∩ {ω | P ω ≤ c})).toReal by
    rw [hsuff, hMeas, ENNReal.toReal_mul, ENNReal.toReal_ofReal hc_nonneg]
  have h_pw : ∀ ω, (if P ω ≤ c then (1 : ℝ) else 0) =
      Set.indicator {ω | P ω ≤ c} (fun _ => (1 : ℝ)) ω := by
    intro ω; simp [Set.indicator]
  simp_rw [h_pw]
  simp only [integral_indicator hS, setIntegral_const, smul_eq_mul, mul_one,
             Measure.restrict_apply hS]
  rw [Set.inter_comm]

theorem setIntegral_indicator_div_const
    {Ω : Type*} [MeasurableSpace Ω] {μ : Measure Ω} [IsProbabilityMeasure μ]
    {P : Ω → ℝ} (hP_unif : IsUniformPValue μ P) (hP_meas : Measurable P)
    {A : Set Ω} (hIndepA : SetIndepOf μ P A)
    {c : ℝ} (hc_pos : 0 < c) (hc_le_one : c ≤ 1) :
    ∫ ω in A, (if P ω ≤ c then (1 : ℝ) else 0) / c ∂μ = (μ A).toReal := by
  have h_factor : ∫ ω in A, (if P ω ≤ c then (1 : ℝ) else 0) / c ∂μ =
      (1/c) * ∫ ω in A, (if P ω ≤ c then (1 : ℝ) else 0) ∂μ := by
    rw [← integral_mul_left]; congr 1; ext ω; ring
  have hMeasId := measure_inter_eq hP_unif hIndepA (le_of_lt hc_pos) hc_le_one
  rw [h_factor, setIntegral_indicator_const hP_meas (le_of_lt hc_pos) hMeasId]
  field_simp

theorem fundamental_lemma_const
    {Ω : Type*} [MeasurableSpace Ω] {μ : Measure Ω} [IsProbabilityMeasure μ]
    {P : Ω → ℝ} (hP_unif : IsUniformPValue μ P) (hP_meas : Measurable P)
    {α : ℝ} (hα_pos : 0 < α) (hα_le_one : α ≤ 1) :
    ∫ ω, (if P ω ≤ α then (1 : ℝ) else 0) / α ∂μ = 1 := by
  have h_factor : ∫ ω, (if P ω ≤ α then (1 : ℝ) else 0) / α ∂μ =
      (1/α) * ∫ ω, (if P ω ≤ α then (1 : ℝ) else 0) ∂μ := by
    rw [← integral_mul_left]; congr 1; ext ω; ring
  rw [h_factor]
  have hS : MeasurableSet {ω | P ω ≤ α} := hP_meas measurableSet_Iic
  have h_pw : ∀ ω, (if P ω ≤ α then (1 : ℝ) else 0) =
      Set.indicator {ω | P ω ≤ α} (fun _ => (1 : ℝ)) ω := by
    intro ω; simp [Set.indicator]
  simp_rw [h_pw]
  simp only [integral_indicator hS, setIntegral_const, smul_eq_mul, mul_one]
  rw [hP_unif α (le_of_lt hα_pos) hα_le_one, ENNReal.toReal_ofReal (le_of_lt hα_pos)]
  field_simp


/-! ================================================================
    STEPS 6–8: CONDITIONAL VERSION

    Variable convention: {m₀ mΩ} with mΩ last → instance synthesis picks mΩ.
    Added hypothesis: ε lower bound on α (sufficient for LORD++).
    ================================================================ -/

section ConditionalVersion

variable {Ω : Type*} {m₀ mΩ : MeasurableSpace Ω} {μ : Measure Ω}

/-! ### Utility: derive SetIndepOf from IsIndepOfSubalgebra -/

theorem setIndepOf_of_isIndep
    {P : Ω → ℝ}
    (hP_indep : IsIndepOfSubalgebra μ P m₀)
    {A : Set Ω} (hA : @MeasurableSet Ω m₀ A) :
    SetIndepOf μ P A :=
  fun B hB => hP_indep A B hA hB


/-! ### Step 7: Integrability (PROVED)

    With α ≥ ε > 0 a.e., the function f = 𝟙{P ≤ α}/α is bounded by 1/ε.
    A bounded measurable function on a finite measure space is integrable. -/

theorem integrable_indicator_div
    [IsProbabilityMeasure μ]
    (hm : m₀ ≤ mΩ)
    {P α : Ω → ℝ} {ε : ℝ} (hε : 0 < ε)
    (hP_meas : Measurable P)
    (hα_meas : @Measurable Ω ℝ m₀ _ α)
    (hα_bdd : ∀ᵐ ω ∂μ, ε ≤ α ω) :
    Integrable (fun ω => (if P ω ≤ α ω then (1 : ℝ) else 0) / α ω) μ := by
  -- α is measurable w.r.t. mΩ (from m₀-measurability + m₀ ≤ mΩ)
  have hα_mΩ : Measurable α := hα_meas.mono hm le_rfl
  -- f is AEStronglyMeasurable
  have hf_aesm : AEStronglyMeasurable
      (fun ω => (if P ω ≤ α ω then (1 : ℝ) else 0) / α ω) μ :=
    ((Measurable.ite (measurableSet_le hP_meas hα_mΩ)
      measurable_const measurable_const).div hα_mΩ).aestronglyMeasurable
  -- Strategy: f is bounded by 1/ε a.e., use Integrable.mono with constant function
  refine (integrable_const (1 / ε)).mono hf_aesm ?_
  -- Show: ‖f ω‖ ≤ ‖1/ε‖ a.e.
  filter_upwards [hα_bdd] with ω hω
  have hα_pos : 0 < α ω := lt_of_lt_of_le hε hω
  -- f ω ≥ 0 and 1/ε ≥ 0, so norms equal values
  have hf_nonneg : 0 ≤ (if P ω ≤ α ω then (1 : ℝ) else 0) / α ω :=
    div_nonneg (by split_ifs <;> norm_num) (le_of_lt hα_pos)
  have hε_nonneg : (0 : ℝ) ≤ 1 / ε := by positivity
  rw [Real.norm_eq_abs, abs_of_nonneg hf_nonneg,
      Real.norm_eq_abs, abs_of_nonneg hε_nonneg]
  -- Goal: (if ... then 1 else 0) / α ω ≤ 1 / ε
  -- Cross-multiply (both denominators positive): num * ε ≤ 1 * α ω
  rw [div_le_div_iff₀ hα_pos hε]
  have h_num : (if P ω ≤ α ω then (1 : ℝ) else 0) ≤ 1 := by split_ifs <;> norm_num
  -- num * ε ≤ 1 * ε ≤ 1 * α ω, handled by nlinarith
  nlinarith [mul_le_mul_of_nonneg_right h_num (le_of_lt hε)]


/-! ### Helper: Nat.floor is measurable -/

private lemma nat_floor_preimage_zero :
    (fun a : ℝ => ⌊a⌋₊) ⁻¹' {0} = Set.Iio 1 := by
  ext a; simp [Nat.floor_eq_zero]

private lemma nat_floor_preimage_pos {m : ℕ} (hm : 0 < m) :
    (fun a : ℝ => ⌊a⌋₊) ⁻¹' {m} = Set.Ico (m : ℝ) ((m : ℝ) + 1) := by
  ext a
  simp only [Set.mem_preimage, Set.mem_singleton_iff, Set.mem_Ico]
  have hm_ne : m ≠ 0 := Nat.pos_iff_ne_zero.mp hm
  constructor
  · intro h
    have ha_nn : (0 : ℝ) ≤ a :=
      le_of_lt (lt_of_lt_of_le one_pos (le_of_not_lt
        (fun h1 => hm_ne (h.symm ▸ Nat.floor_eq_zero.mpr h1))))
    exact (Nat.floor_eq_iff ha_nn).mp h
  · intro ⟨h1, h2⟩
    exact (Nat.floor_eq_iff (le_trans (Nat.cast_nonneg m) h1)).mpr ⟨h1, h2⟩

private lemma measurable_nat_floor : Measurable (fun a : ℝ => ⌊a⌋₊) := by
  apply measurable_to_countable; intro x
  by_cases hm : ⌊x⌋₊ = 0
  · simp only [hm, nat_floor_preimage_zero]; exact measurableSet_Iio
  · rw [nat_floor_preimage_pos (Nat.pos_of_ne_zero hm)]; exact measurableSet_Ico

/-! ### Helper: Dyadic approximation -/

private lemma tendsto_inv_two_pow :
    Filter.Tendsto (fun n : ℕ => (1 : ℝ) / (2 : ℝ) ^ n) Filter.atTop (nhds 0) := by
  have h := tendsto_pow_atTop_nhds_zero_of_lt_one
    (by positivity : (0:ℝ) ≤ 1/2) (by norm_num : (1:ℝ)/2 < 1)
  simp_rw [one_div] at h ⊢
  exact h.congr (fun n => by rw [inv_pow])

private lemma tendsto_nat_floor_div_pow (x : ℝ) (hx : 0 ≤ x) :
    Filter.Tendsto (fun n : ℕ => (↑(⌊(2:ℝ)^n * x⌋₊) : ℝ) / (2:ℝ)^n)
      Filter.atTop (nhds x) := by
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le
  · suffices h : Filter.Tendsto (fun n : ℕ => x - 1 / (2:ℝ)^n)
        Filter.atTop (nhds (x - 0)) by rwa [sub_zero] at h
    exact tendsto_const_nhds.sub tendsto_inv_two_pow
  · exact tendsto_const_nhds
  · intro n
    have h2n : (0:ℝ) < 2^n := by positivity
    rw [le_div_iff₀ h2n]
    have := @Nat.lt_floor_add_one ℝ _ _ ((2:ℝ)^n * x)
    linarith [mul_comm x ((2:ℝ)^n), mul_div_cancel₀ (1:ℝ) (ne_of_gt h2n)]
  · intro n
    have h2n : (0:ℝ) < 2^n := by positivity
    rw [div_le_iff₀ h2n]
    have : (↑(⌊(2:ℝ)^n * x⌋₊) : ℝ) ≤ (2:ℝ)^n * x :=
      Nat.floor_le (mul_nonneg (le_of_lt h2n) hx)
    linarith

/-! ### Ioc measure bound for uniform p-values -/

private lemma ioc_measure_bound {Ω : Type*} {mΩ : MeasurableSpace Ω} {μ : Measure Ω}
    [IsProbabilityMeasure μ]
    {P : Ω → ℝ} (hU : IsUniformPValue μ P) (hP : @Measurable Ω ℝ mΩ _ P)
    {a b : ℝ} (hab : a ≤ b) :
    μ (P ⁻¹' Set.Ioc a b) ≤ ENNReal.ofReal (b - a) := by
  have h_eq : P ⁻¹' Set.Ioc a b = {ω | P ω ≤ b} \ {ω | P ω ≤ a} := by
    ext ω
    simp only [Set.mem_preimage, Set.mem_Ioc, Set.mem_diff, Set.mem_setOf_eq, not_le]
    exact ⟨fun ⟨h1, h2⟩ => ⟨h2, h1⟩, fun ⟨h1, h2⟩ => ⟨h2, h1⟩⟩
  rw [h_eq]
  have hsub : {ω | P ω ≤ a} ⊆ {ω | P ω ≤ b} := fun ω h => le_trans h hab
  have hma : MeasurableSet {ω | P ω ≤ a} := hP measurableSet_Iic
  rw [measure_diff hsub hma.nullMeasurableSet
    (ne_top_of_le_ne_top (ne_of_lt (measure_lt_top μ _)) (measure_mono hsub))]
  by_cases ha0 : a < 0
  · have : μ {ω | P ω ≤ a} = 0 := le_antisymm
      (calc μ {ω | P ω ≤ a} ≤ μ {ω | P ω ≤ 0} :=
            measure_mono (fun ω h => le_trans h (le_of_lt ha0))
        _ = 0 := by rw [hU 0 le_rfl zero_le_one, ENNReal.ofReal_zero]) (zero_le _)
    rw [this, tsub_zero]
    by_cases hba : 1 ≤ b - a
    · calc μ {ω | P ω ≤ b}
          ≤ μ Set.univ := measure_mono (Set.subset_univ _)
        _ = 1 := measure_univ
        _ = ENNReal.ofReal 1 := ENNReal.ofReal_one.symm
        _ ≤ ENNReal.ofReal (b - a) := ENNReal.ofReal_le_ofReal hba
    · push_neg at hba
      by_cases hb0 : b < 0
      · have : μ {ω | P ω ≤ b} = 0 := le_antisymm
          (calc μ {ω | P ω ≤ b} ≤ μ {ω | P ω ≤ 0} :=
                measure_mono (fun ω h => le_trans h (le_of_lt hb0))
            _ = 0 := by rw [hU 0 le_rfl zero_le_one, ENNReal.ofReal_zero]) (zero_le _)
        rw [this]; exact zero_le _
      · push_neg at hb0
        rw [hU b hb0 (by linarith)]
        exact ENNReal.ofReal_le_ofReal (by linarith)
  · push_neg at ha0
    by_cases hb1 : b ≤ 1
    · rw [hU b (le_trans ha0 hab) hb1, hU a ha0 (le_trans hab hb1),
        ← ENNReal.ofReal_sub b ha0]
    · push_neg at hb1
      by_cases ha1 : a ≤ 1
      · have hb : μ {ω | P ω ≤ b} = 1 := le_antisymm
          ((measure_mono (Set.subset_univ _)).trans_eq measure_univ)
          (by calc 1 = ENNReal.ofReal 1 := ENNReal.ofReal_one.symm
              _ = μ {ω | P ω ≤ 1} := (hU 1 zero_le_one le_rfl).symm
              _ ≤ μ {ω | P ω ≤ b} := measure_mono (fun ω h => le_trans h (le_of_lt hb1)))
        rw [hb, hU a ha0 ha1]
        rw [show (1 : ℝ≥0∞) = ENNReal.ofReal 1 from ENNReal.ofReal_one.symm,
          ← ENNReal.ofReal_sub 1 ha0]
        exact ENNReal.ofReal_le_ofReal (by linarith)
      · push_neg at ha1
        have hb : μ {ω | P ω ≤ b} = 1 := le_antisymm
          ((measure_mono (Set.subset_univ _)).trans_eq measure_univ)
          (by calc 1 = μ {ω | P ω ≤ 1} := by rw [hU 1 zero_le_one le_rfl, ENNReal.ofReal_one]
              _ ≤ μ {ω | P ω ≤ b} := measure_mono (fun ω h => le_trans h (le_of_lt hb1)))
        have ha : μ {ω | P ω ≤ a} = 1 := le_antisymm
          ((measure_mono (Set.subset_univ _)).trans_eq measure_univ)
          (by calc 1 = μ {ω | P ω ≤ 1} := by rw [hU 1 zero_le_one le_rfl, ENNReal.ofReal_one]
              _ ≤ μ {ω | P ω ≤ a} := measure_mono (fun ω h => le_trans h (le_of_lt ha1)))
        rw [hb, ha, tsub_self]; exact zero_le _

private lemma tendsto_ofReal_two_div :
    Filter.Tendsto (fun N : ℕ => ENNReal.ofReal (2 / (↑N + 1))) Filter.atTop (nhds 0) := by
  rw [← ENNReal.ofReal_zero]
  apply ENNReal.tendsto_ofReal
  have h_inv : Filter.Tendsto (fun N : ℕ => ((↑N : ℝ) + 1)⁻¹) Filter.atTop (nhds 0) :=
    Filter.Tendsto.inv_tendsto_atTop
      (tendsto_natCast_atTop_atTop.atTop_add (tendsto_const_nhds (x := (1:ℝ))))
  have h := (tendsto_const_nhds (x := (2:ℝ))).mul h_inv
  simp only [mul_zero] at h
  refine h.congr (fun N => ?_)
  show 2 * ((↑N : ℝ) + 1)⁻¹ = 2 / (↑N + 1)
  rw [div_eq_mul_inv]

/-! ### Step 6: Set integral identity — FULLY PROVED

    ∫_A 𝟙{P ≤ α(ω)}/α(ω) dμ = μ(A).toReal for m₀-measurable A.

    ## PROOF PLAN

    ### Construction
    For each n : ℕ, define the approximation:
      sₙ(ω) = max(ε, ⌊2ⁿ · α(ω)⌋ / 2ⁿ)

    Properties:
    - sₙ is m₀-measurable (α is m₀-measurable, floor/max are measurable)
    - sₙ takes finitely many values in {max(ε, k/2ⁿ) : k = 0, ..., 2ⁿ}
    - ε ≤ sₙ(ω) ≤ α(ω) for all ω (by definition of floor + max)
    - sₙ(ω) → α(ω) pointwise (error ≤ 1/2ⁿ)

    ### Simple function case
    For each n, let fₙ(ω) = 𝟙{P(ω) ≤ sₙ(ω)}/sₙ(ω).

    Partition Ω into level sets Bₖⁿ = {ω | sₙ(ω) = cₖ} for the finite
    set of values {cₖ}. Each Bₖⁿ is m₀-measurable.

    On A ∩ Bₖⁿ: fₙ(ω) = 𝟙{P(ω) ≤ cₖ}/cₖ. Since A and Bₖⁿ are both
    m₀-measurable, A ∩ Bₖⁿ is m₀-measurable, hence independent of P.
    By setIntegral_indicator_div_const (Step 4):
      ∫_{A ∩ Bₖⁿ} fₙ dμ = μ(A ∩ Bₖⁿ).toReal

    Summing: ∫_A fₙ dμ = Σₖ μ(A ∩ Bₖⁿ).toReal = μ(A).toReal.

    ### Limit passage (DCT)
    - Pointwise: fₙ(ω) → f(ω) a.e.
      (sₙ → α, so 𝟙{P ≤ sₙ} → 𝟙{P ≤ α} a.e. and 1/sₙ → 1/α)
      (The a.e. comes from: {ω | P(ω) = α(ω)} has μ-measure 0 by
       independence + uniformity: for any measurable partition of α-values,
       P({P = c}) = 0 for each constant c.)
    - Domination: |fₙ(ω)| = 𝟙{P ≤ sₙ}/sₙ ≤ 1/sₙ ≤ 1/ε
      (since sₙ ≥ ε by construction)
    - Dominator 1/ε is integrable (constant on probability space)

    By tendsto_integral_of_dominated_convergence:
      ∫_A f dμ = lim ∫_A fₙ dμ = μ(A).toReal.  QED

    ### Mathlib tools needed
    - `Nat.floor` or `Int.floor` for constructing sₙ
    - `Finset.sum_congr` for the partition sum
    - `setIntegral_biUnion_finset` for integral over disjoint union
    - `tendsto_integral_of_dominated_convergence` for DCT
    - `Measure.sum_toReal_of_pairwiseDisjoint` or addivity for Σ μ(Bₖ) = μ(⊔Bₖ) -/

theorem setIntegral_indicator_div
    [IsProbabilityMeasure μ]
    (hm : m₀ ≤ mΩ)
    {P α : Ω → ℝ} {ε : ℝ} (hε : 0 < ε)
    (hP_unif : IsUniformPValue μ P)
    (hP_meas : Measurable P)
    (hα_meas : @Measurable Ω ℝ m₀ _ α)
    (hα_bdd : ∀ᵐ ω ∂μ, ε ≤ α ω)
    (hα_le_one : ∀ᵐ ω ∂μ, α ω ≤ 1)
    (hP_indep : IsIndepOfSubalgebra μ P m₀)
    {A : Set Ω} (hA : @MeasurableSet Ω m₀ A) (hμA : μ A < ⊤) :
    ∫ ω in A, (if P ω ≤ α ω then (1 : ℝ) else 0) / α ω ∂μ = (μ A).toReal := by
  have hα_mΩ : Measurable α := hα_meas.mono hm le_rfl
  have hA_mΩ : MeasurableSet A := hm _ hA
  /- PROOF: Simple function approximation + DCT.
     Define sₙ(ω) = max(ε, ⌊2ⁿ·α(ω)⌋₊ / 2ⁿ), Fₙ(ω) = 𝟙{P ≤ sₙ}/sₙ.
     (A) ∀ n, ∫_A Fₙ = μ(A).toReal  (simple function decomposition + Step 4)
     (B) Fₙ → f pointwise a.e.       (s → α, indicator a.e. continuous)
     (C) |Fₙ| ≤ 1/ε a.e.            (s ≥ ε)
     (D) Fₙ AEStronglyMeasurable     (measurability of compositions)
     By DCT + limit uniqueness: ∫_A f = μ(A).toReal. -/
  -- Define approximation
  set s : ℕ → Ω → ℝ := fun n ω =>
    max ε (↑(⌊(2 : ℝ) ^ n * α ω⌋₊) / (2 : ℝ) ^ n) with hs_def
  set F : ℕ → Ω → ℝ := fun n ω =>
    (if P ω ≤ s n ω then (1 : ℝ) else 0) / s n ω with hF_def
  -- s n ω ≥ ε (from max)
  have hs_ge : ∀ n ω, ε ≤ s n ω := fun n ω => le_max_left ε _
  have hs_pos : ∀ n ω, 0 < s n ω := fun n ω => lt_of_lt_of_le hε (hs_ge n ω)
  -- s n ω ≤ α ω always (floor ≤ original)
  have hs_le_α : ∀ n ω, ε ≤ α ω → s n ω ≤ α ω := by
    intro n ω hαω
    simp only [s]
    have h2n : (0:ℝ) < 2^n := by positivity
    apply max_le hαω
    rw [div_le_iff₀ h2n]
    have : (↑(⌊(2:ℝ)^n * α ω⌋₊) : ℝ) ≤ (2:ℝ)^n * α ω :=
      Nat.floor_le (mul_nonneg (le_of_lt h2n) (le_trans (le_of_lt hε) hαω))
    linarith
  -- (A) Each ∫_A F n = μ(A).toReal
  have h_each : ∀ n, ∫ ω in A, F n ω ∂μ = (μ A).toReal := by
    intro n
    -- Define level sets: B m = A ∩ {⌊2^n α⌋₊ = m}
    set g : Ω → ℕ := fun ω => ⌊(2 : ℝ) ^ n * α ω⌋₊ with hg_def
    set B : ℕ → Set Ω := fun m => A ∩ {ω | g ω = m} with hB_def
    -- g is measurable (using mΩ)
    have hg_meas : Measurable g := measurable_nat_floor.comp (hα_mΩ.const_mul _)
    -- g is m₀-measurable
    have hg_m0 : @Measurable Ω ℕ m₀ _ g :=
      measurable_nat_floor.comp (hα_meas.const_mul _)
    -- B m is m₀-measurable
    have hB_m0 : ∀ m, @MeasurableSet Ω m₀ (B m) :=
      fun m => @MeasurableSet.inter Ω m₀ _ _ hA (hg_m0 (measurableSet_singleton m))
    -- B m is mΩ-measurable
    have hB_meas : ∀ m, MeasurableSet (B m) :=
      fun m => hA_mΩ.inter (hg_meas (measurableSet_singleton m))
    -- B m pairwise disjoint
    have hB_disj : Pairwise (Disjoint on B) := by
      intro i j hij
      simp only [Function.onFun, B, Set.disjoint_left, Set.mem_inter_iff, Set.mem_setOf_eq]
      intro ω ⟨_, hi⟩ ⟨_, hj⟩; exact hij (hi.symm.trans hj)
    -- A = ⋃ m, B m
    have hA_cover : A = ⋃ m, B m := by
      ext ω; constructor
      · intro hω; exact Set.mem_iUnion.mpr ⟨g ω, hω, rfl⟩
      · intro hω; obtain ⟨_, hω, _⟩ := Set.mem_iUnion.mp hω; exact hω
    -- F n is integrableOn A (bounded by 1/ε on finite measure set)
    have hF_int : IntegrableOn (F n) A μ := by
      apply Integrable.mono' (integrable_const (1/ε)).integrableOn
      · exact ((Measurable.ite (measurableSet_le hP_meas
            (((measurable_from_top.comp hg_meas).div_const _).const_sup _))
          measurable_const measurable_const).div
          (((measurable_from_top.comp hg_meas).div_const _).const_sup _)).aestronglyMeasurable
      · filter_upwards with ω
        simp only [F, Real.norm_eq_abs]
        rw [abs_of_nonneg (div_nonneg (by split_ifs <;> norm_num) (le_of_lt (hs_pos n ω)))]
        rw [div_le_div_iff₀ (hs_pos n ω) hε]
        have : (if P ω ≤ s n ω then (1 : ℝ) else 0) ≤ 1 := by split_ifs <;> norm_num
        nlinarith [hs_ge n ω, mul_le_mul_of_nonneg_right this (le_of_lt hε)]
    -- Step 1: ∫_A F n = ∑' m, ∫_{B m} F n
    conv_lhs => rw [hA_cover]
    rw [integral_iUnion hB_meas hB_disj (hA_cover ▸ hF_int)]
    -- Step 2: on B m, s n ω = max(ε, m/2^n) (constant), so F n = 𝟙{P ≤ c_m}/c_m
    -- Step 3: ∫_{B m} 𝟙{P ≤ c_m}/c_m = μ(B m).toReal  (by Step 4)
    -- Step 4: ∑' m, μ(B m).toReal = μ(A).toReal
    -- Combine steps 2-3: ∫_{B m} F n = μ(B m).toReal
    have h_piece : ∀ m, ∫ ω in B m, F n ω ∂μ = (μ (B m)).toReal := by
      intro m
      -- On B m: g ω = m, so s n ω = max(ε, m/2^n) =: c_m
      set c_m : ℝ := max ε (↑m / (2 : ℝ) ^ n) with hc_m_def
      have hc_m_pos : 0 < c_m := lt_of_lt_of_le hε (le_max_left _ _)
      -- On B m, s n ω = c_m (since ⌊2^n α ω⌋₊ = m)
      have h_s_eq : ∀ ω ∈ B m, s n ω = c_m := by
        intro ω ⟨_, hgm⟩
        simp only [s, B, g, c_m, Set.mem_setOf_eq] at hgm ⊢
        rw [hgm]
      -- Replace F n with 𝟙{P ≤ c_m}/c_m on B m
      have h_eq : Set.EqOn (F n) (fun ω => (if P ω ≤ c_m then (1:ℝ) else 0) / c_m) (B m) := by
        intro ω hω
        simp only [F]
        rw [h_s_eq ω hω]
      rw [setIntegral_congr_fun (hB_meas m) h_eq]
      -- Case split: c_m ≤ 1 (apply Step 4) or c_m > 1 (null set)
      by_cases hc_le : c_m ≤ 1
      · exact setIntegral_indicator_div_const hP_unif hP_meas
          (setIndepOf_of_isIndep hP_indep (hB_m0 m)) hc_m_pos hc_le
      · -- c_m > 1: B m is null since α ≤ 1 a.e. and ε ≤ α a.e.
        push_neg at hc_le
        have hBm_null : μ (B m) = 0 := by
          have h_sub : B m ⊆ {ω | ¬(ε ≤ α ω)} ∪ {ω | ¬(α ω ≤ 1)} := by
            intro ω hω
            by_cases hαε : ε ≤ α ω
            · right; simp only [Set.mem_setOf_eq, not_le]
              have := hs_le_α n ω hαε
              rw [h_s_eq ω hω] at this
              linarith
            · left; exact hαε
          exact le_antisymm (calc
            μ (B m) ≤ μ ({ω | ¬(ε ≤ α ω)} ∪ {ω | ¬(α ω ≤ 1)}) := measure_mono h_sub
            _ ≤ μ {ω | ¬(ε ≤ α ω)} + μ {ω | ¬(α ω ≤ 1)} := measure_union_le _ _
            _ = 0 + 0 := by rw [ae_iff.mp hα_bdd, ae_iff.mp hα_le_one]
            _ = 0 := add_zero 0) (zero_le _)
        rw [hBm_null, ENNReal.zero_toReal, Measure.restrict_eq_zero.mpr hBm_null,
          integral_zero_measure]
    rw [tsum_congr h_piece]
    -- Step 4: ∑' m, μ(B m).toReal = μ(A).toReal
    conv_rhs => rw [hA_cover]
    rw [measure_iUnion hB_disj hB_meas]
    exact (ENNReal.tsum_toReal_eq (fun m => ne_top_of_le_ne_top (ne_of_lt hμA)
      (measure_mono (Set.inter_subset_left)))).symm
  -- (C) ‖F n ω‖ ≤ 1/ε (pointwise, no a.e. needed since s n ≥ ε everywhere)
  have h_bound : ∀ n, ∀ᵐ ω ∂(μ.restrict A), ‖F n ω‖ ≤ 1 / ε := by
    intro n; filter_upwards with ω
    simp only [F, Real.norm_eq_abs]
    rw [abs_of_nonneg (div_nonneg (by split_ifs <;> norm_num) (le_of_lt (hs_pos n ω)))]
    rw [div_le_div_iff₀ (hs_pos n ω) hε]
    have : (if P ω ≤ s n ω then (1 : ℝ) else 0) ≤ 1 := by split_ifs <;> norm_num
    nlinarith [hs_ge n ω, mul_le_mul_of_nonneg_right this (le_of_lt hε)]
  -- (B) F n → f pointwise a.e.
  -- s n ω → α ω: max(ε, ⌊2^n·α⌋/2^n) → max(ε, α) = α (since α ≥ ε)
  have hs_tendsto : ∀ ω, ε ≤ α ω →
      Filter.Tendsto (fun n => s n ω) Filter.atTop (nhds (α ω)) := by
    intro ω hαω
    simp only [s]
    have h_dyad := tendsto_nat_floor_div_pow (α ω) (le_trans (le_of_lt hε) hαω)
    have h := Filter.Tendsto.max (f := fun _ => ε)
      (g := fun n => (↑(⌊(2:ℝ)^n * α ω⌋₊) : ℝ) / (2:ℝ)^n)
      tendsto_const_nhds h_dyad
    rwa [max_eq_right hαω] at h
  -- μ{P = α} = 0 (uniform P has no atoms + independence)
  have h_P_ne_α : ∀ᵐ ω ∂μ, P ω ≠ α ω := by
    rw [ae_iff]; simp only [ne_eq, not_not]
    suffices h : ∀ N : ℕ, μ {ω | P ω = α ω} ≤ ENNReal.ofReal (2 / (↑N + 1)) from
      nonpos_iff_eq_zero.mp (ge_of_tendsto tendsto_ofReal_two_div
        (Filter.Eventually.of_forall h))
    intro N
    set nn : ℕ := N + 1
    have hnn : (0 : ℝ) < ↑nn := Nat.cast_pos.mpr (Nat.succ_pos N)
    set gg : Ω → ℕ := fun ω => ⌊(↑nn : ℝ) * α ω⌋₊
    have hgg_m0 : @Measurable Ω ℕ m₀ _ gg := measurable_nat_floor.comp (hα_meas.const_mul _)
    have hC_m0 : ∀ k, @MeasurableSet Ω m₀ {ω | gg ω = k} :=
      fun k => hgg_m0 (measurableSet_singleton k)
    have hC_mΩ : ∀ k, @MeasurableSet Ω mΩ {ω | gg ω = k} :=
      fun k => hm _ (hC_m0 k)
    have hα0 : μ {ω | α ω < 0} = 0 := le_antisymm
      (calc μ {ω | α ω < 0} ≤ μ {ω | ¬(ε ≤ α ω)} :=
            measure_mono (fun ω h => not_le.mpr (lt_of_lt_of_le h (le_of_lt hε)))
        _ = 0 := ae_iff.mp hα_bdd) (zero_le _)
    have hcov : {ω | P ω = α ω} ∩ {ω | 0 ≤ α ω} ⊆
        ⋃ k : ℕ, ({ω | gg ω = k} ∩
          P ⁻¹' Set.Ioc ((↑k - 1) / ↑nn) ((↑k + 1) / ↑nn)) := by
      intro ω ⟨hPα, hα_nn⟩
      simp only [Set.mem_setOf_eq] at hPα hα_nn
      rw [Set.mem_iUnion]; use gg ω
      refine ⟨rfl, ?_⟩
      simp only [Set.mem_preimage, Set.mem_Ioc]
      have h_nn2 : 0 ≤ (↑nn : ℝ) * α ω := mul_nonneg (le_of_lt hnn) hα_nn
      rw [hPα]; constructor
      · rw [div_lt_iff₀ hnn]; linarith [Nat.floor_le h_nn2]
      · rw [le_div_iff₀ hnn]
        have := Nat.lt_floor_add_one ((↑nn : ℝ) * α ω)
        linarith
    have hdisj : Pairwise (Disjoint on fun k => {ω : Ω | gg ω = k}) := by
      intro i j hij
      simp only [Function.onFun, Set.disjoint_left, Set.mem_setOf_eq]
      exact fun ω hi hj => hij (hi.symm.trans hj)
    calc μ {ω | P ω = α ω}
        ≤ μ ({ω | P ω = α ω} ∩ {ω | 0 ≤ α ω}) + μ {ω | α ω < 0} := by
          calc μ {ω | P ω = α ω}
              ≤ μ (({ω | P ω = α ω} ∩ {ω | 0 ≤ α ω}) ∪ {ω | α ω < 0}) :=
                measure_mono (fun ω hω => by
                  by_cases h : 0 ≤ α ω
                  · exact Or.inl ⟨hω, h⟩
                  · exact Or.inr (lt_of_not_le h))
              _ ≤ _ := measure_union_le _ _
      _ = μ ({ω | P ω = α ω} ∩ {ω | 0 ≤ α ω}) := by rw [hα0, add_zero]
      _ ≤ ∑' k, μ ({ω | gg ω = k} ∩
            P ⁻¹' Set.Ioc ((↑k - 1) / ↑nn) ((↑k + 1) / ↑nn)) :=
          (measure_mono hcov).trans (measure_iUnion_le _)
      _ = ∑' k, μ {ω | gg ω = k} *
            μ (P ⁻¹' Set.Ioc ((↑k - 1) / ↑nn) ((↑k + 1) / ↑nn)) := by
          congr 1; ext k; exact hP_indep _ _ (hC_m0 k) measurableSet_Ioc
      _ ≤ ∑' k, μ {ω | gg ω = k} * ENNReal.ofReal (2 / ↑nn) := by
          apply ENNReal.tsum_le_tsum; intro k; apply mul_le_mul_left'
          calc μ (P ⁻¹' Set.Ioc ((↑k - 1) / ↑nn) ((↑k + 1) / ↑nn))
              ≤ ENNReal.ofReal ((↑k + 1) / ↑nn - (↑k - 1) / ↑nn) :=
                ioc_measure_bound hP_unif hP_meas
                  (div_le_div_of_nonneg_right (by linarith) (le_of_lt hnn))
            _ = ENNReal.ofReal (2 / ↑nn) := by congr 1; field_simp; ring
      _ = (∑' k, μ {ω | gg ω = k}) * ENNReal.ofReal (2 / ↑nn) :=
          ENNReal.tsum_mul_right
      _ = μ Set.univ * ENNReal.ofReal (2 / ↑nn) := by
          congr 1
          have h_union : (⋃ k, {ω : Ω | gg ω = k}) = Set.univ := by
            ext ω; simp [Set.mem_iUnion, Set.mem_setOf_eq]
          rw [← h_union, measure_iUnion hdisj hC_mΩ]
      _ = ENNReal.ofReal (2 / (↑N + 1)) := by
          rw [measure_univ, one_mul]; push_cast; ring_nf
  have h_lim : ∀ᵐ ω ∂(μ.restrict A),
      Filter.Tendsto (fun n => F n ω) Filter.atTop
        (nhds ((if P ω ≤ α ω then (1 : ℝ) else 0) / α ω)) := by
    rw [ae_restrict_iff' hA_mΩ]
    filter_upwards [hα_bdd, h_P_ne_α] with ω hαω hne _
    have hα_pos : 0 < α ω := lt_of_lt_of_le hε hαω
    have hs_lim := hs_tendsto ω hαω
    cases lt_or_gt_of_ne hne with
    | inl hlt => -- P ω < α ω
      rw [if_pos (le_of_lt hlt)]
      have h_ev : ∀ᶠ n in Filter.atTop, F n ω = 1 / s n ω := by
        filter_upwards [hs_lim.eventually (eventually_gt_nhds hlt)] with n hn
        simp only [F, if_pos (le_of_lt hn)]
      exact (tendsto_const_nhds.div hs_lim (ne_of_gt hα_pos)).congr'
        (h_ev.mono (fun n h => h.symm))
    | inr hgt => -- P ω > α ω
      rw [if_neg (not_le.mpr hgt), zero_div]
      -- For all n: s n ω ≤ α ω < P ω, so indicator = 0, F n = 0
      apply Filter.Tendsto.congr (fun n => ?_) tendsto_const_nhds
      simp only [F, if_neg (not_le.mpr (lt_of_le_of_lt (hs_le_α n ω hαω) hgt)), zero_div]
  -- (D) F n is AEStronglyMeasurable
  have h_aesm : ∀ n, AEStronglyMeasurable (F n) (μ.restrict A) := by
    intro n
    -- Step 1: s n is measurable
    have hs_meas : Measurable (s n) := by
      show Measurable (fun ω => max ε (↑(⌊(2 : ℝ) ^ n * α ω⌋₊) / (2 : ℝ) ^ n))
      -- The floor function Ω → ℕ is measurable
      have h_floor : Measurable (fun ω => ⌊(2 : ℝ) ^ n * α ω⌋₊) :=
        measurable_nat_floor.comp (hα_mΩ.const_mul _)
      -- Cast ℕ → ℝ is measurable (ℕ has discrete MeasurableSpace)
      -- Then div_const and const_sup
      exact ((measurable_from_top.comp h_floor).div_const _).const_sup _
    -- Step 2: F n = (if P ≤ s n then 1 else 0) / s n is measurable
    exact ((Measurable.ite (measurableSet_le hP_meas hs_meas)
      measurable_const measurable_const).div hs_meas).aestronglyMeasurable
  -- Apply DCT: lim ∫_A F n = ∫_A f
  have h_dct := tendsto_integral_of_dominated_convergence
    (fun _ => 1 / ε) h_aesm (integrable_const _) h_bound h_lim
  -- Each ∫_A F n = μ(A).toReal (constant sequence)
  have h_const_seq : (fun n => ∫ ω in A, F n ω ∂μ) = fun _ => (μ A).toReal :=
    funext h_each
  rw [h_const_seq] at h_dct
  -- Limit uniqueness: ∫_A f = μ(A).toReal
  exact tendsto_nhds_unique h_dct tendsto_const_nhds


/-! ### Step 8: Conditional expectation (PROVED from Steps 6–7)

    condexp m₀ μ (𝟙{P ≤ α}/α) =ᵐ[μ] 1

    Proof: Apply ae_eq_condexp_of_forall_setIntegral_eq with g ≡ 1.
    - Integrable f μ: by Step 7
    - IntegrableOn 1 s μ: constant on finite-measure set
    - ∫_s 1 dμ = ∫_s f dμ: both equal μ(s).toReal, by Step 6
    - AEStronglyMeasurable' m₀ 1 μ: from stronglyMeasurable_const -/

theorem fundamental_lemma_conditional
    [IsProbabilityMeasure μ]
    (hm : m₀ ≤ mΩ)
    {P α : Ω → ℝ} {ε : ℝ} (hε : 0 < ε)
    (hP_unif : IsUniformPValue μ P)
    (hP_meas : Measurable P)
    (hα_meas : @Measurable Ω ℝ m₀ _ α)
    (hα_bdd : ∀ᵐ ω ∂μ, ε ≤ α ω)
    (hα_le_one : ∀ᵐ ω ∂μ, α ω ≤ 1)
    (hP_indep : IsIndepOfSubalgebra μ P m₀) :
    condexp m₀ μ (fun ω => (if P ω ≤ α ω then (1 : ℝ) else 0) / α ω)
    =ᵐ[μ] fun _ => (1 : ℝ) := by
  -- SigmaFinite (μ.trim hm) from finite measure
  haveI : IsFiniteMeasure (μ.trim hm) := isFiniteMeasure_trim hm
  -- Integrability of f
  have hf_int : Integrable (fun ω => (if P ω ≤ α ω then (1 : ℝ) else 0) / α ω) μ :=
    integrable_indicator_div hm hε hP_meas hα_meas hα_bdd
  -- Apply uniqueness: g ≡ 1 =ᵐ μ[f | m₀]
  symm
  exact ae_eq_condexp_of_forall_setIntegral_eq hm hf_int
    -- (i) g ≡ 1 is integrable on m₀-measurable sets of finite measure
    (fun s _hs hμs => integrableOn_const.mpr (Or.inr hμs))
    -- (ii) ∫_s 1 dμ = ∫_s f dμ  (both = μ(s).toReal)
    (fun s hs hμs => by
      rw [integral_const, Measure.restrict_apply_univ, smul_eq_mul, mul_one]
      exact (setIntegral_indicator_div hm hε hP_unif hP_meas hα_meas
               hα_bdd hα_le_one hP_indep hs hμs).symm)
    -- (iii) g ≡ 1 is AEStronglyMeasurable' m₀
    ⟨fun _ => 1, @stronglyMeasurable_const Ω ℝ m₀ _ _, ae_eq_refl _⟩

end ConditionalVersion

end LordFDR.FundamentalLemma
