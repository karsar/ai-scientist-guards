--  Lord_Eps: the threshold-sum driver, with the wealth-safety invariants that
--  SPARK proves well, and an explicit note on the floating-point error bound
--  that it does not.
--
--  Over the reals the LORD++ thresholds telescope:
--      alpha_t = gamma_t * W_t,  W_{t+1} = (1 - gamma_t) * W_t
--      =>  alpha_t + W_{t+1} = W_t,  so  sum alpha_t = W0 - W_n <= W0.
--
--  GNATprove proves the structural safety of the floating-point realisation:
--  the wealth stays a probability and the threshold sum stays non-negative,
--  under every rounding sequence (see postcondition; discharged at level 2).
--
--  It does NOT prove the closed-form rounding bound |sum alpha^fl - sum
--  alpha^real| <= n*eps. That is floating-point error analysis, outside the
--  sweet spot of SPARK + SMT: the per-step round-to-nearest bound needs a
--  specialist FP prover (Gappa) — absent from this toolchain — and the n-step
--  accumulation needs Big_Real ghosts to escape float-addition non-
--  associativity in the contract. The canonical tool for that bound is
--  Coq+Flocq(+Gappa); see paper Section 5.4. The load-bearing
--  arithmetic obligation H4 — wealth never goes negative — is fully proved in
--  Lord_PP (lord_pp.ads); this package adds the sum driver.

package Lord_Eps
  with SPARK_Mode => On
is

   subtype Prob is Long_Float range 0.0 .. 1.0;

   type Gamma_Array is array (Positive range <>) of Prob;

   --  Process a gamma sequence, accumulating realized thresholds.
   --  Proved: the threshold sum and final wealth are non-negative, and the
   --  wealth remains a probability — under every IEEE-754 rounding sequence.
   procedure Sum_Thresholds
     (W0        : in     Prob;
      Gammas    : in     Gamma_Array;
      Sum_Alpha :    out Long_Float;
      W_Final   :    out Prob)
     with
       Post => Sum_Alpha >= 0.0 and then W_Final >= 0.0;

end Lord_Eps;
