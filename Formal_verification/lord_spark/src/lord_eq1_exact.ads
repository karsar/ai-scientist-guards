--  Lord_Eq1_Exact: the floating-point budget of Eq. 1 without the clamp (G2).
--
--  Lord_Eq1.Advance spends Alpha_T = min (Eq1, W). This package proves that,
--  under a margin condition on the gamma table, the clamp never acts:
--  the Eq. 1 value computed in floating point is at most the wealth at
--  every step. So Lord_Eq1.Advance computes exactly Eq. 1, and
--  Clamp_Count stays 0.
--
--  The margin is necessary. With floating-point prefix sums of gamma equal
--  to 1.0, IEEE 754 rounding can make the Eq. 1 value exceed the wealth by
--  one unit in the last place (see test/lord_eq1_check.adb).
--
--  Proof idea. Let P (n) be the exact real sum gamma_1 + ... + gamma_n of
--  the floating-point table values, and d = alpha - W0 (the floating-point
--  value that the code uses). Over the reals the wealth after T steps is
--
--    Budget = W0 * (1 - P (T)) + d * sum_j (1 - P (T - tau_j)).
--
--  The ghost invariant Exact_Inv states that the floating-point wealth is
--  at least Budget - T * Step_Err. Each step makes at most N_Disc + 5
--  roundings, each bounded by U (SPARK lemma library, rounding-error
--  lemmas). Budget - (exact Eq. 1 value) >= W0 * (1 - P (T + 1)), and the
--  margin condition makes this at least Max_T * Step_Err.
--
--  All functions and lemmas here are ghost code. They do not run.

with SPARK.Big_Reals;                         use SPARK.Big_Reals;
with SPARK.Conversions.Long_Float_Conversions;
use  SPARK.Conversions.Long_Float_Conversions;
with Lord_Eq1;                                use Lord_Eq1;

package Lord_Eq1_Exact
  with SPARK_Mode => On
is

   -------------------------------------------------------------------------
   --  Error constants (exact real values)
   -------------------------------------------------------------------------

   --  U = 2**(-38): a bound on the error of one rounding in this package.
   --  All operands are at most 20_002, and
   --  2**(-53) * 20_002 + 2**(-1075) < 2**(-38).
   U : constant Big_Real := 0.00000000000363797880709171295166015625
     with Ghost;

   --  Error bound for one step: at most Max_T + 5 roundings
   Step_Err : constant Big_Real := 10_005.0 * U
     with Ghost;

   --  Required margin: Max_T steps of error (about 3.64E-4)
   Min_Margin : constant Big_Real := 10_000.0 * Step_Err
     with Ghost;

   -------------------------------------------------------------------------
   --  Exact real sums
   -------------------------------------------------------------------------

   --  P (N) = gamma_1 + ... + gamma_N over the reals
   function Real_Prefix (G : Gamma_Table; N : Time_Count) return Big_Real is
     (if N = 0 then 0.0 else Real_Prefix (G, N - 1) + To_Big_Real (G (N)))
     with
       Ghost,
       Subprogram_Variant => (Decreases => N);

   --  The discovery times 1 .. K are at most S.T
   function Disc_OK (S : Protocol_State; K : Time_Count) return Boolean is
     (K <= S.N_Disc
      and then (for all J in 1 .. K => S.Disc (J) <= S.T))
     with Ghost;

   --  Exact real sum in Eq. 1 for step S.T + 1 (Disc_Sum without rounding)
   function Spend
     (S : Protocol_State; G : Gamma_Table; K : Time_Count) return Big_Real
   is
     (if K = 0 then 0.0
      else Spend (S, G, K - 1) + To_Big_Real (G (S.T + 1 - S.Disc (K))))
     with
       Ghost,
       Pre => S.T < Max_T and then Disc_OK (S, K),
       Subprogram_Variant => (Decreases => K);

   --  Remaining budget of the discoveries 1 .. K after S.T steps, per unit
   --  of reward: sum_j (1 - P (S.T - tau_j))
   function Remain
     (S : Protocol_State; G : Gamma_Table; K : Time_Count) return Big_Real
   is
     (if K = 0 then 0.0
      else Remain (S, G, K - 1) + (1.0 - Real_Prefix (G, S.T - S.Disc (K))))
     with
       Ghost,
       Pre => Disc_OK (S, K),
       Subprogram_Variant => (Decreases => K);

   --  The reward d = alpha - W0, as the code computes it
   function Reward (S : Protocol_State) return Long_Float is
     (S.Alpha_Param - S.W0)
     with Ghost, Pre => Valid (S);

   --  Eq. 1 over the reals, with the floating-point table values
   function Real_Eq1 (S : Protocol_State; G : Gamma_Table) return Big_Real is
     (To_Big_Real (S.W0) * To_Big_Real (G (S.T + 1))
      + To_Big_Real (Reward (S)) * Spend (S, G, S.N_Disc))
     with Ghost, Pre => Valid (S) and then S.T < Max_T;

   --  The wealth over the reals
   function Budget (S : Protocol_State; G : Gamma_Table) return Big_Real is
     (To_Big_Real (S.W0) * (1.0 - Real_Prefix (G, S.T))
      + To_Big_Real (Reward (S)) * Remain (S, G, S.N_Disc))
     with Ghost, Pre => Valid (S);

   -------------------------------------------------------------------------
   --  The precondition on the table and the invariant
   -------------------------------------------------------------------------

   --  W0 * (1 - P (N)) >= Min_Margin for every N
   function Margin_OK (G : Gamma_Table; W0 : Long_Float) return Boolean is
     (for all N in Time_Count =>
        To_Big_Real (W0) * (1.0 - Real_Prefix (G, N)) >= Min_Margin)
     with Ghost;

   --  The floating-point wealth is at least Budget - T * Step_Err
   function Exact_Inv (S : Protocol_State; G : Gamma_Table) return Boolean is
     (To_Big_Real (S.W) >= Budget (S, G) - To_Real (S.T) * Step_Err)
     with Ghost, Pre => Valid (S);

   -------------------------------------------------------------------------
   --  The results
   -------------------------------------------------------------------------

   --  Base case: the invariant holds after Initialize
   procedure Lemma_Initialize
     (Alpha_Param : Probability;
      W0          : Probability;
      G           : Gamma_Table)
     with
       Ghost,
       Global => null,
       Pre    => W0 > 0.0
                 and then W0 < Alpha_Param
                 and then Alpha_Param <= 1.0,
       Post   => Exact_Inv (Initialize (Alpha_Param, W0), G);

   --  G2, one step: the unclamped Eq. 1 value is at most the wealth, so
   --  Advance does not clamp, and the invariant is kept.
   procedure Advance_Exact
     (S       : in out Protocol_State;
      G       : in     Gamma_Table;
      P_Value : in     Probability;
      Alpha_T : out    Nonnegative;
      Reject  : out    Boolean)
     with
       Pre  => Valid (S)
               and then S.T < Max_T
               and then Margin_OK (G, S.W0)
               and then Exact_Inv (S, G),
       Post => Valid (S)
               and then Exact_Inv (S, G)
               and then S.T = S.T'Old + 1
               and then S.W0 = S.W0'Old
               and then S.Alpha_Param = S.Alpha_Param'Old
               --  G2: exactly Eq. 1, within the wealth, no clamp
               and then Threshold (S'Old, G) <= S.W'Old
               and then Alpha_T = Threshold (S'Old, G)
               and then S.Clamp_Count = S.Clamp_Count'Old
               and then Reject = (P_Value <= Alpha_T)
               and then (if Reject
                         then S.W = (S.W'Old - Alpha_T)
                                    + (S.Alpha_Param - S.W0)
                         else S.W = S.W'Old - Alpha_T);

   --  G2 over a whole sequence: the clamp never acts
   procedure Run_Sequence_Exact
     (S        : in out Protocol_State;
      G        : in     Gamma_Table;
      P_Values : in     P_Value_Array;
      Alphas   : in out Level_Array;
      W_Before : in out Level_Array;
      Rejects  : in out Reject_Array)
     with
       Pre  => Valid (S)
               and then Margin_OK (G, S.W0)
               and then Exact_Inv (S, G)
               and then P_Values'Length <= Max_T - S.T
               and then Alphas'First = P_Values'First
               and then Alphas'Last = P_Values'Last
               and then W_Before'First = P_Values'First
               and then W_Before'Last = P_Values'Last
               and then Rejects'First = P_Values'First
               and then Rejects'Last = P_Values'Last,
       Post => Valid (S)
               and then Exact_Inv (S, G)
               and then S.T = S.T'Old + P_Values'Length
               and then S.W0 = S.W0'Old
               and then S.Alpha_Param = S.Alpha_Param'Old
               and then S.Clamp_Count = S.Clamp_Count'Old
               and then (for all I in P_Values'Range =>
                           Alphas (I) <= W_Before (I)
                           and then Rejects (I) = (P_Values (I) <= Alphas (I)));

end Lord_Eq1_Exact;
