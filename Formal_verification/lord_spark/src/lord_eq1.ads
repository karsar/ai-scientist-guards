--  Lord_Eq1: the LORD thresholds of Equation (1) in SPARK/Ada.
--
--  This package computes the thresholds of Javanmard and Montanari (2018),
--  LORD version 2, with reward alpha - W0 for each discovery:
--
--    alpha_t = gamma_t * W0 + (alpha - W0) * sum_{j : tau_j < t} gamma_(t - tau_j)
--
--  The package also keeps the wealth W of the generalized alpha-investing
--  form: W(0) = W0 and W(t) = W(t-1) - alpha_t + (alpha - W0) * R_t.
--
--  Over the real numbers, alpha_t <= W(t-1) is always true. Over IEEE 754
--  it can fail by one unit in the last place when the prefix sums of gamma
--  reach 1. Advance therefore spends Alpha_T = min (Eq1, W). It counts in
--  Clamp_Count each step where the clamp changes the value. The proved
--  statements are:
--
--    * Alpha_T >= 0 and Alpha_T <= W before each update, and W >= 0 after it;
--    * Alpha_T equals Threshold (the Eq. 1 value, computed in floating
--      point) at every step where Clamp_Count does not change;
--    * Threshold equals the Eq. 1 expression with a left-to-right sum over
--      the discoveries in time order (Disc_Sum).
--
--  Lord_Eq1_Exact proves that the clamp never acts when the gamma table
--  leaves a margin below 1 (Margin_OK). Then Advance computes exactly Eq. 1.
--
--  Lord_PP (alpha_t = gamma_t * W) is a different rule. This package does
--  not change it.

package Lord_Eq1
  with SPARK_Mode => On
is

   --  Maximum number of steps (the paper's simulation uses 2000)
   Max_T : constant := 10_000;

   subtype Time_Count is Natural range 0 .. Max_T;
   subtype Time_Index is Positive range 1 .. Max_T;

   subtype Probability is Long_Float range 0.0 .. 1.0;
   subtype Nonnegative is Long_Float range 0.0 .. Long_Float'Last;

   --  Gamma (J) is gamma_J. The caller computes the table outside SPARK and
   --  clamps each value to [0, 1] (for example with Lord_PP.Safe_Gamma).
   type Gamma_Table is array (Time_Index) of Probability;

   type Discovery_Times is array (Time_Index) of Time_Index;

   type Protocol_State is record
      T           : Time_Count;       --  Number of completed steps
      W0          : Probability;      --  Initial wealth
      Alpha_Param : Probability;      --  Target FDR level alpha
      W           : Nonnegative;      --  Wealth after step T
      N_Disc      : Time_Count;       --  Number of discoveries R_T
      Disc        : Discovery_Times;  --  Disc (1 .. N_Disc) = tau_1 < tau_2 < ...
      Clamp_Count : Time_Count;       --  Number of steps where the clamp acted
   end record;

   --  The state invariant. The wealth bound W <= T + 1 prevents overflow:
   --  each step adds at most alpha - W0 < 1 to the wealth.
   function Valid (S : Protocol_State) return Boolean is
     (S.W0 > 0.0
      and then S.W0 < S.Alpha_Param
      and then S.Alpha_Param <= 1.0
      and then S.W <= Long_Float (S.T + 1)
      and then S.N_Disc <= S.T
      and then S.Clamp_Count <= S.T
      and then (for all K in 1 .. S.N_Disc => S.Disc (K) <= S.T)
      and then (for all K in 2 .. S.N_Disc => S.Disc (K - 1) < S.Disc (K)));

   -------------------------------------------------------------------------
   --  Disc_Sum: the sum in Eq. 1 for the next step t = S.T + 1, over the
   --  first K discoveries, added left to right in floating point:
   --    Disc_Sum (0) = 0.0
   --    Disc_Sum (K) = Disc_Sum (K - 1) + gamma_(t - tau_K)
   --  The postcondition states this definition. It is the specification of
   --  the sum; Threshold computes the same value in a loop.
   -------------------------------------------------------------------------
   function Disc_Sum
     (S : Protocol_State;
      G : Gamma_Table;
      K : Time_Count) return Long_Float
     with
       Ghost,
       Pre  => Valid (S) and then S.T < Max_T and then K <= S.N_Disc,
       Post => Disc_Sum'Result >= 0.0
               and then Disc_Sum'Result <= Long_Float (K)
               and then (if K = 0
                         then Disc_Sum'Result = 0.0
                         else Disc_Sum'Result =
                                Disc_Sum (S, G, K - 1)
                                + G (S.T + 1 - S.Disc (K))),
       Subprogram_Variant => (Decreases => K);

   -------------------------------------------------------------------------
   --  Initialize: T = 0, W = W0, no discoveries
   -------------------------------------------------------------------------
   function Initialize
     (Alpha_Param : Probability;
      W0          : Probability) return Protocol_State
     with
       Pre  => W0 > 0.0
               and then W0 < Alpha_Param
               and then Alpha_Param <= 1.0,
       Post => Valid (Initialize'Result)
               and then Initialize'Result.T = 0
               and then Initialize'Result.W = W0
               and then Initialize'Result.W0 = W0
               and then Initialize'Result.Alpha_Param = Alpha_Param
               and then Initialize'Result.N_Disc = 0
               and then Initialize'Result.Clamp_Count = 0;

   -------------------------------------------------------------------------
   --  Threshold: alpha_t of Eq. 1 for the next step t = S.T + 1, computed
   --  in floating point with no clamping.
   -------------------------------------------------------------------------
   function Threshold
     (S : Protocol_State;
      G : Gamma_Table) return Nonnegative
     with
       Pre  => Valid (S) and then S.T < Max_T,
       Post => Threshold'Result =
                 S.W0 * G (S.T + 1)
                 + (S.Alpha_Param - S.W0) * Disc_Sum (S, G, S.N_Disc);

   -------------------------------------------------------------------------
   --  Advance: one step of the procedure.
   --    Alpha_T := min (Threshold, W)
   --    Reject  := P_Value <= Alpha_T
   --    W       := W - Alpha_T, plus alpha - W0 if Reject
   -------------------------------------------------------------------------
   procedure Advance
     (S       : in out Protocol_State;
      G       : in     Gamma_Table;
      P_Value : in     Probability;
      Alpha_T : out    Nonnegative;
      Reject  : out    Boolean)
     with
       Pre  => Valid (S) and then S.T < Max_T,
       Post => Valid (S)
               and then S.T = S.T'Old + 1
               and then S.W0 = S.W0'Old
               and then S.Alpha_Param = S.Alpha_Param'Old
               --  Budget: the step never spends more than the wealth
               and then Alpha_T <= S.W'Old
               --  The clamp, and when it acts
               and then (if Threshold (S'Old, G) <= S.W'Old
                         then Alpha_T = Threshold (S'Old, G)
                              and then S.Clamp_Count = S.Clamp_Count'Old
                         else Alpha_T = S.W'Old
                              and then S.Clamp_Count = S.Clamp_Count'Old + 1)
               and then Reject = (P_Value <= Alpha_T)
               and then S.W'Old - Alpha_T in 0.0 .. S.W'Old
               --  Wealth update of the generalized alpha-investing form
               and then (if Reject
                         then S.W = (S.W'Old - Alpha_T)
                                    + (S.Alpha_Param - S.W0)
                         else S.W = S.W'Old - Alpha_T)
               --  Discovery bookkeeping
               and then (if Reject
                         then S.N_Disc = S.N_Disc'Old + 1
                              and then S.Disc (S.N_Disc) = S.T
                         else S.N_Disc = S.N_Disc'Old)
               and then (for all K in 1 .. S.N_Disc'Old =>
                           S.Disc (K) = S.Disc'Old (K));

   -------------------------------------------------------------------------
   --  Run_Sequence: Advance over a whole p-value sequence.
   --  Alphas (I) is the threshold used at step I and W_Before (I) the
   --  wealth before that step. The loop invariant carries the budget
   --  property Alphas (I) <= W_Before (I) across all steps.
   -------------------------------------------------------------------------
   type P_Value_Array is array (Positive range <>) of Probability;
   type Level_Array   is array (Positive range <>) of Nonnegative;
   type Reject_Array  is array (Positive range <>) of Boolean;

   procedure Run_Sequence
     (S        : in out Protocol_State;
      G        : in     Gamma_Table;
      P_Values : in     P_Value_Array;
      Alphas   : in out Level_Array;
      W_Before : in out Level_Array;
      Rejects  : in out Reject_Array)
     with
       Pre  => Valid (S)
               and then P_Values'Length <= Max_T - S.T
               and then Alphas'First = P_Values'First
               and then Alphas'Last = P_Values'Last
               and then W_Before'First = P_Values'First
               and then W_Before'Last = P_Values'Last
               and then Rejects'First = P_Values'First
               and then Rejects'Last = P_Values'Last,
       Post => Valid (S)
               and then S.T = S.T'Old + P_Values'Length
               and then S.W0 = S.W0'Old
               and then S.Alpha_Param = S.Alpha_Param'Old
               and then S.Clamp_Count >= S.Clamp_Count'Old
               and then (for all I in P_Values'Range =>
                           Alphas (I) <= W_Before (I)
                           and then Rejects (I) = (P_Values (I) <= Alphas (I)));

end Lord_Eq1;
