--  Lord_Eq1_Exact: the floating-point budget of Eq. 1 without the clamp (G2)
--  Body: ghost lemmas and the proof of Advance_Exact.

with SPARK.Lemmas.Long_Float_Arithmetic;
use  SPARK.Lemmas.Long_Float_Arithmetic;

package body Lord_Eq1_Exact
  with SPARK_Mode => On
is

   -------------------------------------------------------------------------
   --  Rounding bounds for one operation (from the SPARK lemma library)
   -------------------------------------------------------------------------

   procedure Add_Upper (A, B : Long_Float)
     with
       Ghost,
       Global => null,
       Pre    => A in 0.0 .. 10_001.0 and then B in 0.0 .. 10_001.0,
       Post   => To_Big_Real (A + B) <= To_Big_Real (A) + To_Big_Real (B) + U;

   procedure Add_Upper (A, B : Long_Float) is
   begin
      Lemma_Rounding_Error_Add (A, B);
   end Add_Upper;

   procedure Add_Lower (A, B : Long_Float)
     with
       Ghost,
       Global => null,
       Pre    => A in 0.0 .. 10_001.0 and then B in 0.0 .. 10_001.0,
       Post   => To_Big_Real (A + B) >= To_Big_Real (A) + To_Big_Real (B) - U;

   procedure Add_Lower (A, B : Long_Float) is
   begin
      Lemma_Rounding_Error_Add (A, B);
   end Add_Lower;

   procedure Mul_Upper (A, B : Long_Float)
     with
       Ghost,
       Global => null,
       Pre    => A in 0.0 .. 1.0 and then B in 0.0 .. 10_001.0,
       Post   => To_Big_Real (A * B) <= To_Big_Real (A) * To_Big_Real (B) + U;

   procedure Mul_Upper (A, B : Long_Float) is
   begin
      Lemma_Rounding_Error_Mul (A, B);
   end Mul_Upper;

   procedure Sub_Lower (A, B : Long_Float)
     with
       Ghost,
       Global => null,
       Pre    => A in 0.0 .. 10_001.0 and then B in 0.0 .. A,
       Post   => To_Big_Real (A - B) >= To_Big_Real (A) - To_Big_Real (B) - U;

   procedure Sub_Lower (A, B : Long_Float) is
   begin
      Lemma_Rounding_Error_Sub (A, B);
   end Sub_Lower;

   --  Real order gives floating-point order
   procedure Real_Le (A, B : Long_Float)
     with
       Ghost,
       Global => null,
       Pre    => To_Big_Real (A) <= To_Big_Real (B),
       Post   => A <= B;

   procedure Real_Le (A, B : Long_Float) is
   begin
      null;
   end Real_Le;

   --  Small integers convert below 10_000.0
   procedure Lemma_Time_Bound (N : Time_Count)
     with
       Ghost,
       Global => null,
       Post   => Long_Float (N) <= 10_000.0;

   procedure Lemma_Time_Bound (N : Time_Count) is
   begin
      null;
   end Lemma_Time_Bound;

   --  Multiplication by D in [0, 1] keeps an upper bound with error C
   procedure Scale (D, X, Y, C : Big_Real)
     with
       Ghost,
       Global => null,
       Pre    => D >= 0.0 and then D <= 1.0 and then C >= 0.0
                 and then X <= Y + C,
       Post   => D * X <= D * Y + C;

   procedure Scale (D, X, Y, C : Big_Real) is
   begin
      pragma Assert (D * X <= D * (Y + C));
      pragma Assert (D * (Y + C) = D * Y + D * C);
      pragma Assert (D * C <= C);
   end Scale;

   -------------------------------------------------------------------------
   --  Lemmas on the sums
   -------------------------------------------------------------------------

   --  P (N) <= 1 for every N, from the margin
   procedure Lemma_Prefix_Le_One (G : Gamma_Table; W0 : Long_Float)
     with
       Ghost,
       Global => null,
       Pre    => W0 > 0.0 and then Margin_OK (G, W0),
       Post   => (for all N in Time_Count => Real_Prefix (G, N) <= 1.0);

   procedure Lemma_Prefix_Le_One (G : Gamma_Table; W0 : Long_Float) is
   begin
      for N in Time_Count loop
         pragma Assert
           (To_Big_Real (W0) * (1.0 - Real_Prefix (G, N)) >= Min_Margin);
         pragma Assert (Min_Margin > 0.0);
         pragma Assert (To_Big_Real (W0) > 0.0);
         pragma Assert (Real_Prefix (G, N) <= 1.0);
         pragma Loop_Invariant
           (for all M in 0 .. N => Real_Prefix (G, M) <= 1.0);
      end loop;
   end Lemma_Prefix_Le_One;

   --  The floating-point sum Disc_Sum is at most the exact sum plus K * U
   procedure Lemma_Sum_Error
     (S : Protocol_State; G : Gamma_Table; K : Time_Count)
     with
       Ghost,
       Global => null,
       Pre    => Valid (S) and then S.T < Max_T and then K <= S.N_Disc,
       Post   => To_Big_Real (Disc_Sum (S, G, K))
                   <= Spend (S, G, K) + To_Real (K) * U,
       Subprogram_Variant => (Decreases => K);

   procedure Lemma_Sum_Error
     (S : Protocol_State; G : Gamma_Table; K : Time_Count) is
   begin
      if K = 0 then
         return;
      end if;
      Lemma_Sum_Error (S, G, K - 1);
      declare
         Prev : constant Long_Float := Disc_Sum (S, G, K - 1);
         Term : constant Long_Float := G (S.T + 1 - S.Disc (K));
      begin
         pragma Assert (Prev <= 10_000.0);
         Add_Upper (Prev, Term);
         pragma Assert (Disc_Sum (S, G, K) = Prev + Term);
         pragma Assert
           (Spend (S, G, K) = Spend (S, G, K - 1) + To_Big_Real (Term));
         pragma Assert (To_Real (K) = To_Real (K - 1) + 1.0);
      end;
   end Lemma_Sum_Error;

   --  After one more step, each discovery's remaining budget drops by
   --  that step's spend: Remain (T + 1) = Remain (T) - Spend (T + 1).
   procedure Lemma_Remain_Step
     (S_Old, S_New : Protocol_State; G : Gamma_Table; K : Time_Count)
     with
       Ghost,
       Global => null,
       Pre    => S_Old.T < Max_T
                 and then S_New.T = S_Old.T + 1
                 and then Disc_OK (S_Old, K)
                 and then K <= S_New.N_Disc
                 and then (for all J in 1 .. K =>
                             S_New.Disc (J) = S_Old.Disc (J)),
       Post   => Disc_OK (S_New, K)
                 and then Remain (S_New, G, K)
                          = Remain (S_Old, G, K) - Spend (S_Old, G, K),
       Subprogram_Variant => (Decreases => K);

   procedure Lemma_Remain_Step
     (S_Old, S_New : Protocol_State; G : Gamma_Table; K : Time_Count) is
   begin
      if K = 0 then
         return;
      end if;
      Lemma_Remain_Step (S_Old, S_New, G, K - 1);
      declare
         N : constant Time_Index := S_New.T - S_New.Disc (K);
      begin
         pragma Assert (N = S_Old.T + 1 - S_Old.Disc (K));
         pragma Assert
           (Real_Prefix (G, N) = Real_Prefix (G, N - 1) + To_Big_Real (G (N)));
      end;
   end Lemma_Remain_Step;

   --  Remaining budgets are non-negative when P (N) <= 1
   procedure Lemma_Remain_Nonneg
     (S : Protocol_State; G : Gamma_Table; K : Time_Count)
     with
       Ghost,
       Global => null,
       Pre    => Disc_OK (S, K)
                 and then (for all N in Time_Count =>
                             Real_Prefix (G, N) <= 1.0),
       Post   => Remain (S, G, K) >= 0.0,
       Subprogram_Variant => (Decreases => K);

   procedure Lemma_Remain_Nonneg
     (S : Protocol_State; G : Gamma_Table; K : Time_Count) is
   begin
      if K > 0 then
         Lemma_Remain_Nonneg (S, G, K - 1);
      end if;
   end Lemma_Remain_Nonneg;

   --  The floating-point Eq. 1 value is at most the real one plus
   --  (N_Disc + 3) roundings
   procedure Lemma_Eq1_Error (S : Protocol_State; G : Gamma_Table)
     with
       Ghost,
       Global => null,
       Pre    => Valid (S) and then S.T < Max_T,
       Post   => To_Big_Real (Threshold (S, G))
                   <= Real_Eq1 (S, G) + (To_Real (S.N_Disc) + 3.0) * U;

   procedure Lemma_Eq1_Error (S : Protocol_State; G : Gamma_Table) is
      D   : constant Long_Float := S.Alpha_Param - S.W0;
      Sum : constant Long_Float := Disc_Sum (S, G, S.N_Disc);
      Gt  : constant Long_Float := G (S.T + 1);
      M1  : constant Long_Float := S.W0 * Gt;
      M2  : constant Long_Float := D * Sum;
   begin
      pragma Assert (D >= 0.0 and then D <= 1.0);
      pragma Assert (Sum <= 10_000.0);

      Lemma_Sum_Error (S, G, S.N_Disc);
      Scale (To_Big_Real (D), To_Big_Real (Sum), Spend (S, G, S.N_Disc),
             To_Real (S.N_Disc) * U);

      Mul_Upper (S.W0, Gt);
      Mul_Upper (D, Sum);
      Lemma_Mult_By_Less_Than_One (S.W0, Gt);
      Lemma_Mult_By_Less_Than_One (D, Sum);
      Add_Upper (M1, M2);

      pragma Assert (Threshold (S, G) = M1 + M2);
      pragma Assert (To_Big_Real (M1) <= To_Big_Real (S.W0) * To_Big_Real (Gt) + U);
      pragma Assert
        (To_Big_Real (M2) <= To_Big_Real (D) * Spend (S, G, S.N_Disc)
                      + To_Real (S.N_Disc) * U + U);
      pragma Assert (Reward (S) = D);
   end Lemma_Eq1_Error;

   --  Budget unfolded (kept separate so that the provers see only this)
   procedure Lemma_Budget_Unfold (S : Protocol_State; G : Gamma_Table)
     with
       Ghost,
       Global => null,
       Pre    => Valid (S),
       Post   => Budget (S, G)
                 = To_Big_Real (S.W0) * (1.0 - Real_Prefix (G, S.T))
                   + To_Big_Real (Reward (S)) * Remain (S, G, S.N_Disc);

   procedure Lemma_Budget_Unfold (S : Protocol_State; G : Gamma_Table) is
   begin
      null;
   end Lemma_Budget_Unfold;

   --  The budget after one step: Budget (S_Prev) minus the exact Eq. 1
   --  value, plus the reward if the step made a discovery. Kept as a
   --  separate lemma so that the provers see only these facts.
   procedure Lemma_Budget_After
     (S_Prev, S : Protocol_State; G : Gamma_Table; N_Old : Time_Count)
     with
       Ghost,
       Global => null,
       Pre    => Valid (S_Prev)
                 and then Valid (S)
                 and then S_Prev.T < Max_T
                 and then S.T = S_Prev.T + 1
                 and then S.W0 = S_Prev.W0
                 and then S.Alpha_Param = S_Prev.Alpha_Param
                 and then N_Old = S_Prev.N_Disc
                 and then (for all J in 1 .. N_Old =>
                             S.Disc (J) = S_Prev.Disc (J))
                 and then (S.N_Disc = N_Old
                           or else (S.N_Disc = N_Old + 1
                                    and then S.Disc (S.N_Disc) = S.T)),
       Post   => Budget (S, G)
                 = Budget (S_Prev, G) - Real_Eq1 (S_Prev, G)
                   + (if S.N_Disc = N_Old + 1
                      then To_Big_Real (Reward (S_Prev)) else 0.0);

   procedure Lemma_Budget_After
     (S_Prev, S : Protocol_State; G : Gamma_Table; N_Old : Time_Count)
   is
      W  : constant Big_Real := To_Big_Real (S.W0);
      Dr : constant Big_Real := To_Big_Real (Reward (S_Prev));
      P0 : constant Big_Real := Real_Prefix (G, S_Prev.T);
      Gt : constant Big_Real := To_Big_Real (G (S.T));
      R0 : constant Big_Real := Remain (S_Prev, G, N_Old);
      Sp : constant Big_Real := Spend (S_Prev, G, N_Old);
   begin
      Lemma_Remain_Step (S_Prev, S, G, N_Old);
      Lemma_Budget_Unfold (S, G);
      pragma Assert (Reward (S) = Reward (S_Prev));
      pragma Assert (Real_Prefix (G, S.T) = P0 + Gt);
      pragma Assert (Remain (S, G, N_Old) = R0 - Sp);
      pragma Assert (Budget (S_Prev, G) = W * (1.0 - P0) + Dr * R0);
      pragma Assert (Real_Eq1 (S_Prev, G) = W * Gt + Dr * Sp);
      pragma Assert (W * (1.0 - (P0 + Gt)) = W * (1.0 - P0) - W * Gt);

      if S.N_Disc = N_Old + 1 then
         pragma Assert (S.T - S.Disc (S.N_Disc) = 0);
         pragma Assert (Real_Prefix (G, 0) = 0.0);
         pragma Assert
           (Remain (S, G, S.N_Disc)
            = Remain (S, G, N_Old)
              + (1.0 - Real_Prefix (G, S.T - S.Disc (S.N_Disc))));
         pragma Assert (Remain (S, G, S.N_Disc) = R0 - Sp + 1.0);
         pragma Assert (Dr * (R0 - Sp + 1.0) = Dr * R0 - Dr * Sp + Dr);
         pragma Assert
           (Budget (S, G)
            = W * (1.0 - Real_Prefix (G, S.T)) + Dr * Remain (S, G, S.N_Disc));
         pragma Assert
           (W * (1.0 - Real_Prefix (G, S.T)) = W * (1.0 - (P0 + Gt)));
         pragma Assert
           (Dr * Remain (S, G, S.N_Disc) = Dr * (R0 - Sp + 1.0));
         pragma Assert
           (Budget (S, G) = W * (1.0 - (P0 + Gt)) + Dr * (R0 - Sp + 1.0));
      else
         pragma Assert (Remain (S, G, S.N_Disc) = R0 - Sp);
         pragma Assert (Dr * (R0 - Sp) = Dr * R0 - Dr * Sp);
         pragma Assert
           (Budget (S, G)
            = W * (1.0 - Real_Prefix (G, S.T)) + Dr * Remain (S, G, S.N_Disc));
         pragma Assert
           (W * (1.0 - Real_Prefix (G, S.T)) = W * (1.0 - (P0 + Gt)));
         pragma Assert (Dr * Remain (S, G, S.N_Disc) = Dr * (R0 - Sp));
         pragma Assert
           (Budget (S, G) = W * (1.0 - (P0 + Gt)) + Dr * (R0 - Sp));
      end if;
   end Lemma_Budget_After;

   -------------------------------------------------------------------------
   --  Lemma_Initialize
   -------------------------------------------------------------------------

   procedure Lemma_Initialize
     (Alpha_Param : Probability;
      W0          : Probability;
      G           : Gamma_Table)
   is
      S : constant Protocol_State := Initialize (Alpha_Param, W0);
   begin
      pragma Assert (Real_Prefix (G, 0) = 0.0);
      pragma Assert (Remain (S, G, 0) = 0.0);
      pragma Assert (Budget (S, G) = To_Big_Real (W0));
      pragma Assert (To_Real (0) * Step_Err = 0.0);
   end Lemma_Initialize;

   -------------------------------------------------------------------------
   --  Advance_Exact
   -------------------------------------------------------------------------

   procedure Advance_Exact
     (S       : in out Protocol_State;
      G       : in     Gamma_Table;
      P_Value : in     Probability;
      Alpha_T : out    Nonnegative;
      Reject  : out    Boolean)
   is
      S_Old  : constant Protocol_State := S with Ghost;
      D      : constant Long_Float := S.Alpha_Param - S.W0 with Ghost;
      Th     : constant Long_Float := Threshold (S, G) with Ghost;
      N_Old  : constant Time_Count := S.N_Disc with Ghost;
      S_Next : Protocol_State := S with Ghost;
      Base   : Big_Real with Ghost;   --  Budget - Eq1 = Budget after the step
   begin
      --  1. The unclamped Eq. 1 value is at most the wealth
      Lemma_Prefix_Le_One (G, S.W0);
      Lemma_Eq1_Error (S, G);

      S_Next.T := S.T + 1;
      Lemma_Remain_Step (S_Old, S_Next, G, N_Old);
      Lemma_Remain_Nonneg (S_Next, G, N_Old);

      Base := To_Big_Real (S.W0) * (1.0 - Real_Prefix (G, S.T + 1))
              + To_Big_Real (D) * Remain (S_Next, G, N_Old);

      pragma Assert
        (Real_Prefix (G, S.T + 1)
         = Real_Prefix (G, S.T) + To_Big_Real (G (S.T + 1)));
      pragma Assert (Reward (S_Old) = D);
      pragma Assert
        (Budget (S_Old, G) - Real_Eq1 (S_Old, G)
         = To_Big_Real (S.W0) * (1.0 - Real_Prefix (G, S.T) - To_Big_Real (G (S.T + 1)))
           + To_Big_Real (D) * (Remain (S_Old, G, N_Old)
                         - Spend (S_Old, G, N_Old)));
      pragma Assert (Budget (S_Old, G) - Real_Eq1 (S_Old, G) = Base);

      pragma Assert
        (To_Big_Real (S.W0) * (1.0 - Real_Prefix (G, S.T + 1)) >= Min_Margin);
      pragma Assert (D >= 0.0);
      pragma Assert (To_Big_Real (D) >= 0.0);
      pragma Assert (Remain (S_Next, G, N_Old) >= 0.0);
      pragma Assert (To_Big_Real (D) * Remain (S_Next, G, N_Old) >= 0.0);
      pragma Assert (Base >= Min_Margin);

      pragma Assert
        (To_Real (S.T) * Step_Err + (To_Real (N_Old) + 3.0) * U
         <= Min_Margin);
      pragma Assert (To_Big_Real (Th) <= To_Big_Real (S.W));
      Real_Le (Th, S.W);

      --  2. Advance does not clamp
      Advance (S, G, P_Value, Alpha_T, Reject);
      pragma Assert (Alpha_T = Th);

      --  3. The invariant after the step
      pragma Assert (S_Old.W <= Long_Float (S_Old.T + 1));
      Lemma_Time_Bound (S_Old.T + 1);
      Sub_Lower (S_Old.W, Alpha_T);
      pragma Assert (Reward (S_Old) = D);
      pragma Assert
        (To_Big_Real (S_Old.W - Alpha_T)
         >= Base - To_Real (S_Old.T) * Step_Err
            - (To_Real (N_Old) + 4.0) * U);
      Lemma_Budget_After (S_Old, S, G, N_Old);

      if Reject then
         Add_Lower (S_Old.W - Alpha_T, D);
         pragma Assert (Budget (S, G) = Base + To_Big_Real (D));
      else
         pragma Assert (Budget (S, G) = Base);
      end if;
      pragma Assert
        (To_Real (S.T) * Step_Err
         = To_Real (S_Old.T) * Step_Err + Step_Err);
   end Advance_Exact;

   -------------------------------------------------------------------------
   --  Run_Sequence_Exact
   -------------------------------------------------------------------------

   procedure Run_Sequence_Exact
     (S        : in out Protocol_State;
      G        : in     Gamma_Table;
      P_Values : in     P_Value_Array;
      Alphas   : in out Level_Array;
      W_Before : in out Level_Array;
      Rejects  : in out Reject_Array)
   is
   begin
      for I in P_Values'Range loop
         W_Before (I) := S.W;
         Advance_Exact (S, G, P_Values (I), Alphas (I), Rejects (I));

         pragma Loop_Invariant (Valid (S));
         pragma Loop_Invariant (S.W0 = S.W0'Loop_Entry);
         pragma Loop_Invariant (S.Alpha_Param = S.Alpha_Param'Loop_Entry);
         pragma Loop_Invariant (Margin_OK (G, S.W0));
         pragma Loop_Invariant (Exact_Inv (S, G));
         pragma Loop_Invariant
           (S.T = S.T'Loop_Entry + (I - P_Values'First + 1));
         pragma Loop_Invariant (S.Clamp_Count = S.Clamp_Count'Loop_Entry);
         pragma Loop_Invariant
           (for all J in P_Values'First .. I =>
              Alphas (J) <= W_Before (J)
              and then Rejects (J) = (P_Values (J) <= Alphas (J)));
      end loop;
   end Run_Sequence_Exact;

end Lord_Eq1_Exact;
