--  Lord_Eq1: the LORD thresholds of Equation (1) in SPARK/Ada - Body

with SPARK.Lemmas.Long_Float_Arithmetic;
use  SPARK.Lemmas.Long_Float_Arithmetic;

package body Lord_Eq1
  with SPARK_Mode => On
is

   -------------------------------------------------------------------------
   --  Lemma_Next_Integer: N + 1 is exact for these small integers
   -------------------------------------------------------------------------
   procedure Lemma_Next_Integer (N : Time_Count)
     with
       Ghost,
       Global => null,
       Post   => Long_Float (N) + 1.0 = Long_Float (N + 1);

   procedure Lemma_Next_Integer (N : Time_Count) is
   begin
      null;
   end Lemma_Next_Integer;

   -------------------------------------------------------------------------
   --  Disc_Sum
   -------------------------------------------------------------------------
   function Disc_Sum
     (S : Protocol_State;
      G : Gamma_Table;
      K : Time_Count) return Long_Float
   is
   begin
      if K = 0 then
         return 0.0;
      end if;

      declare
         Prev  : constant Long_Float := Disc_Sum (S, G, K - 1);
         Term  : constant Probability := G (S.T + 1 - S.Disc (K));
         Bound : constant Long_Float := Long_Float (K - 1);
      begin
         --  Prev + Term <= Bound + Term <= Bound + 1.0 = K
         Lemma_Add_Is_Monotonic (Prev, Bound, Term);
         Lemma_Add_Is_Monotonic (Term, 1.0, Bound);
         Lemma_Next_Integer (K - 1);
         return Prev + Term;
      end;
   end Disc_Sum;

   -------------------------------------------------------------------------
   --  Initialize
   -------------------------------------------------------------------------
   function Initialize
     (Alpha_Param : Probability;
      W0          : Probability) return Protocol_State
   is
   begin
      return (T           => 0,
              W0          => W0,
              Alpha_Param => Alpha_Param,
              W           => W0,
              N_Disc      => 0,
              Disc        => (others => 1),
              Clamp_Count => 0);
   end Initialize;

   -------------------------------------------------------------------------
   --  Threshold: Eq. 1, with the sum over discoveries in time order
   -------------------------------------------------------------------------
   function Threshold
     (S : Protocol_State;
      G : Gamma_Table) return Nonnegative
   is
      T   : constant Time_Index := S.T + 1;
      Sum : Long_Float := 0.0;
   begin
      for K in 1 .. S.N_Disc loop
         Sum := Sum + G (T - S.Disc (K));
         pragma Loop_Invariant (Sum = Disc_Sum (S, G, K));
      end loop;
      pragma Assert (Sum = Disc_Sum (S, G, S.N_Disc));

      return S.W0 * G (T) + (S.Alpha_Param - S.W0) * Sum;
   end Threshold;

   -------------------------------------------------------------------------
   --  Advance
   -------------------------------------------------------------------------
   procedure Advance
     (S       : in out Protocol_State;
      G       : in     Gamma_Table;
      P_Value : in     Probability;
      Alpha_T : out    Nonnegative;
      Reject  : out    Boolean)
   is
      Eq1    : constant Nonnegative := Threshold (S, G);
      T_New  : constant Time_Index := S.T + 1;
      Bound  : constant Long_Float := Long_Float (S.T + 1);
      Rest   : Long_Float;
      Reward : Long_Float;
   begin
      --  Clamp: spend at most the current wealth
      if Eq1 <= S.W then
         Alpha_T := Eq1;
      else
         Alpha_T := S.W;
         S.Clamp_Count := S.Clamp_Count + 1;
      end if;

      --  W - Alpha_T >= 0. Rounding is monotone, so from Alpha_T <= W we
      --  get Alpha_T - Alpha_T <= W - Alpha_T, and the left side is 0.
      Lemma_Sub_Is_Monotonic (Alpha_T, S.W, Alpha_T);
      Rest := S.W - Alpha_T;
      pragma Assert (Rest >= 0.0);
      pragma Assert (Rest <= S.W);

      Reject := P_Value <= Alpha_T;
      S.T := T_New;

      if Reject then
         Reward := S.Alpha_Param - S.W0;
         pragma Assert (Reward >= 0.0 and then Reward <= 1.0);

         --  Rest + Reward <= Bound + Reward <= Bound + 1.0 = T_New + 1
         Lemma_Add_Is_Monotonic (Rest, Bound, Reward);
         pragma Assert (Rest + Reward <= Bound + Reward);
         Lemma_Add_Is_Monotonic (Reward, 1.0, Bound);
         pragma Assert (Reward + Bound <= 1.0 + Bound);
         pragma Assert (Bound + Reward = Reward + Bound);
         pragma Assert (1.0 + Bound = Bound + 1.0);
         Lemma_Next_Integer (T_New);
         pragma Assert (Bound = Long_Float (T_New));
         pragma Assert (Bound + 1.0 = Long_Float (T_New + 1));
         pragma Assert (Rest + Reward <= Long_Float (T_New + 1));

         S.W := Rest + Reward;
         S.N_Disc := S.N_Disc + 1;
         S.Disc (S.N_Disc) := T_New;
      else
         S.W := Rest;
      end if;

      pragma Assert (S.W <= Long_Float (S.T + 1));
      pragma Assert (S.N_Disc <= S.T);
      pragma Assert (S.Clamp_Count <= S.T);
      pragma Assert (for all K in 1 .. S.N_Disc => S.Disc (K) <= S.T);
      pragma Assert
        (for all K in 2 .. S.N_Disc => S.Disc (K - 1) < S.Disc (K));
   end Advance;

   -------------------------------------------------------------------------
   --  Run_Sequence
   -------------------------------------------------------------------------
   procedure Run_Sequence
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
         Advance (S, G, P_Values (I), Alphas (I), Rejects (I));

         pragma Loop_Invariant (Valid (S));
         pragma Loop_Invariant
           (S.T = S.T'Loop_Entry + (I - P_Values'First + 1));
         pragma Loop_Invariant (S.W0 = S.W0'Loop_Entry);
         pragma Loop_Invariant (S.Alpha_Param = S.Alpha_Param'Loop_Entry);
         pragma Loop_Invariant (S.Clamp_Count >= S.Clamp_Count'Loop_Entry);
         pragma Loop_Invariant
           (for all J in P_Values'First .. I =>
              Alphas (J) <= W_Before (J)
              and then Rejects (J) = (P_Values (J) <= Alphas (J)));
      end loop;
   end Run_Sequence;

end Lord_Eq1;
