--  LORD++ / Alpha-Investing Core in SPARK/Ada - Body

package body Lord_PP
  with SPARK_Mode => On
is

   -------------------------------------------------------------------------
   --  Safe_Gamma: Clamps any floating-point value to [0,1]
   -------------------------------------------------------------------------
   function Safe_Gamma (Raw : Long_Float) return Probability is
   begin
      if Raw <= 0.0 then
         return 0.0;
      elsif Raw >= 1.0 then
         return 1.0;
      else
         return Raw;
      end if;
   end Safe_Gamma;

   -------------------------------------------------------------------------
   --  Initialize (function form)
   -------------------------------------------------------------------------
   function Initialize
     (Alpha_Param : Probability;
      W0          : Probability) return Protocol_State
   is
   begin
      return (Wealth      => W0,
              W0          => W0,
              Alpha_Param => Alpha_Param);
   end Initialize;

   -------------------------------------------------------------------------
   --  Advance: Single-step wealth update
   --  Key insight: W := (1 - gamma) * W is provably non-negative under
   --  IEEE 754 (product of non-negatives), whereas W := W - gamma * W
   --  can round to a small negative value.
   -------------------------------------------------------------------------
   procedure Advance
     (S       : in Out Protocol_State;
      Gamma_T : in     Probability;
      P_Value : in     Probability;
      Alpha_T : out    Nonnegative;
      Reject  : out    Boolean)
   is
      One_Minus_G : Long_Float;
      Reward      : Long_Float;
   begin
      --  Compute complement (in [0,1] since Gamma_T in [0,1])
      One_Minus_G := 1.0 - Gamma_T;
      pragma Assert (One_Minus_G >= 0.0);
      pragma Assert (One_Minus_G <= 1.0);

      --  Compute threshold: alpha_t = gamma_t * W(t)
      Alpha_T := Gamma_T * S.Wealth;
      pragma Assert (Alpha_T >= 0.0);

      --  Update wealth via multiplication (key to IEEE 754 safety)
      --  W(t+1) = (1 - gamma_t) * W(t)
      S.Wealth := One_Minus_G * S.Wealth;
      pragma Assert (S.Wealth >= 0.0);  --  H4: budget invariant

      --  Make rejection decision
      Reject := P_Value <= Alpha_T;

      --  On rejection, receive reward (alpha - W0)
      if Reject then
         Reward := S.Alpha_Param - S.W0;
         pragma Assert (Reward >= 0.0);  --  Since W0 < Alpha_Param
         S.Wealth := S.Wealth + Reward;
         pragma Assert (S.Wealth >= 0.0);  --  H4 maintained after reward
      end if;
   end Advance;

   -------------------------------------------------------------------------
   --  Run_Sequence: Full inductive closure via loop invariant
   -------------------------------------------------------------------------
   procedure Run_Sequence
     (S        : in Out Protocol_State;
      Gammas   : in     Gamma_Array;
      P_Values : in     P_Value_Array;
      Rejects  : in Out Reject_Array)
   is
      Alpha_T : Nonnegative;
   begin
      for I in Gammas'Range loop
         pragma Loop_Invariant (S.Wealth >= 0.0);
         pragma Loop_Invariant (S.W0 > 0.0);
         pragma Loop_Invariant (S.W0 < S.Alpha_Param);
         pragma Loop_Invariant (S.Alpha_Param <= 1.0);
         
         Advance (S, Gammas (I), P_Values (I), Alpha_T, Rejects (I));
      end loop;
   end Run_Sequence;

end Lord_PP;
