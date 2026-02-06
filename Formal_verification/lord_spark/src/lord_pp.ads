--  LORD++ / Alpha-Investing Core in SPARK/Ada
--  Verifies the budget invariant (H4) over IEEE 754 arithmetic

package Lord_PP
  with SPARK_Mode => On
is

   --  IEEE 754 double precision
   subtype Probability is Long_Float range 0.0 .. 1.0;
   subtype Nonnegative is Long_Float range 0.0 .. Long_Float'Last;

   type Protocol_State is record
      Wealth      : Nonnegative;     --  Current wealth W(t), must remain >= 0
      W0          : Probability;     --  Initial wealth (constant after init)
      Alpha_Param : Probability;     --  Target FDR level (constant after init)
   end record;

   --  Array types for sequence processing
   type Gamma_Array   is array (Positive range <>) of Probability;
   type P_Value_Array is array (Positive range <>) of Probability;
   type Reject_Array  is array (Positive range <>) of Boolean;

   -------------------------------------------------------------------------
   --  Safe_Gamma: Clamps any floating-point value to [0,1]
   --  Bridges the gap between transcendental gamma computation (outside
   --  SPARK) and the verified core (inside SPARK).
   -------------------------------------------------------------------------
   function Safe_Gamma (Raw : Long_Float) return Probability
     with Post => Safe_Gamma'Result in 0.0 .. 1.0;

   -------------------------------------------------------------------------
   --  Initialize: Set up protocol state (function form for driver compatibility)
   -------------------------------------------------------------------------
   function Initialize
     (Alpha_Param : Probability;
      W0          : Probability) return Protocol_State
     with
       Pre  => W0 > 0.0 
               and then W0 < Alpha_Param 
               and then Alpha_Param <= 1.0,
       Post => Initialize'Result.Wealth = W0
               and Initialize'Result.W0 = W0
               and Initialize'Result.Alpha_Param = Alpha_Param;

   -------------------------------------------------------------------------
   --  Advance: Single-step wealth update (the inductive step)
   --  Core invariant: Wealth >= 0 is preserved
   --  Also preserves W0 and Alpha_Param (they are constants)
   -------------------------------------------------------------------------
   procedure Advance
     (S       : in Out Protocol_State;
      Gamma_T : in     Probability;
      P_Value : in     Probability;
      Alpha_T : out    Nonnegative;
      Reject  : out    Boolean)
     with
       Pre  => S.Wealth >= 0.0
               and then S.W0 > 0.0
               and then S.W0 < S.Alpha_Param
               and then S.Alpha_Param <= 1.0,
       Post => S.Wealth >= 0.0
               and Alpha_T >= 0.0
               and S.W0 = S.W0'Old
               and S.Alpha_Param = S.Alpha_Param'Old;

   -------------------------------------------------------------------------
   --  Run_Sequence: Process multiple hypotheses with loop invariant
   --  Establishes full inductive closure: Wealth >= 0 throughout
   -------------------------------------------------------------------------
   procedure Run_Sequence
     (S        : in Out Protocol_State;
      Gammas   : in     Gamma_Array;
      P_Values : in     P_Value_Array;
      Rejects  : in Out Reject_Array)  --  in out to satisfy initialization
     with
       Pre  => S.Wealth >= 0.0
               and then S.W0 > 0.0
               and then S.W0 < S.Alpha_Param
               and then S.Alpha_Param <= 1.0
               and then Gammas'First = P_Values'First
               and then Gammas'Last = P_Values'Last
               and then Gammas'First = Rejects'First
               and then Gammas'Last = Rejects'Last
               and then Gammas'Length > 0,
       Post => S.Wealth >= 0.0;

end Lord_PP;
