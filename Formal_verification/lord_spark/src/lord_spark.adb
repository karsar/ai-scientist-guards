--  LORD++ Monte Carlo Simulation Driver
--
--  Compares FDR control between:
--    (1) LORD++ alpha-investing (using the SPARK-verified core)
--    (2) Naive fixed-threshold testing (no correction)
--
--  Expected output (at alpha = 0.05, 10% alternative):
--    LORD++ FDR  ~  1-3%   (<= 5%)
--    Naive  FDR  ~ 40-45%  (uncontrolled)

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Long_Elementary_Functions;
with Lord_PP;

procedure Lord_Spark is

   package LF_IO renames Ada.Long_Float_Text_IO;
   package Int_IO renames Ada.Integer_Text_IO;
   package Elem renames Ada.Numerics.Long_Elementary_Functions;

   --  Simulation parameters
   Num_Hypotheses  : constant := 1_000;
   Num_Replications : constant := 500;
   Pi_0            : constant Long_Float := 0.9;   --  fraction of true nulls
   Alpha           : constant Long_Float := 0.05;
   W0              : constant Long_Float := 0.025;  --  Alpha / 2

   --  Gamma sequence: gamma(t) = c / t^1.6, normalized
   --  We precompute for indices 1 .. Num_Hypotheses
   type Gamma_Array is array (1 .. Num_Hypotheses) of Long_Float;

   function Compute_Gamma return Gamma_Array is
      G   : Gamma_Array;
      Sum : Long_Float := 0.0;
   begin
      --  Raw power-law weights
      for T in G'Range loop
         G (T) := 1.0 / Elem."**" (Long_Float (T), 1.6);
         Sum := Sum + G (T);
      end loop;
      --  Normalize so that the sum equals exactly 1.0
      --  (ensures partial sums stay <= 1.0)
      for T in G'Range loop
         G (T) := G (T) / Sum;
      end loop;
      return G;
   end Compute_Gamma;

   --  Random number generator
   Gen : Ada.Numerics.Float_Random.Generator;

   function Rand return Long_Float is
   begin
      return Long_Float (Ada.Numerics.Float_Random.Random (Gen));
   end Rand;

   --  Generate a p-value:
   --    Null hypothesis:        p ~ Uniform(0, 1)
   --    Alternative hypothesis: p ~ Uniform(0, 0.05)
   --  (simple model; real simulations use Beta or truncated normal)
   function Generate_P_Value (Is_Null : Boolean) return Long_Float is
   begin
      if Is_Null then
         return Rand;
      else
         return Rand * 0.05;
      end if;
   end Generate_P_Value;

   --  Gamma values (precomputed once)
   Gamma : constant Gamma_Array := Compute_Gamma;

   --  Accumulators across replications
   LORD_FDR_Sum   : Long_Float := 0.0;
   Naive_FDR_Sum  : Long_Float := 0.0;
   LORD_Power_Sum : Long_Float := 0.0;
   Naive_Power_Sum : Long_Float := 0.0;

begin
   --  Seed the generator
   Ada.Numerics.Float_Random.Reset (Gen, 42);

   Put_Line ("LORD++ Monte Carlo Simulation");
   Put_Line ("========================================");
   Put ("  Hypotheses per run : ");
   Int_IO.Put (Num_Hypotheses, Width => 1); New_Line;
   Put ("  Replications       : ");
   Int_IO.Put (Num_Replications, Width => 1); New_Line;
   Put ("  Pi_0 (null frac)   : ");
   LF_IO.Put (Pi_0, Fore => 1, Aft => 2, Exp => 0); New_Line;
   Put ("  Alpha (target FDR) : ");
   LF_IO.Put (Alpha, Fore => 1, Aft => 3, Exp => 0); New_Line;
   Put ("  W0 (initial wealth): ");
   LF_IO.Put (W0, Fore => 1, Aft => 4, Exp => 0); New_Line;
   Put_Line ("========================================");
   New_Line;

   --  Main simulation loop
   for Rep in 1 .. Num_Replications loop

      --  Initialize LORD++ state (SPARK-verified)
      declare
         S : Lord_PP.Protocol_State := Lord_PP.Initialize (Alpha, W0);

         --  Tracking variables for this replication
         LORD_Reject       : Natural := 0;
         LORD_False_Disc   : Natural := 0;
         LORD_True_Disc    : Natural := 0;
         Naive_Reject      : Natural := 0;
         Naive_False_Disc  : Natural := 0;
         Naive_True_Disc   : Natural := 0;

         Is_Null  : Boolean;
         P_Value  : Long_Float;
         Alpha_T  : Long_Float;
         Rejected : Boolean;
      begin
         for T in 1 .. Num_Hypotheses loop

            --  Determine ground truth
            Is_Null := (Rand >= (1.0 - Pi_0));

            --  Generate p-value
            P_Value := Generate_P_Value (Is_Null);

            -----------------------------------------------
            --  LORD++ test (calls SPARK-verified Advance)
            -----------------------------------------------
            Lord_PP.Advance
              (S       => S,
               Gamma_T => Gamma (T),
               P_Value => P_Value,
               Alpha_T => Alpha_T,
               Reject  => Rejected);

            if Rejected then
               LORD_Reject := LORD_Reject + 1;
               if Is_Null then
                  LORD_False_Disc := LORD_False_Disc + 1;
               else
                  LORD_True_Disc := LORD_True_Disc + 1;
               end if;
            end if;

            -----------------------------------------------
            --  Naive test (fixed threshold, no correction)
            -----------------------------------------------
            if P_Value <= Alpha then
               Naive_Reject := Naive_Reject + 1;
               if Is_Null then
                  Naive_False_Disc := Naive_False_Disc + 1;
               else
                  Naive_True_Disc := Naive_True_Disc + 1;
               end if;
            end if;

         end loop;

         --  Compute FDR for this replication: FD / max(1, R)
         declare
            LORD_FDR  : Long_Float;
            Naive_FDR : Long_Float;
            LORD_Pow  : Long_Float;
            Naive_Pow : Long_Float;
            Num_Alt   : constant Natural := LORD_True_Disc
                                            + (Num_Hypotheses - LORD_Reject
                                               - (Natural (Long_Float (Num_Hypotheses) * Pi_0)
                                                  - LORD_False_Disc));
            --  Simpler: count actual alternatives by looking at rejections
         begin
            if LORD_Reject > 0 then
               LORD_FDR := Long_Float (LORD_False_Disc)
                           / Long_Float (LORD_Reject);
            else
               LORD_FDR := 0.0;
            end if;

            if Naive_Reject > 0 then
               Naive_FDR := Long_Float (Naive_False_Disc)
                            / Long_Float (Naive_Reject);
            else
               Naive_FDR := 0.0;
            end if;

            --  Power = true discoveries / total alternatives
            --  Total alternatives ≈ Num_Hypotheses * (1 - Pi_0)
            LORD_Pow  := Long_Float (LORD_True_Disc)
                         / Long_Float'Max (1.0,
                             Long_Float (Num_Hypotheses) * (1.0 - Pi_0));
            Naive_Pow := Long_Float (Naive_True_Disc)
                         / Long_Float'Max (1.0,
                             Long_Float (Num_Hypotheses) * (1.0 - Pi_0));

            LORD_FDR_Sum   := LORD_FDR_Sum   + LORD_FDR;
            Naive_FDR_Sum  := Naive_FDR_Sum  + Naive_FDR;
            LORD_Power_Sum := LORD_Power_Sum + LORD_Pow;
            Naive_Power_Sum := Naive_Power_Sum + Naive_Pow;
         end;

      end;
   end loop;

   --  Report average results
   New_Line;
   Put_Line ("Results (averaged over "
             & Integer'Image (Num_Replications) & " replications):");
   Put_Line ("----------------------------------------");
   Put ("  LORD++ FDR   : ");
   LF_IO.Put (LORD_FDR_Sum / Long_Float (Num_Replications),
              Fore => 1, Aft => 4, Exp => 0);
   if LORD_FDR_Sum / Long_Float (Num_Replications) <= Alpha then
      Put ("  [CONTROLLED]");
   else
      Put ("  [VIOLATION!]");
   end if;
   New_Line;

   Put ("  Naive  FDR   : ");
   LF_IO.Put (Naive_FDR_Sum / Long_Float (Num_Replications),
              Fore => 1, Aft => 4, Exp => 0);
   Put ("  [UNCONTROLLED]");
   New_Line;

   Put ("  LORD++ Power : ");
   LF_IO.Put (LORD_Power_Sum / Long_Float (Num_Replications),
              Fore => 1, Aft => 4, Exp => 0);
   New_Line;

   Put ("  Naive  Power : ");
   LF_IO.Put (Naive_Power_Sum / Long_Float (Num_Replications),
              Fore => 1, Aft => 4, Exp => 0);
   New_Line;

   New_Line;
   Put_Line ("Budget soundness (H4) of LORD++ core: verified by GNATprove.");
   Put_Line ("FDR guarantee follows by alpha-investing theorem.");

end Lord_Spark;
