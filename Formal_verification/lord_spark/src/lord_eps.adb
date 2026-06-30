package body Lord_Eps
  with SPARK_Mode => On
is

   procedure Sum_Thresholds
     (W0        : in     Prob;
      Gammas    : in     Gamma_Array;
      Sum_Alpha :    out Long_Float;
      W_Final   :    out Prob)
   is
      W     : Prob := W0;           --  current wealth W_t, always in [0,1]
      G1    : Prob;                 --  1 - gamma_t, in [0,1]
      Alpha : Long_Float;           --  gamma_t * W_t, non-negative
   begin
      Sum_Alpha := 0.0;

      for I in Gammas'Range loop
         pragma Loop_Invariant (Sum_Alpha >= 0.0);

         --  alpha_t = gamma * W >= 0 (product of probabilities).
         Alpha := Gammas (I) * W;
         pragma Assert (Alpha >= 0.0);

         --  Wealth update as a product (1-gamma)*W: provably in [0,1] under
         --  IEEE-754, since the product of two values in [0,1] rounds to a
         --  value in [0,1]. This is the same multiplicative form whose
         --  non-negativity Lord_PP verifies for H4.
         G1 := 1.0 - Gammas (I);
         W  := G1 * W;

         Sum_Alpha := Sum_Alpha + Alpha;
      end loop;

      W_Final := W;
   end Sum_Thresholds;

end Lord_Eps;
