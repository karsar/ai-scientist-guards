--  Lord_Capi: C-ABI surface of the verified LORD++ wealth arithmetic.
--
--  These are the budget-critical operations whose non-negativity GNATprove
--  proves over IEEE-754 (Lord_PP). Exporting them with C convention lets the
--  Haskell orchestrator call the *verified* arithmetic directly via FFI,
--  instead of keeping a separate unverified copy of the wealth update. The
--  multiplicative form (1-gamma)*W is the one proved non-negative under
--  rounding.

package Lord_Capi
  with SPARK_Mode => On
is

   --  New wealth W_{t+1} = (1 - gamma) * W_t. Proved >= 0 (product of
   --  non-negatives is non-negative under IEEE-754).
   function New_Wealth (Wealth, Gamma : Long_Float) return Long_Float
     with
       Export, Convention => C, External_Name => "lord_new_wealth",
       Pre  => Wealth >= 0.0 and then Wealth <= 1.0
               and then Gamma >= 0.0 and then Gamma <= 1.0,
       Post => New_Wealth'Result >= 0.0;

   --  Threshold alpha_t = gamma * W_t. Proved >= 0.
   function Alpha (Wealth, Gamma : Long_Float) return Long_Float
     with
       Export, Convention => C, External_Name => "lord_alpha",
       Pre  => Wealth >= 0.0 and then Wealth <= 1.0
               and then Gamma >= 0.0 and then Gamma <= 1.0,
       Post => Alpha'Result >= 0.0;

end Lord_Capi;
