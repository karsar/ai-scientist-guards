package body Lord_Capi
  with SPARK_Mode => On
is

   function New_Wealth (Wealth, Gamma : Long_Float) return Long_Float is
      One_Minus_G : constant Long_Float := 1.0 - Gamma;
   begin
      return One_Minus_G * Wealth;
   end New_Wealth;

   function Alpha (Wealth, Gamma : Long_Float) return Long_Float is
   begin
      return Gamma * Wealth;
   end Alpha;

end Lord_Capi;
