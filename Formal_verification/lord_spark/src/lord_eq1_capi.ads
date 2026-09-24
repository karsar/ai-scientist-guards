--  Lord_Eq1_Capi: C-ABI surface of the verified Eq. 1 kernel (Lord_Eq1).
--
--  The package holds one protocol state and one gamma table. The exported
--  procedures use the C calling convention, so a Haskell (or C) caller can
--  run the proved kernel through its foreign-function interface. In C:
--
--    void lord_eq1_init       (double alpha, double w0, int *status);
--    void lord_eq1_set_gamma  (int j, double gamma_j, int *status);
--    void lord_eq1_threshold  (double *alpha_t, int *status);
--    void lord_eq1_advance    (double p, double *alpha_t, int *reject,
--                              int *status);
--    void lord_eq1_clamp_count (int *count);
--    void lord_eq1_wealth     (double *w);
--
--  The procedures have no preconditions. They check every input from the
--  caller at run time and report the result in Status, so the proof does
--  not depend on what the caller passes.

with Interfaces.C; use Interfaces.C;

package Lord_Eq1_Capi
  with
    SPARK_Mode     => On,
    Abstract_State => Kernel,
    Initializes    => Kernel
is

   --  Status codes
   Status_OK      : constant int := 0;
   Status_Invalid : constant int := 1;   --  argument out of range
   Status_Not_Set : constant int := 2;   --  lord_eq1_init not called yet
   Status_Full    : constant int := 3;   --  Max_T steps already done

   --  Start a new run: W = W0, no discoveries, Clamp_Count = 0.
   --  Needs 0 < W0 < alpha <= 1. The gamma table is kept.
   procedure Init (Alpha, W0 : Long_Float; Status : out int)
     with
       Export, Convention => C, External_Name => "lord_eq1_init",
       Global => (In_Out => Kernel);

   --  Set gamma_J, for J in 1 .. Max_T and a value in [0, 1]
   procedure Set_Gamma (J : int; Value : Long_Float; Status : out int)
     with
       Export, Convention => C, External_Name => "lord_eq1_set_gamma",
       Global => (In_Out => Kernel);

   --  The Eq. 1 value for the next step (no clamp, no state change)
   procedure Next_Threshold (Alpha_T : out Long_Float; Status : out int)
     with
       Export, Convention => C, External_Name => "lord_eq1_threshold",
       Global => (Input => Kernel),
       Post   => (if Status = Status_OK then Alpha_T >= 0.0);

   --  One step of Lord_Eq1.Advance. Reject is 1 for a discovery, else 0.
   procedure Step
     (P_Value : Long_Float;
      Alpha_T : out Long_Float;
      Reject  : out int;
      Status  : out int)
     with
       Export, Convention => C, External_Name => "lord_eq1_advance",
       Global => (In_Out => Kernel),
       Post   => (if Status = Status_OK then Alpha_T >= 0.0);

   --  Number of steps where the clamp min (Eq1, W) acted
   procedure Clamp_Count (Count : out int)
     with
       Export, Convention => C, External_Name => "lord_eq1_clamp_count",
       Global => (Input => Kernel);

   --  Current wealth W
   procedure Wealth (W : out Long_Float)
     with
       Export, Convention => C, External_Name => "lord_eq1_wealth",
       Global => (Input => Kernel),
       Post   => W >= 0.0;

end Lord_Eq1_Capi;
