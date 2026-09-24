with Lord_Eq1; use Lord_Eq1;

package body Lord_Eq1_Capi
  with
    SPARK_Mode    => On,
    Refined_State => (Kernel => (State, Gamma, Ready))
is

   --  Static initial values, so the package needs no elaboration code
   State : Protocol_State :=
     (T           => 0,
      W0          => 0.025,
      Alpha_Param => 0.05,
      W           => 0.025,
      N_Disc      => 0,
      Disc        => (others => 1),
      Clamp_Count => 0);
   Gamma : Gamma_Table := (others => 0.0);
   Ready : Boolean := False;

   procedure Init (Alpha, W0 : Long_Float; Status : out int)
     with Refined_Global => (In_Out => (State, Ready))
   is
   begin
      if W0 > 0.0 and then W0 < Alpha and then Alpha <= 1.0 then
         State := Initialize (Alpha, W0);
         Ready := True;
         Status := Status_OK;
      else
         Status := Status_Invalid;
      end if;
   end Init;

   procedure Set_Gamma (J : int; Value : Long_Float; Status : out int)
     with Refined_Global => (In_Out => Gamma)
   is
   begin
      if J >= 1 and then J <= Max_T
        and then Value >= 0.0 and then Value <= 1.0
      then
         Gamma (Time_Index (J)) := Value;
         Status := Status_OK;
      else
         Status := Status_Invalid;
      end if;
   end Set_Gamma;

   procedure Next_Threshold (Alpha_T : out Long_Float; Status : out int)
     with Refined_Global => (Input => (State, Gamma, Ready))
   is
   begin
      Alpha_T := 0.0;
      if not Ready then
         Status := Status_Not_Set;
      elsif not Valid (State) or else State.T = Max_T then
         Status := Status_Full;
      else
         Alpha_T := Threshold (State, Gamma);
         Status := Status_OK;
      end if;
   end Next_Threshold;

   procedure Step
     (P_Value : Long_Float;
      Alpha_T : out Long_Float;
      Reject  : out int;
      Status  : out int)
     with Refined_Global => (In_Out => State, Input => (Gamma, Ready))
   is
      A : Nonnegative;
      R : Boolean;
   begin
      Alpha_T := 0.0;
      Reject := 0;
      if not Ready then
         Status := Status_Not_Set;
      elsif not (P_Value >= 0.0 and then P_Value <= 1.0) then
         Status := Status_Invalid;
      elsif not Valid (State) or else State.T = Max_T then
         Status := Status_Full;
      else
         Advance (State, Gamma, P_Value, A, R);
         Alpha_T := A;
         Reject := (if R then 1 else 0);
         Status := Status_OK;
      end if;
   end Step;

   procedure Clamp_Count (Count : out int)
     with Refined_Global => (Input => State)
   is
   begin
      Count := int (State.Clamp_Count);
   end Clamp_Count;

   procedure Wealth (W : out Long_Float)
     with Refined_Global => (Input => State)
   is
   begin
      W := State.W;
   end Wealth;

end Lord_Eq1_Capi;
