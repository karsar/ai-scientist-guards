--  Lord_Eq1_Check: compare the SPARK Eq. 1 kernel (Lord_Eq1) with the
--  Haskell (Lord.hs) and Python (case_study_rerun.py) implementations.
--
--  Inputs (written by run_eq1_check.sh):
--    out/gamma_hs.csv : the gamma table of Lord.hs (j = 1 .. 2000)
--    out/steps_hs.csv : p-values, thresholds and decisions of Lord.hs
--    out/steps_py.csv : thresholds and decisions of case_study_rerun.py
--  Floating-point values are IEEE 754 bit patterns, so all comparisons
--  are exact.
--
--  Lord.hs uses gamma constant c = 0.07720838 and case_study_rerun.py uses
--  c = 0.0772. So the driver builds two gamma tables and runs one Lord_Eq1
--  state against each reference.
--
--  This driver is test code. It is not in SPARK and is not proved.

with Ada.Text_IO;                         use Ada.Text_IO;
with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;
with Ada.Unchecked_Conversion;
with Interfaces;                          use Interfaces;
with Lord_Eq1;                            use Lord_Eq1;
with Lord_PP;

procedure Lord_Eq1_Check
  with SPARK_Mode => Off
is

   function To_Float is new Ada.Unchecked_Conversion (Unsigned_64, Long_Float);

   function From_Bits (Hex : String) return Long_Float is
     (To_Float (Unsigned_64'Value ("16#" & Hex & "#")));

   --  N-th comma-separated field of Line (1-based)
   function Field (Line : String; N : Positive) return String is
      Start : Positive := Line'First;
      Count : Positive := 1;
   begin
      for I in Line'Range loop
         if Line (I) = ',' then
            if Count = N then
               return Line (Start .. I - 1);
            end if;
            Count := Count + 1;
            Start := I + 1;
         end if;
      end loop;
      return Line (Start .. Line'Last);
   end Field;

   --  gamma_j = c * log (max (j, 2)) / (j * exp (sqrt (log j))), clamped
   --  to [0, 1] with Lord_PP.Safe_Gamma. Same formula as Lord.hs and
   --  case_study_rerun.py.
   function Make_Gamma (C : Long_Float) return Gamma_Table is
      G : Gamma_Table;
   begin
      for J in Time_Index loop
         declare
            X : constant Long_Float := Long_Float (J);
         begin
            G (J) := Lord_PP.Safe_Gamma
              (C * Log (Long_Float'Max (X, 2.0))
               / (X * Exp (Sqrt (Log (X)))));
         end;
      end loop;
      return G;
   end Make_Gamma;

   G_Hs : constant Gamma_Table := Make_Gamma (0.07720838);
   G_Py : constant Gamma_Table := Make_Gamma (0.0772);

   --  Comparison statistics for one reference implementation
   type Stats is record
      Steps           : Natural := 0;
      Exact           : Natural := 0;     --  bit-identical alpha_t
      Max_Abs         : Long_Float := 0.0;
      Max_Rel         : Long_Float := 0.0;
      Decision_Diff   : Natural := 0;
      Clamp_Total     : Natural := 0;
      Eq1_Differs     : Natural := 0;     --  Alpha_T /= Threshold
   end record;

   procedure Compare
     (St : in out Stats; Spark_A, Ref_A : Long_Float;
      Spark_R, Ref_R : Boolean)
   is
      D : constant Long_Float := abs (Spark_A - Ref_A);
   begin
      St.Steps := St.Steps + 1;
      if Spark_A = Ref_A then
         St.Exact := St.Exact + 1;
      end if;
      St.Max_Abs := Long_Float'Max (St.Max_Abs, D);
      if Ref_A /= 0.0 then
         St.Max_Rel := Long_Float'Max (St.Max_Rel, D / abs Ref_A);
      end if;
      if Spark_R /= Ref_R then
         St.Decision_Diff := St.Decision_Diff + 1;
      end if;
   end Compare;

   procedure Report (Name : String; St : Stats) is
   begin
      Put_Line ("  " & Name);
      Put_Line ("    steps compared         :" & St.Steps'Image);
      Put_Line ("    bit-identical alpha_t  :" & St.Exact'Image);
      Put_Line ("    max |alpha diff|       :" & St.Max_Abs'Image);
      Put_Line ("    max relative diff      :" & St.Max_Rel'Image);
      Put_Line ("    reject decisions differ:" & St.Decision_Diff'Image);
      Put_Line ("    Clamp_Count (total)    :" & St.Clamp_Total'Image);
      Put_Line ("    Alpha_T /= Threshold   :" & St.Eq1_Differs'Image);
   end Report;

   --  Statistics: index 1 = paper tables, index 2 = Monte Carlo
   Hs_Stats, Py_Stats : array (1 .. 2) of Stats;

   Gamma_File, Hs_File, Py_File : File_Type;

   S_Hs, S_Py : Protocol_State := Initialize (0.05, 0.025);
   Current    : String (1 .. 64) := (others => ' ');
   Cur_Len    : Natural := 0;
   Cur_Kind   : Positive := 1;

begin
   Put_Line ("Lord_Eq1 agreement check");
   Put_Line ("========================");

   --  1. Gamma tables: Ada (this driver) against Lord.hs
   declare
      Exact   : Natural := 0;
      Max_Abs : Long_Float := 0.0;
   begin
      Open (Gamma_File, In_File, "out/gamma_hs.csv");
      Skip_Line (Gamma_File);
      while not End_Of_File (Gamma_File) loop
         declare
            Line : constant String := Get_Line (Gamma_File);
            J    : constant Time_Index := Time_Index'Value (Field (Line, 1));
            G    : constant Long_Float := From_Bits (Field (Line, 2));
         begin
            if G = G_Hs (J) then
               Exact := Exact + 1;
            end if;
            Max_Abs := Long_Float'Max (Max_Abs, abs (G - G_Hs (J)));
         end;
      end loop;
      Close (Gamma_File);
      Put_Line ("Gamma table (c = 0.07720838), Ada vs Lord.hs, j = 1 .. 2000:");
      Put_Line ("  bit-identical entries :" & Exact'Image);
      Put_Line ("  max |difference|      :" & Max_Abs'Image);
   end;

   --  2. Step-by-step replay of every sequence
   Open (Hs_File, In_File, "out/steps_hs.csv");
   Open (Py_File, In_File, "out/steps_py.csv");
   Skip_Line (Hs_File);
   Skip_Line (Py_File);

   New_Line;
   Put_Line ("Paper tables (SPARK alpha_t, c = 0.07720838 / c = 0.0772):");

   while not End_Of_File (Hs_File) loop
      declare
         H     : constant String := Get_Line (Hs_File);
         P     : constant String := Get_Line (Py_File);
         Name  : constant String := Field (H, 1) & "/" & Field (H, 2);
         Kind  : constant Positive := (if Field (H, 1) = "mc" then 2 else 1);
         Alpha : constant Long_Float := From_Bits (Field (H, 4));
         W0    : constant Long_Float := From_Bits (Field (H, 5));
         P_Val : constant Long_Float := From_Bits (Field (H, 6));
         A_Hs  : constant Long_Float := From_Bits (Field (H, 7));
         R_Hs  : constant Boolean := Field (H, 8) = "1";
         A_Py  : constant Long_Float := From_Bits (Field (P, 4));
         R_Py  : constant Boolean := Field (P, 5) = "1";
         A1, A2 : Nonnegative;
         R1, R2 : Boolean;
         E1, E2 : Long_Float;
      begin
         if Field (P, 1) /= Field (H, 1) or else Field (P, 3) /= Field (H, 3)
         then
            raise Program_Error with "input files out of step";
         end if;

         --  New sequence: close the previous one, start new states
         if Name /= Current (1 .. Cur_Len) then
            if Cur_Len > 0 then
               Hs_Stats (Cur_Kind).Clamp_Total :=
                 Hs_Stats (Cur_Kind).Clamp_Total + S_Hs.Clamp_Count;
               Py_Stats (Cur_Kind).Clamp_Total :=
                 Py_Stats (Cur_Kind).Clamp_Total + S_Py.Clamp_Count;
            end if;
            Cur_Kind := Kind;
            Cur_Len := Name'Length;
            Current (1 .. Cur_Len) := Name;
            S_Hs := Initialize (Alpha, W0);
            S_Py := Initialize (Alpha, W0);
            if Kind = 1 then
               New_Line;
               Put ("  " & Field (H, 1) & ":");
            end if;
         end if;

         E1 := Threshold (S_Hs, G_Hs);
         E2 := Threshold (S_Py, G_Py);
         Advance (S_Hs, G_Hs, P_Val, A1, R1);
         Advance (S_Py, G_Py, P_Val, A2, R2);

         Compare (Hs_Stats (Kind), A1, A_Hs, R1, R_Hs);
         Compare (Py_Stats (Kind), A2, A_Py, R2, R_Py);
         if A1 /= E1 then
            Hs_Stats (Kind).Eq1_Differs := Hs_Stats (Kind).Eq1_Differs + 1;
         end if;
         if A2 /= E2 then
            Py_Stats (Kind).Eq1_Differs := Py_Stats (Kind).Eq1_Differs + 1;
         end if;

         if Kind = 1 then
            Put (" " & Long_Integer (A1 * 1.0E5)'Image & "e-5"
                 & (if R1 then "*" else ""));
         end if;
      end;
   end loop;
   --  Close the last sequence
   Hs_Stats (Cur_Kind).Clamp_Total :=
     Hs_Stats (Cur_Kind).Clamp_Total + S_Hs.Clamp_Count;
   Py_Stats (Cur_Kind).Clamp_Total :=
     Py_Stats (Cur_Kind).Clamp_Total + S_Py.Clamp_Count;
   Close (Hs_File);
   Close (Py_File);
   New_Line;
   Put_Line ("  (* = discovery; values rounded to 1e-5)");

   New_Line;
   Put_Line ("Paper tables (Table 1, Table 4 both columns, Table 5):");
   Report ("vs Lord.hs", Hs_Stats (1));
   Report ("vs case_study_rerun.py", Py_Stats (1));
   New_Line;
   Put_Line ("Monte Carlo (N = 2000, 100 runs, alpha = 0.05, W0 = 0.1 alpha):");
   Report ("vs Lord.hs", Hs_Stats (2));
   Report ("vs case_study_rerun.py", Py_Stats (2));

   --  3. An IEEE 754 case where the unclamped Eq. 1 value exceeds the
   --  wealth, although gamma_1 + gamma_2 = 1.0 in floating point.
   declare
      G : Gamma_Table := (others => 0.0);
      S : Protocol_State :=
        Initialize (From_Bits ("3fef2712dbb241fc"),
                    From_Bits ("3fdf34a59a90138e"));
      A : Nonnegative;
      R : Boolean;
      E : Long_Float;
   begin
      G (1) := From_Bits ("3fdd06019087de8a");
      G (2) := From_Bits ("3fe17cff37bc10bb");
      Advance (S, G, 1.0, A, R);
      E := Threshold (S, G);
      New_Line;
      Put_Line ("Floating-point counterexample to the unclamped budget:");
      Put_Line ("  gamma_1 + gamma_2 (floating point) =" & Long_Float'Image (G (1) + G (2)));
      Put_Line ("  step 2: Eq. 1 value =" & E'Image & ", wealth =" & S.W'Image);
      Put_Line ("  Eq. 1 value - wealth =" & Long_Float'Image (E - S.W));
      Advance (S, G, 0.0, A, R);
      Put_Line ("  Alpha_T used        =" & A'Image & " (clamped to the wealth)");
      Put_Line ("  Clamp_Count         =" & S.Clamp_Count'Image);
   end;
end Lord_Eq1_Check;
