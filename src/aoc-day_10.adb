with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Interfaces;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.String_Split;
with Aoc.Common;

use Ada;

package body Aoc.Day_10 is

   function Parse_Machine (Line : String) return Machine is
      use Ada.Strings.Fixed;
      use GNAT.String_Split;

      Light_Start : constant Light_Index :=
        Light_Index (Index (Line, "[") + 1);
      Light_End : constant Light_Index :=
        Light_Index (Index (Line, "]") - 1);
      Jolt_Start : constant Positive := Positive (Index (Line, "{"));
      Jolt_End : constant Positive := Positive (Index (Line, "}"));
      Button_Count : constant Button_Index := Button_Index (Count (Line, "("));
      Light_Count : constant Light_Index := Light_End - Light_Start + 1;

      M : Machine := (
        Light_Count => Light_Count,
        Button_Count => Button_Count,
        Lights => (others => False),
        Buttons => (others => (others => False)),
        Joltage_Reqs => (others => 1));
      Next_Paren : Natural := Index (Line, "(");

      Jolt_Str : constant String := Line (Jolt_Start + 1 .. Jolt_End - 1);
      Jolts : constant Slice_Set := Create (Jolt_Str, ",", Multiple);
   begin
      for I in 1 .. Light_Count loop
         M.Lights (I) := (Line (Integer (Light_Start + I - 1)) = '#');
      end loop;

      for Btn_Id in 1 .. Button_Count loop
         declare
            Closing : constant Natural := Index (
              Line, ")", Positive (Next_Paren));
            Btn_Str : constant String := Line (Next_Paren + 1 .. Closing - 1);
            Nums : constant Slice_Set := Create (Btn_Str, ",", Multiple);
         begin
            for I in 1 .. Slice_Count (Nums) loop
               declare
                  Num : constant Natural := Natural'Value (Slice (Nums, I));
                  Light_Id : constant Light_Index := Light_Index (Num + 1);
               begin
                  M.Buttons (Btn_Id, Light_Id) := True;
               end;
            end loop;

            if Btn_Id /= Button_Count then
               Next_Paren := Index (Line, "(", Closing);
            end if;
         end;
      end loop;

      for I in 1 .. Slice_Count (Jolts) loop
         M.Joltage_Reqs (Light_Index (I)) := Joltage'Value (Slice (Jolts, I));
      end loop;

      return M;
   end Parse_Machine;

   function Image (L : Light_Mask) return String is
      Ret : String (1 .. L'Length + 2);
   begin
      Ret (1) := '[';
      for I in L'Range loop
         Ret (Integer (I) + 1) := (if L (I) then '#' else '.');
      end loop;
      Ret (L'Length + 2) := ']';
      return Ret;
   end Image;

   function Presses (M : Machine) return Int_Mat is
      Mat : Int_Mat (1 .. M.Light_Count, 1 .. M.Button_Count + 1);
   begin
      for I in 1 .. M.Light_Count loop
         for J in 1 .. M.Button_Count loop
            Mat (I, J) := (if M.Buttons (J, I) then 1 else 0);
         end loop;
         Mat (I, M.Button_Count + 1) := M.Joltage_Reqs (I);
      end loop;
      return Mat;
   end Presses;

   function GCD (A : Integer; B : Integer) return Integer is
      First : Integer := A;
      Second : Integer := B;
   begin
      while Second /= 0 loop
         declare
            Temp : constant Integer := Second;
         begin
            Second := First mod Second;
            First := Temp;
         end;
      end loop;
      return First;
   end GCD;

   procedure Swap_Rows
     (M : in out Int_Mat;
      A : Positive;
      B : Positive)
   is
   begin
      if A = B then
         return;
      end if;

      for I in M'Range (2) loop
         declare
            Temp : constant Integer := M (A, I);
         begin
            M (A, I) := M (B, I);
            M (B, I) := Temp;
         end;
      end loop;
   end Swap_Rows;

   procedure Add_Rows
     (M : in out Int_Mat;
      Target : Positive;
      Source : Positive)
   is
   begin
      for I in M'Range (2) loop
         M (Target, I) := M (Target, I) + M (Source, I);
      end loop;
   end Add_Rows;

   procedure Mul_Row
     (M : in out Int_Mat;
      Row : Positive;
      Scale : Integer)
   is
      pragma Assert (Scale /= 0);
   begin
      for I in M'Range (2) loop
         M (Row, I) := M (Row, I) * Scale;
      end loop;
   end Mul_Row;

   procedure Add_Mul_Rows
     (M : in out Int_Mat;
      Target : Positive;
      Source : Positive;
      Scale_Target : Integer;
      Scale_Source : Integer)
   is
   begin
      for I in M'Range (2) loop
         M (Target, I) := M (Target, I) * Scale_Target +
           M (Source, I) * Scale_Source;
      end loop;
   end Add_Mul_Rows;

   procedure Normalize_Row
     (M : in out Int_Mat;
      Row : Positive)
   is
      G : Integer := M (Row, 1);
   begin
      for Col in 2 .. M'Last (2) loop
         G := GCD (G, M (Row, Col));
      end loop;

      G := abs G;

      if G >= 2 then
         for Col in M'Range (2) loop
            M (Row, Col) := M (Row, Col) / G;
         end loop;
      end if;
   end Normalize_Row;

   function Get_Row (M : Int_Mat; R : Positive) return Int_Mat_Row is
      Row : Int_Mat_Row (M'Range (2));
   begin
      for Col in M'Range (2) loop
         Row (Col) := M (R, Col);
      end loop;
      return Row;
   end Get_Row;

   function Get_Col (M : Int_Mat; C : Positive) return Int_Mat_Col is
      Col : Int_Mat_Col (M'Range (1));
   begin
      for Row in M'Range (1) loop
         Col (Row) := M (Row, C);
      end loop;
      return Col;
   end Get_Col;

   function Sum_Rows (M : Int_Mat) return Int_Mat_Row is
      Row : Int_Mat_Row (M'Range (2));
   begin
      for Col in M'Range (2) loop
         declare
            Col_Sum : Integer := 0;
         begin
            for R in M'Range (1) loop
               Col_Sum := Col_Sum + M (R, Col);
            end loop;
            Row (Col) := Col_Sum;
         end;
      end loop;
      return Row;
   end Sum_Rows;

   function Image (M : Int_Mat) return String is
      use Ada.Characters.Latin_1;
      use Ada.Strings.Unbounded;
      Img : Unbounded_String;
   begin
      for Row in M'Range (1) loop
         Append (Img, "[ ");
         for Col in M'Range (2) loop
            if Col > M'First (2) then
               Append (Img, " ");
            end if;
            Append (Img, M (Row, Col)'Image);
         end loop;
         Append (Img, " ]");
         if Row /= M'Last (1) then
            Append (Img, LF);
         end if;
      end loop;
      return To_String (Img);
   end Image;

   function Image (R : Int_Mat_Row) return String is
      use Ada.Strings.Unbounded;
      Img : Unbounded_String;
   begin
      Append (Img, "[ ");
      for Ind in R'Range loop
         if Ind > R'First then
            Append (Img, " ");
         end if;
         Append (Img, R (Ind)'Image);
      end loop;
      Append (Img, " ]");
      return To_String (Img);
   end Image;

   function Image (C : Int_Mat_Col) return String is
      use Ada.Characters.Latin_1;
      use Ada.Strings.Unbounded;
      Img : Unbounded_String;
   begin
      for Ind in C'Range loop
         Append (Img, "[ ");
         Append (Img, C (Ind)'Image);
         Append (Img, " ]");
         if Ind /= C'Last then
            Append (Img, LF);
         end if;
      end loop;
      return To_String (Img);
   end Image;

   function RREF (M : in out Int_Mat) return RREF_Data is
      pragma Assert (M'Length (2) > 1);

      --  Finds first nonzero and swaps it with the next pivot position.
      --  Returns true if the pivot was found. Store a copy of Next_Pivot
      --  before calling to get the current pivot position
      function Find_Pivot (Col : Positive) return Boolean;

      First_Col : constant Positive := M'First (2);
      Last_Col : constant Positive := M'Last (2) - 1;
      Augmented_Col : constant Positive := M'Last (2);
      First_Row : constant Positive := M'First (1);
      Last_Row : constant Positive := M'Last (1);

      Next_Pivot_Row : Positive := First_Row;
      Total_Pivots : Natural := 0;
      Is_Pivot : Col_Is_Pivot (First_Col .. Last_Col) := (others => False);

      function Find_Pivot (Col : Positive) return Boolean is
      begin
         for Row in Next_Pivot_Row .. Last_Row loop
            if M (Row, Col) /= 0 then
               --  Make sure the pivot is positive
               if M (Row, Col) < 0 then
                  Mul_Row (M, Row, -1);
               end if;
               Swap_Rows (M, Row, Next_Pivot_Row);
               Next_Pivot_Row := Next_Pivot_Row + 1;
               return True;
            end if;
         end loop;
         return False;
      end Find_Pivot;
   begin
      for Col in First_Col .. Last_Col loop
         declare
            Pivot : constant Positive := Next_Pivot_Row;
         begin
            if Find_Pivot (Col) then
               for Row in M'Range (1) loop
                  if Row /= Pivot and then M (Row, Col) /= 0 then
                     declare
                        Row_Val : constant Integer := M (Row, Col);
                        Piv_Val : constant Integer := M (Pivot, Col);
                        G : constant Integer := GCD (Row_Val, Piv_Val);
                     begin
                        Add_Mul_Rows (M, Row, Pivot,
                          Piv_Val / G, (-Row_Val) / G);
                     end;
                  end if;
               end loop;
               Total_Pivots := Total_Pivots + 1;
               Is_Pivot (Col) := True;
            end if;
         end;
      end loop;

      --  Divide each row by its cumulative GCD
      for Row in M'Range (1) loop
         Normalize_Row (M, Row);
      end loop;

      declare
         Zero_Rows : Natural := 0;
         Has_Solutions : Boolean := True;
      begin
         --  Count number of zero rows
         for Row in reverse M'Range (1) loop
            if (for all Col in
               First_Col .. Last_Col => M (Row, Col) = 0)
            then
               Zero_Rows := Zero_Rows + 1;
               Has_Solutions := Has_Solutions and then
                  M (Row, Augmented_Col) = 0;
            else
               exit;
            end if;
         end loop;

         return (Lower => First_Col,
           Upper => Last_Col,
           Pivots => Total_Pivots,
           Free_Vars => Natural (Last_Col) - Total_Pivots,
           Is_Pivot => Is_Pivot,
           Zero_Rows => Zero_Rows,
           Has_Solutions => Has_Solutions);
      end;
   end RREF;

   function Paramaterize
     (Data : RREF_Data; M : Int_Mat)
      return Paramaterized
   is
      pragma Assert (Data.Free_Vars > 0);

      --  One row for each pivot variable
      Last_Row : constant Positive := Positive (Data.Pivots);

      --  One col for each free var + constants
      Last_Col : constant Positive := Positive (Data.Free_Vars + 1);

      P : Int_Mat (1 .. Last_Row, 1 .. Last_Col);
      Mappings : Matrix_Mapping (1 .. Data.Free_Vars);
      Pivot_Vals : Int_Mat_Col (1 .. Last_Row);
      Current_Var : Positive := 1;
   begin
      --  Move free column coefficients to new paramaterized matrix
      for Col in Data.Lower .. Data.Upper loop
         if not Data.Is_Pivot (Col) then
            for Row in P'Range (1) loop
               P (Row, Current_Var) := M (M'First (1) + Row - 1, Col);
               Mappings (Current_Var) := Col;
            end loop;
            Current_Var := Current_Var + 1;
         end if;
      end loop;

      for Row in P'Range (1) loop
         --  Move constant values in final row
         P (Row, Last_Col) := M (M'First (1) + Row - 1, M'Last (2));

         --  Record the values of each pivot. This is relevant
         --  because not all pivot values will be 1 due to divisibility
         --  requirements
         for Col in M'Range (2) loop
            if M (Row, Col) /= 0 then
               Pivot_Vals (Row) := M (Row, Col);
               exit;
            end if;
            pragma Assert (Col < M'Last (2));
         end loop;
      end loop;

      return (M => P,
              Rows => Last_Row,
              Cols => Last_Col,
              Vars => Data.Free_Vars,
              Var_Mapping => Mappings,
              Pivot_Vals => Pivot_Vals);
   end Paramaterize;

   function Min_Presses (M : Machine) return Natural is
      function Max_For_Button (Button : Button_Index) return Joltage;
      function Calc_Min (P : Paramaterized) return Natural;

      --  Find the maximum number of times a button can be pressed
      function Max_For_Button (Button : Button_Index) return Joltage is
         Max_Presses : Joltage := 0;
      begin
         for Light in 1 .. M.Light_Count loop
            if M.Buttons (Button, Light) and then
              M.Joltage_Reqs (Light) > Max_Presses
            then
               Max_Presses := M.Joltage_Reqs (Light);
            end if;
         end loop;
         return Max_Presses;
      end Max_For_Button;

      function Calc_Min (P : Paramaterized) return Natural is
         Press_Min : Natural := Natural'Last;
         Press_Bounds : Joltages (1 .. P.Vars);
         Current_Test : Joltages (1 .. P.Vars);

         --  Sum the number of presses for the given test
         procedure Run_Test is
            Sum : Natural := 0;
         begin
            for I in Current_Test'Range loop
               Sum := Sum + Current_Test (I);
            end loop;

            for Row in P.M'Range (1) loop
               declare
                  Row_Sum : Integer := P.M (Row, P.M'Last (2));
               begin
                  for Col in 1 .. P.Vars loop
                     declare
                        Computed : constant Integer := P.M (Row, Col) *
                          Current_Test (Col);
                     begin
                        Row_Sum := Row_Sum - Computed;
                     end;
                  end loop;

                  --  Check if the current press permutation conforms to
                  --  the constraints given by the paramaterized matrix.
                  --  Each press value must be whole and non-negative.
                  if Row_Sum < 0 or else
                    Row_Sum mod P.Pivot_Vals (Row) /= 0
                  then
                     return;
                  end if;

                  Sum := Sum + Natural (Row_Sum / P.Pivot_Vals (Row));
               end;
            end loop;

            Press_Min := Natural'Min (Press_Min, Sum);
         end Run_Test;

         --  Iterate through press bound permutations
         procedure Iterate_Test (Var : Positive) is
         begin
            for I in 0 .. Press_Bounds (Var) loop
               Current_Test (Var) := I;
               if Var < P.Vars then
                  Iterate_Test (Var + 1);
               else
                  Run_Test;
               end if;
            end loop;
         end Iterate_Test;

      begin
         for Var in 1 .. P.Vars loop
            Press_Bounds (Var) := Max_For_Button (P.Var_Mapping (Var));
         end loop;

         Iterate_Test (1);
         return Press_Min;
      end Calc_Min;

      A : Int_Mat := Presses (M);
      Data : constant RREF_Data := RREF (A);
   begin
      --  If we have no free vars, then we have a single solution
      if Data.Free_Vars = 0 then
         declare
            Total : Natural := 0;
         begin
            for Row in A'Range (1) loop
               Total := Total + Natural (A (Row, A'Last (2)));
            end loop;
            return Total;
         end;
      end if;

      return Calc_Min (Paramaterize (Data, A));
   end Min_Presses;

   procedure Part_One (Input : String) is
      use GNAT.String_Split;
      use Interfaces;
      Lines : constant Slice_Set := Common.Split_Lines (Input);
      Total_Presses : Unsigned_64 := 0;
   begin
      for I in 1 .. Slice_Count (Lines) loop
         declare
            M : constant Machine := Parse_Machine (Slice (Lines, I));
            Button_Combos : constant Positive :=
              2 ** Positive (M.Button_Count) - 1;
            Min_Presses : Unsigned_64 := Unsigned_64'Last;
         begin
            for I in 1 .. Button_Combos loop
               declare
                  Light : Light_Mask (1 .. M.Light_Count) := (others => False);
                  Val : Natural := I;
                  Presses : Unsigned_64 := 0;
               begin
                  for J in 1 .. M.Button_Count loop
                     exit when Val = 0;
                     if Val mod 2 = 1 then
                        for K in 1 .. M.Light_Count loop
                           Light (K) := Light (K) xor M.Buttons (J, K);
                        end loop;
                        Presses := Presses + 1;
                     end if;
                     Val := Val / 2;
                  end loop;

                  if Light = M.Lights then
                     Min_Presses := Unsigned_64'Min (Presses, Min_Presses);
                  end if;
               end;
            end loop;
            Total_Presses := Total_Presses + Min_Presses;
         end;
      end loop;
      Ada.Text_IO.Put_Line ("Total presses: " & Total_Presses'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
      use GNAT.String_Split;

      Lines : constant Slice_Set := Common.Split_Lines (Input);
      Total_Presses : Natural := 0;
   begin
      for I in 1 .. Slice_Count (Lines) loop
         declare
            M : constant Machine := Parse_Machine (Slice (Lines, I));
            Presses : constant Natural := Min_Presses (M);
         begin
            Total_Presses := Total_Presses + Presses;
         end;
      end loop;
      Text_IO.Put_Line ("Min total presses: " & Total_Presses'Image);
   end Part_Two;
end Aoc.Day_10;
