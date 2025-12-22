with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with GNAT.String_Split;

use Ada;

package body Aoc.Day_12 is
   function Image (S : Present_Shape) return String is
      use Ada.Characters.Latin_1;
      Buffer : String (1 .. 11);
      Str_Row_Len : constant Natural := Present_Row'Range_Length + 1;
   begin
      for I in Present_Row'Range loop
         for J in Present_Col'Range loop
            declare
               Str_Ind : constant Positive := Positive (
                 (Natural (I) - 1) * Str_Row_Len + Natural (J));
            begin
               Buffer (Str_Ind) := (if S (I, J) then '#' else '.');
            end;
         end loop;

         if I /= Present_Row'Last then
            Buffer (Positive (I) * Str_Row_Len) := LF;
         end if;
      end loop;
      return Buffer;
   end Image;

   function Image (R : Gift_Region) return String is
      use Ada.Characters.Latin_1;
      Str_Row_Len : constant Positive := R'Length (2) + 1;
      Buffer : String (1 .. R'Length (1) * Str_Row_Len - 1);
   begin
      for Row in R'Range (1) loop
         for Col in R'Range (2) loop
            declare
               Str_Ind : constant Positive := Positive (
                 (Natural (Row) - 1) * Str_Row_Len + Natural (Col));
            begin
               Buffer (Str_Ind) := (if R (Row, Col) then '#' else '.');
            end;
         end loop;
         if Row /= R'Last (1) then
            Buffer (Row * Str_Row_Len) := LF;
         end if;
      end loop;
      return Buffer;
   end Image;

   function Parse_Shape (S : String) return Present_Shape is
      Str_Row_Len : constant Natural := Present_Row'Range_Length + 1;
      P : Present_Shape := (others => (others => False));
   begin
      for I in Present_Row'Range loop
         for J in Present_Col'Range loop
            declare
               Str_Ind : constant Natural :=
                 (Natural (I) - Natural (Present_Row'First)) * Str_Row_Len +
                 (Natural (J) - Natural (Present_Col'First));
               Ch : constant Character := S (Str_Ind + S'First);
            begin
               P (I, J) := (Ch = '#');
            end;
         end loop;
      end loop;
      return P;
   end Parse_Shape;

   procedure Flip_X (S : in out Present_Shape) is
   begin
      for I in Present_Row'Range loop
         declare
            Temp : constant Boolean := S (I, Present_Col'First);
         begin
            S (I, Present_Col'First) := S (I, Present_Col'Last);
            S (I, Present_Col'Last) := Temp;
         end;
      end loop;
   end Flip_X;

   procedure Flip_Y (S : in out Present_Shape) is
   begin
      for I in Present_Col'Range loop
         declare
            Temp : constant Boolean := S (Present_Row'First, I);
         begin
            S (Present_Row'First, I) := S (Present_Row'Last, I);
            S (Present_Row'Last, I) := Temp;
         end;
      end loop;
   end Flip_Y;

   procedure Rotate (S : in out Present_Shape) is
   begin
      for I in Present_Row'First .. Present_Row'Last - 1 loop
         for J in Present_Col (I) + 1 .. Present_Col'Last loop
            declare
               Flip_I : constant Present_Row := Present_Row (J);
               Flip_J : constant Present_Col := Present_Col (I);
               Temp : constant Boolean := S (I, J);
            begin
               S (I, J) := S (Flip_I, Flip_J);
               S (Flip_I, Flip_J) := Temp;
            end;
         end loop;
      end loop;
      Flip_X (S);
   end Rotate;

   function Parse_Input (Input : String) return Box_Requirements is
      use GNAT.String_Split;
      use Ada.Characters.Latin_1;

      Delim : constant String := LF & LF;
      Shape_Count : constant Positive := Positive (
        Strings.Fixed.Count (Input, Delim));
      Last_Index : Positive := 1;

      Last_Delim : constant Positive := Positive (
        Strings.Fixed.Index
          (Source => Input,
           Pattern => Delim,
           Going => Strings.Backward));
      Box_Str : constant String := Input (
        Last_Delim + Delim'Length .. Input'Last);
      Box_Count : constant Positive := Positive (
        Strings.Fixed.Count (Box_Str, LF & "") + 1);

      Result : Box_Requirements := (
        Shape_Count => Shape_Count,
        Box_Count => Box_Count,
        Shapes => (others => (others =>
          (others => (others => False)))),
        Is_Unique => (others => (others => True)),
        Dimensions => (others => (First => 1, Second => 1)),
        Requirements => (others => (others => 0)));

      Box_Rows : constant Slice_Set := Common.Split_Lines (Box_Str);
   begin
      for I in 1 .. Shape_Count loop
         declare
            First_Line : constant Positive := Strings.Fixed.Index
              (Source => Input, Pattern => "" & LF, From => Last_Index);
            Next_Ind : constant Positive := Strings.Fixed.Index
              (Source => Input, Pattern => Delim, From => Last_Index);
            Shape_Str : constant String := Input (
              First_Line + 1 .. Next_Ind - 1);
            Shape : Present_Shape := Parse_Shape (Shape_Str);
            F_X : Present_Shape := Shape;
            F_Y : Present_Shape := Shape;
         begin
            Result.Shapes (I, Base) := Shape;
            for Rot in Rot_90 .. Rot_270 loop
               Rotate (Shape);
               Result.Shapes (I, Rot) := Shape;
            end loop;

            Flip_X (F_X);
            Flip_Y (F_Y);

            Result.Shapes (I, Flipped_X) := F_X;
            Result.Shapes (I, Flipped_Y) := F_Y;

            for J in Shape_Variant'First ..
              Shape_Variant'Pred (Shape_Variant'Last)
            loop
               if Result.Is_Unique (I, J) then
                  for K in Shape_Variant'Succ (J) .. Shape_Variant'Last loop
                     if Result.Shapes (I, J) = Result.Shapes (I, K) then
                        Result.Is_Unique (I, K) := False;
                     end if;
                  end loop;
               end if;
            end loop;

            Last_Index := Next_Ind + Delim'Length;
         end;
      end loop;

      for I in 1 .. Slice_Count (Box_Rows) loop
         declare
            Box_Id : constant Positive := Positive (I);
            Line : constant String := Slice (Box_Rows, I);
            Divider : constant Positive := Strings.Fixed.Index (Line, ":");
            Dim : constant Box_Bounds.Pair := Box_Bounds.Parse (
              Line (Line'First .. Divider - 1));
            Nums_Str : constant String := Line (Divider + 2 .. Line'Last);
            Nums : constant Slice_Set := Create (Nums_Str, " ", Multiple);
         begin
            Result.Dimensions (Box_Id) := Dim;
            for J in 1 .. Slice_Count (Nums) loop
               Result.Requirements (Box_Id, Positive (J)) :=
                 Natural'Value (Slice (Nums, J));
            end loop;
         end;
      end loop;

      return Result;
   end Parse_Input;

   function Can_Apply
     (A : Attempt; Reqs : Box_Requirements;
      Region : Gift_Region) return Boolean
   is
      function Check_Adjacent
        (Row : Positive; Col : Positive)
         return Boolean;

      Any_Adjacent : Boolean := False;

      --  Checks if the given coordinate is adjacent to a
      --  currently occupied tile
      function Check_Adjacent
        (Row : Positive; Col : Positive)
         return Boolean
      is
         Lower_Row : constant Positive := Positive (
           Natural'Max (Natural (Row) - 1, Region'First (1)));
         Upper_Row : constant Positive := Positive'Min (
           Row + 1, Region'Length (1));
         Lower_Col : constant Positive := Positive (
           Natural'Max (Natural (Col) - 1, Region'First (2)));
         Upper_Col : constant Positive := Positive'Min (
           Col + 1, Region'Length (2));
      begin
         for R in Lower_Row .. Upper_Row loop
            for C in Lower_Col .. Upper_Col loop
               if (R /= Row or else C /= Col) and then
                 Region (R, C)
               then
                  return True;
               end if;
            end loop;
         end loop;
         return False;
      end Check_Adjacent;
   begin
      for I in Present_Row'Range loop
         for J in Present_Col'Range loop
            declare
               Row : constant Positive := Map_Row (A.Pos, I);
               Col : constant Positive := Map_Col (A.Pos, J);
            begin
               if Region (Row, Col) and then
                 Reqs.Shapes (A.Shape.Index, A.Shape.Variant) (I, J)
               then
                  return False;
               end if;
               Any_Adjacent := Any_Adjacent or else Check_Adjacent (Row, Col);
            end;
         end loop;
      end loop;
      return Any_Adjacent;
   end Can_Apply;

   procedure Apply
     (A : Attempt;
      Reqs : Box_Requirements;
      Region : in out Gift_Region;
      Do_Apply : Boolean)
   is
   begin
      for I in Present_Row'Range loop
         for J in Present_Col'Range loop
            declare
               Row : constant Positive := Map_Row (A.Pos, I);
               Col : constant Positive := Map_Col (A.Pos, J);
            begin
               if Reqs.Shapes (A.Shape.Index, A.Shape.Variant) (I, J) then
                  Region (Row, Col) := Do_Apply;
               end if;
            end;
         end loop;
      end loop;
   end Apply;

   function Sum_Required_Boxes
     (Reqs : Box_Requirements; Box_Id : Positive)
      return Natural
   is
      Boxes : Natural := 0;
   begin
      for Shape in 1 .. Reqs.Shape_Count loop
         declare
            Shape_Boxes : Natural := 0;
         begin
            for I in Present_Row'Range loop
               for J in Present_Col'Range loop
                  if Reqs.Shapes (Shape, Base) (I, J) then
                     Shape_Boxes := Shape_Boxes + 1;
                  end if;
               end loop;
            end loop;
            Boxes := Boxes + Reqs.Requirements (Box_Id, Shape) * Shape_Boxes;
         end;
      end loop;
      return Boxes;
   end Sum_Required_Boxes;

   function Can_Fit_All_In_Region
     (Reqs : Box_Requirements; Box_Id : Positive)
      return Boolean
   is
      function All_Fit_Internal return Boolean;
      function Apply_Attempt (A : Attempt) return Boolean;

      Bounds : constant Box_Bounds.Pair := Reqs.Dimensions (Box_Id);
      Layout : Gift_Region (1 .. Bounds.Second, 1 .. Bounds.First);
      Presents_Left : array (1 .. Reqs.Shape_Count)
        of Natural := (others => 0);
      Presents_Placed : Natural := 0;

      --  Applies the given attempt to the board and returns
      --  true if all presents have been placed.
      function Apply_Attempt (A : Attempt) return Boolean is
      begin
         if Presents_Placed = 0 or else
           Can_Apply (A, Reqs, Layout)
         then
            --  Apply and adjust counters
            Apply (A, Reqs, Layout, True);
            Presents_Placed := Presents_Placed + 1;
            Presents_Left (A.Shape.Index) :=
              Presents_Left (A.Shape.Index) - 1;

            if All_Fit_Internal then
               return True;
            end if;

            --  Unapply tile and adjust counters
            --  if the placement was not successful
            Presents_Left (A.Shape.Index) :=
              Presents_Left (A.Shape.Index) + 1;
            Presents_Placed := Presents_Placed - 1;
            Apply (A, Reqs, Layout, False);
         end if;

         return False;
      end Apply_Attempt;

      function All_Fit_Internal return Boolean is
      begin
         if (for all I in 1 .. Reqs.Shape_Count =>
           Presents_Left (I) = 0)
         then
            return True;
         end if;

         for I in 1 .. Reqs.Shape_Count loop
            if Presents_Left (I) > 0 then
               for J in Shape_Variant'Range loop
                  if Reqs.Is_Unique (I, J) then
                     if Presents_Placed = 0 then
                        --  First present should be in top right corner
                        if Apply_Attempt ((
                           Pos => (First => 1, Second => 1),
                           Shape => (Variant => J, Index => I)))
                        then
                           return True;
                        end if;
                     else
                        for K in 1 .. Layout'Length (1) - 2 loop
                           for L in 1 .. Layout'Length (2) - 2 loop
                              if Apply_Attempt ((
                                Pos => (First => K, Second => L),
                                Shape => (Variant => J, Index => I)))
                              then
                                 return True;
                              end if;
                           end loop;
                        end loop;
                     end if;
                  end if;
               end loop;
            end if;
         end loop;

         return False;
      end All_Fit_Internal;

      Present_Area : constant Natural := Sum_Required_Boxes (Reqs, Box_Id);
      Region_Area : constant Positive :=
        Reqs.Dimensions (Box_Id).First * Reqs.Dimensions (Box_Id).Second;
   begin
      if Present_Area > Region_Area then
         return False;
      end if;

      for I in 1 .. Reqs.Shape_Count loop
         Presents_Left (I) := Reqs.Requirements (Box_Id, I);
      end loop;

      return All_Fit_Internal;
   end Can_Fit_All_In_Region;

   procedure Part_One (Input : String) is
      Reqs : constant Box_Requirements := Parse_Input (Input);
      Valid_Region_Count : Natural := 0;
   begin
      for I in 1 .. Reqs.Box_Count loop
         if Can_Fit_All_In_Region (Reqs, I) then
            Valid_Region_Count := Valid_Region_Count + 1;
         end if;
      end loop;
      Text_IO.Put_Line ("Valid regions: " & Valid_Region_Count'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
      pragma Unreferenced (Input);
   begin
      Text_IO.Put_Line ("Happy holidays! The tree is decorated");
   end Part_Two;
end Aoc.Day_12;
