with Interfaces;
with Aoc.Common;
with Ada.Text_IO;
with GNAT.String_Split;

use Ada;
use GNAT;
use Interfaces;

package body Aoc.Day_6 is
   function Parse_Homework (Input : String) return Homework is
      use GNAT.String_Split;

      Lines : constant Slice_Set :=
        Common.Split_Lines (Input);
      Rows : constant Positive := Positive'Val (
        Slice_Count (Lines) - 1);

      First_Line : constant String := Slice (Lines, 1);
      First_Split : constant Slice_Set :=
        Common.Split_Space_Trim (First_Line);

      Last_Line : constant String := Slice (
        Lines, Slice_Count (Lines));
      Last_Split : constant Slice_Set :=
        Common.Split_Space_Trim (Last_Line);

      Cols : constant Positive := Positive'Val (
        Slice_Count (First_Split));

      Numbers : Grid (1 .. Rows, 1 .. Cols);
      Ops : Operators (1 .. Cols);

      procedure Process_Row (Nums : Slice_Set; Row_Ind : Positive) is
      begin
         for I in 1 .. Slice_Count (Nums) loop
            Numbers (Row_Ind, Positive'Val (I)) :=
              Positive'Value (Slice (Nums, I));
         end loop;
      end Process_Row;
   begin
      Process_Row (First_Split, 1);
      for I in 2 .. Slice_Count (Lines) - 1 loop
         declare
            Line : constant String := Slice (Lines, I);
            Slice : constant Slice_Set := Common.Split_Space_Trim (Line);
         begin
            Process_Row (Slice, Positive'Val (I));
         end;
      end loop;

      for I in 1 .. Slice_Count (Last_Split) loop
         declare
            Op_Str : constant String := Slice (Last_Split, I);
            Ind : constant Positive := Positive'Val (I);
            pragma Assert (Op_Str'Length = 1);
         begin
            case Op_Str (Op_Str'First) is
               when '+' =>
                  Ops (Ind) := Plus;
               when '*' =>
                  Ops (Ind) := Mult;
               when others =>
                  pragma Assert (False);
            end case;
         end;
      end loop;

      return (Rows => Rows, Cols => Cols,
        Nums => Numbers, Ops => Ops);
   end Parse_Homework;

   function Sum_Homework (Worksheet : Homework)
     return Unsigned_64
   is
      Sum : Unsigned_64 := 0;
   begin
      for I in 1 .. Worksheet.Cols loop
         declare
            Op : constant Operator := Worksheet.Ops (I);
            Local : Unsigned_64 := (if Op = Plus then 0 else 1);
         begin
            for J in 1 .. Worksheet.Rows loop
               declare
                  Num : constant Unsigned_64 :=
                    Unsigned_64'Val (Worksheet.Nums (J, I));
               begin
                  case Op is
                     when Plus =>
                        Local := Local + Num;
                     when Mult =>
                        Local := Local * Num;
                  end case;
               end;
            end loop;
            Sum := Sum + Local;
         end;
      end loop;
      return Sum;
   end Sum_Homework;

   procedure Part_One (Input : String) is
      Worksheet : constant Homework := Parse_Homework (Input);
      Sum : constant Unsigned_64 := Sum_Homework (Worksheet);
   begin
      Text_IO.Put_Line ("Sum: " & Sum'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
      use GNAT.String_Split;
      Sum : Unsigned_64 := 0;
      Lines : constant Slice_Set := Common.Split_Lines (Input);
      Last_Line : constant String := Slice (Lines, Slice_Count (Lines));
      Ops : constant Slice_Set := Create (Last_Line, " ",
        String_Split.Multiple);
   begin
      for Op_Ind in 1 .. Slice_Count (Ops) loop
         declare
            Op_Str : constant String := Slice (Ops, Op_Ind);
            Op : constant Operator := (if Op_Str = "+" then Plus else Mult);
            Local_Val : Unsigned_64 := (if Op = Plus then 0 else 1);
            Upper_Ind : Natural := Last_Line'Last;
         begin
            exit when Op_Str = "";
            if Op_Ind < Slice_Count (Ops) then
               Upper_Ind := Slice (Ops, Op_Ind + 1)'First - 1;
            end if;

            for J in Op_Str'First .. Upper_Ind loop
               declare
                  Power : Unsigned_64 := 1;
                  Num : Unsigned_64 := 0;
               begin
                  for K in reverse 1 .. Slice_Count (Lines) - 1 loop
                     declare
                        Line : constant String := Slice (Lines, K);
                        Digit : constant Character := Line (
                          J - Last_Line'First + Line'First);
                     begin
                        if Digit /= ' ' then
                           Num := Num + Unsigned_64'Val (
                             Character'Pos (Digit) -
                             Character'Pos ('0')) * Power;
                           Power := Power * 10;
                        end if;
                     end;
                  end loop;

                  exit when Num = 0;

                  case Op is
                     when Plus =>
                        Local_Val := Local_Val + Num;
                     when Mult =>
                        Local_Val := Local_Val * Num;
                  end case;
               end;
            end loop;
            Sum := Sum + Local_Val;
         end;
      end loop;

      Text_IO.Put_Line ("Sum: " & Sum'Image);
   end Part_Two;
end Aoc.Day_6;
