with Interfaces;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with GNAT.String_Split;

use Ada;
use Interfaces;
use Ada.Strings;

package body Aoc.Day_6 is
   function Parse_Homework (Input : String) return Homework is
      use GNAT.String_Split;
      use Ada.Characters;

      function Split_Space_Trim (S : String) return Slice_Set is
         Trimmed : constant String := Fixed.Trim (S, Both);
         Set : constant Slice_Set := Create (Trimmed, " ", Multiple);
      begin
         return Set;
      end Split_Space_Trim;

      Lines : constant Slice_Set := Create (
        Input, Latin_1.LF & "", Multiple);
      Rows : constant Positive := Positive'Val (
        Slice_Count (Lines) - 1);

      First_Line : constant String := Slice (Lines, 1);
      First_Split : constant Slice_Set := Split_Space_Trim (First_Line);

      Last_Line : constant String := Slice (
        Lines, Slice_Count (Lines));
      Last_Split : constant Slice_Set := Split_Space_Trim (Last_Line);

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
            Slice : constant Slice_Set := Split_Space_Trim (Line);
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

   procedure Part_One (Input : String) is
      Worksheet : constant Homework := Parse_Homework (Input);
      Sum : Unsigned_64 := 0;
   begin
      for I in 1 .. Worksheet.Cols loop
         declare
            Op : constant Operator := Worksheet.Ops (I);
            Local : Unsigned_64 := (if Op = Plus then 0 else 1);
         begin
            for J in 1 .. Worksheet.Rows loop
               declare
                  Num : constant Unsigned_64 := Unsigned_64'Val (
                    Worksheet.Nums (J, I));
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
      Text_IO.Put_Line ("Sum: " & Sum'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
   begin
      null;
   end Part_Two;
end Aoc.Day_6;
