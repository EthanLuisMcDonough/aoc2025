with GNAT;
with GNAT.String_Split;
with Ada.Text_IO;
with Ada.Characters.Latin_1;

use GNAT;
use Ada;

package body Aoc.Day_3 is
   function Char_To_Digit (C : Character) return Dec_Digit is
   begin
      return Dec_Digit'Val (Character'Pos (C) - Character'Pos ('0'));
   end Char_To_Digit;

   function Max_Joltage (Line : String) return Joltage is
      First_Max  : Natural := 0;
      Second_Max : Natural := 0;
      First_Ind  : Integer := -1;
   begin
      for I in Line'First .. Line'Last - 1 loop
         declare
            Digit : constant Dec_Digit := Char_To_Digit (Line (I));
         begin
            if Natural'Val (Digit) > First_Max then
               First_Max := Natural'Val (Digit);
               First_Ind := I;
            end if;
         end;
      end loop;

      for I in First_Ind + 1 .. Line'Last loop
         declare
            Digit : constant Dec_Digit := Char_To_Digit (Line (I));
         begin
            if Natural'Val (Digit) > Second_Max then
               Second_Max := Natural'Val (Digit);
            end if;
         end;
      end loop;

      return Joltage'Val (First_Max * 10 + Second_Max);
   end Max_Joltage;

   procedure Part_One (Input : String) is
      S : String_Split.Slice_Set;
      Sum_Joltage : Natural := 0;
   begin
      String_Split.Create (S,
        Input,
        "" & Characters.Latin_1.LF,
        String_Split.Multiple);

      for I in 1 .. String_Split.Slice_Count (S) loop
         Sum_Joltage := Sum_Joltage + Natural'Val (
           Max_Joltage (String_Split.Slice (S, I)));
      end loop;

      Text_IO.Put_Line ("Largest sum joltage: " & Sum_Joltage'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
   begin
      null;
   end Part_Two;
end Aoc.Day_3;
