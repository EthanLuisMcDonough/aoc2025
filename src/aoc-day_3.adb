with GNAT;
with GNAT.String_Split;
with Interfaces;
with Ada.Text_IO;
with Ada.Characters.Latin_1;

use GNAT;
use Ada;
use Interfaces;

package body Aoc.Day_3 is
   function Max_Joltage (Line : String; Digit_Count : Positive)
     return Unsigned_64
   is
      type Digit_Array is array (1 .. Digit_Count) of Natural;
      type Index_Array is array (1 .. Digit_Count) of Integer;

      Battery_Vals : Digit_Array := (others => 0);
      Battery_Inds : Index_Array := (others => -1);

      pragma Assert (Line'Length >= Digit_Count);

      procedure Run_Iteration_K (K : Positive) is
         Upper_Limit  : constant Positive := Line'Last - (Digit_Count - K);
         Search_Start : constant Positive :=
           (if K = Index_Array'First
            then Line'First
            else Battery_Inds (K - 1) + 1);
      begin
         for I in Search_Start .. Upper_Limit loop
            declare
               Digit : constant Natural := Natural'Val (
                 Character'Pos (Line (I)) - Character'Pos ('0'));
            begin
               if Digit > Battery_Vals (K) then
                  Battery_Inds (K) := I;
                  Battery_Vals (K) := Digit;
               end if;
            end;
         end loop;
      end Run_Iteration_K;

      Joltage : Unsigned_64 := 0;
   begin
      for I in 1 .. Digit_Count loop
         Run_Iteration_K (I);
      end loop;

      for I in 1 .. Digit_Count loop
         Joltage := Joltage * 10 + Unsigned_64'Val (Battery_Vals (I));
      end loop;

      return Joltage;
   end Max_Joltage;

   function Sum_Joltage (Input : String; Digit_Count : Positive)
     return Unsigned_64
   is
      S : String_Split.Slice_Set;
      Sum : Unsigned_64 := 0;
   begin
      String_Split.Create (S,
        Input,
        "" & Characters.Latin_1.LF,
        String_Split.Multiple);

      for I in 1 .. String_Split.Slice_Count (S) loop
         Sum := Sum + Max_Joltage (
           String_Split.Slice (S, I), Digit_Count);
      end loop;

      return Sum;
   end Sum_Joltage;

   procedure Part_One (Input : String) is
      Sum : constant Unsigned_64 := Sum_Joltage (Input, 2);
   begin
      Text_IO.Put_Line ("Largest sum joltage: " & Sum'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
      Sum : constant Unsigned_64 := Sum_Joltage (Input, 12);
   begin
      Text_IO.Put_Line ("Largest sum joltage: " & Sum'Image);
   end Part_Two;
end Aoc.Day_3;
