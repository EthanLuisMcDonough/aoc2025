with Interfaces;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.String_Split;
with Aoc.Common;

use Ada;
use Ada.Strings;
use Interfaces;
use GNAT;
use Aoc.Common;

package body Aoc.Day_2 is
   package Id_Pair is new Common.Numeric_Pair (Num => Unsigned_64);

   function Check_Repeat (Id : Unsigned_64) return Boolean is
      Str  : constant String := Fixed.Trim (Unsigned_64'Image (Id), Both);
      Half : constant Integer := Str'Length / 2;
   begin
      return Str'Length mod 2 = 0 and then
        Str (Str'First .. Half) = Str (Half + 1 .. Str'Last);
   end Check_Repeat;

   function Check_Repeat_Exhaustive
     (Id : Interfaces.Unsigned_64) return Boolean
   is
      Str : constant String := Fixed.Trim (Unsigned_64'Image (Id), Both);

      function Check_Repeat_K (K : Positive) return Boolean;

      function Check_Repeat_K (K : Positive) return Boolean is
         Prev_Last : Integer := Str'First + K - 1;
         First : constant String := Str (Str'First .. Prev_Last);
      begin
         if Str'Length mod K > 0 then
            return False;
         end if;

         for J in 2 .. Str'Length / K loop
            declare
               Next_Last : constant Integer := Prev_Last + K;
               Current : constant String := Str (Prev_Last + 1 .. Next_Last);
            begin
               if Current /= First then
                  return False;
               end if;
               Prev_Last := Next_Last;
            end;
         end loop;

         return True;
      end Check_Repeat_K;

   begin
      for I in 1 .. Str'Length - 1 loop
         if Check_Repeat_K (I) then
            return True;
         end if;
      end loop;
      return False;
   end Check_Repeat_Exhaustive;

   function Sum_Errs (Input : String) return Unsigned_64 is
      use Id_Pair;
      Pair_Strs : String_Split.Slice_Set;
      Err_Sum   : Unsigned_64 := 0;
   begin
      String_Split.Create (
        Pair_Strs, Input, ",",
        String_Split.Multiple);

      for I in 1 .. String_Split.Slice_Count (Pair_Strs) loop
         declare
            P : constant Pair := Parse (
              String_Split.Slice (Pair_Strs, I));
         begin
            for I in P.First .. P.Second loop
               if Check_Err (I) then
                  Err_Sum := Err_Sum + I;
               end if;
            end loop;
         end;
      end loop;

      return Err_Sum;
   end Sum_Errs;

   procedure Part_One (Input : String) is
      function Sum_Logic is new Sum_Errs (
        Check_Err => Check_Repeat);
      Err_Sum : constant Unsigned_64 := Sum_Logic (Input);
   begin
      Text_IO.Put_Line ("Error sum: " & Err_Sum'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
      function Sum_Logic is new Sum_Errs (
        Check_Err => Check_Repeat_Exhaustive);
      Err_Sum : constant Unsigned_64 := Sum_Logic (Input);
   begin
      Text_IO.Put_Line ("Error sum: " & Err_Sum'Image);
   end Part_Two;
end Aoc.Day_2;
