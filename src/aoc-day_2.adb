with Interfaces;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.String_Split;

use Ada;
use Ada.Strings;
use Interfaces;
use GNAT;

package body Aoc.Day_2 is
   function Parse_Pair (S : String) return Id_Pair is
      Split_Ind : constant Natural := Fixed.Index (S, "-");
      Str_One   : constant String := S (S'First .. Split_Ind - 1);
      Str_Two   : constant String := S (Split_Ind + 1 .. S'Last);
      First_Id  : constant Unsigned_64 := Unsigned_64'Value (Str_One);
      Second_Id : constant Unsigned_64 := Unsigned_64'Value (Str_Two);
   begin
      return (First => First_Id, Second => Second_Id);
   end Parse_Pair;

   function Check_Repeat (Id : Unsigned_64) return Boolean is
      Str  : constant String := Fixed.Trim (Unsigned_64'Image (Id), Both);
      Half : constant Integer := Str'Length / 2;
   begin
      return Str'Length mod 2 = 0 and then
        Str (Str'First .. Half) = Str (Half + 1 .. Str'Last);
   end Check_Repeat;

   procedure Part_One (Input : String) is
      Pair_Strs : String_Split.Slice_Set;
      Err_Sum   : Unsigned_64 := 0;
   begin
      String_Split.Create (
        Pair_Strs, Input, ",",
        String_Split.Multiple);

      for I in 1 .. String_Split.Slice_Count (Pair_Strs) loop
         declare
            Pair : constant Id_Pair := Parse_Pair (
              String_Split.Slice (Pair_Strs, I));
         begin
            for I in Pair.First .. Pair.Second loop
               if Check_Repeat (I) then
                  Err_Sum := Err_Sum + I;
               end if;
            end loop;
         end;
      end loop;

      Text_IO.Put_Line ("Error sum: " & Err_Sum'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
   begin
      null;
   end Part_Two;
end Aoc.Day_2;
