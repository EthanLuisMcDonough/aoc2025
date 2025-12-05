with Ada.Characters.Latin_1;
with GNAT.String_Split;
with Ada.Text_IO;
with Ada.Strings.Fixed;

use Ada;
use Ada.Strings;

package body Aoc.Day_5 is
   function Parse_Input (Input : String) return Database is
      use GNAT.String_Split;
      Lines : constant Slice_Set := Create (Input,
        Characters.Latin_1.LF & "", Multiple);
      Dates       : Dates_Vec.Vector;
      Ingredients : Uint64_Vec.Vector;
   begin
      for I in 1 .. Slice_Count (Lines) loop
         declare
            Line : constant String := Slice (Lines, I);
         begin
            if Fixed.Index (Line, "-") = 0 then
               Uint64_Vec.Append (Ingredients, Unsigned_64'Value (Line));
            else
               Dates_Vec.Append (Dates, Parse_Pair (Line));
            end if;
         end;
      end loop;
      return (Fresh_Ranges => Dates, Ingredients => Ingredients);
   end Parse_Input;

   procedure Part_One (Input : String) is
      DB : constant Database := Parse_Input (Input);
      Fresh_Count : Natural := 0;
   begin
      for Ingredient of DB.Ingredients loop
         for Rng of DB.Fresh_Ranges loop
            if Ingredient >= Rng.First and then
              Ingredient <= Rng.Second
            then
               Fresh_Count := Fresh_Count + 1;
               exit;
            end if;
         end loop;
      end loop;
      Text_IO.Put_Line ("Fresh ingredients: " & Fresh_Count'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
   begin
      null;
   end Part_Two;
end Aoc.Day_5;
