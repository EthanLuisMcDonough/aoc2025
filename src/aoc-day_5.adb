with GNAT.String_Split;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Containers.Hashed_Sets;
with Ada.Containers;
with Aoc.Common;

use Ada;
use Ada.Strings;

package body Aoc.Day_5 is
   function Hash (H : Unsigned_64)
     return Ada.Containers.Hash_Type
   is
      MOD_MAX : constant Unsigned_64 := Unsigned_64'Val (
        Ada.Containers.Hash_Type'Last) + 1;
   begin
      return Ada.Containers.Hash_Type'Val (H mod MOD_MAX);
   end Hash;

   package Uint64_Set is new Ada.Containers.Hashed_Sets
     (Element_Type => Unsigned_64,
      Hash => Hash,
      Equivalent_Elements => "=");

   function Parse_Input (Input : String) return Database is
      use GNAT.String_Split;
      Lines : constant Slice_Set := Common.Split_Lines (Input);
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
            if Pair_Membership (Ingredient, Rng) then
               Fresh_Count := Fresh_Count + 1;
               exit;
            end if;
         end loop;
      end loop;
      Text_IO.Put_Line ("Fresh ingredients: " & Fresh_Count'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
      Dates : Dates_Vec.Vector := Parse_Input (Input).Fresh_Ranges;
      Fresh_Count : Unsigned_64 := 0;
      Visited : Uint64_Set.Set;
   begin
      for Rng_1 of Dates loop
         for Rng_2 of Dates loop
            if Pair_Overlap (Rng_1, Rng_2) then
               declare
                  M_First : constant Unsigned_64 :=
                    Unsigned_64'Min (Rng_1.First, Rng_2.First);
                  M_Second : constant Unsigned_64 :=
                    Unsigned_64'Max (Rng_1.Second, Rng_2.Second);
               begin
                  Rng_1.First := M_First;
                  Rng_2.First := M_First;
                  Rng_1.Second := M_Second;
                  Rng_2.Second := M_Second;
               end;
            end if;
         end loop;
      end loop;

      for Rng of Dates loop
         if not Uint64_Set.Contains (Visited, Rng.First) then
            Fresh_Count := Fresh_Count + (Rng.Second - Rng.First + 1);
            Uint64_Set.Insert (Visited, Rng.First);
         end if;
      end loop;

      Text_IO.Put_Line ("Fresh ingredients: " & Fresh_Count'Image);
   end Part_Two;
end Aoc.Day_5;
