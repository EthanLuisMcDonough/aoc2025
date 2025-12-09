with Interfaces;
with Ada.Text_IO;
with GNAT.String_Split;
with Interfaces;

use Ada;
use Interfaces;

package body Aoc.Day_9 is
   function Parse_Inputs (Input : String) return Coords is
      use GNAT.String_Split;
      Lines : constant Slice_Set := Common.Split_Lines (Input);
      Coord_Count : constant Positive :=
        Positive'Val (Slice_Count (Lines));
      C : Coords (1 .. Coord_Count);
   begin
      for I in 1 .. Slice_Count (Lines) loop
         declare
            Line : constant String := Slice (Lines, I);
         begin
            C (Positive'Val (I)) := Common.Parse_Pair (Line, ',');
         end;
      end loop;
      return C;
   end Parse_Inputs;

   procedure Part_One (Input : String) is
      C : constant Coords := Parse_Inputs (Input);
      Max_Area : Unsigned_64 := 0;
   begin
      for I in C'First .. C'Last - 1 loop
         for J in I + 1 .. C'Last loop
            declare
               Diff_X : constant Integer_64 := abs (
                 Integer_64 (C (I).First) - Integer_64 (C (J).First)) + 1;
               Diff_Y : constant Integer_64 := abs (
                 Integer_64 (C (I).Second) - Integer_64 (C (J).Second)) + 1;
               Area : constant Unsigned_64 :=
                 Unsigned_64 (Diff_X) * Unsigned_64 (Diff_Y); 
            begin
               Max_Area := Unsigned_64'Max (Max_Area, Area);
            end;
         end loop;
      end loop;
      Text_IO.Put_Line ("Max area: " & Max_Area'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
   begin
      null;
   end Part_Two;
end Aoc.Day_9;
