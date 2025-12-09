with Interfaces;
with Ada.Text_IO;
with GNAT.String_Split;

use Ada;
use Interfaces;

package body Aoc.Day_9 is
   function Corner_Area (A : Coord.Pair; B : Coord.Pair)
     return Interfaces.Unsigned_64
   is
      A_X : constant Integer_64 := Integer_64 (A.First);
      B_X : constant Integer_64 := Integer_64 (B.First);
      A_Y : constant Integer_64 := Integer_64 (A.Second);
      B_Y : constant Integer_64 := Integer_64 (B.Second);
      Diff_X : constant Unsigned_64 := Unsigned_64 (abs (A_X - B_X) + 1);
      Diff_Y : constant Unsigned_64 := Unsigned_64 (abs (A_Y - B_Y) + 1);
   begin
      return Diff_X * Diff_Y;
   end Corner_Area;

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
            C (Positive'Val (I)) := Coord.Parse (Line);
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
            Max_Area := Unsigned_64'Max (Corner_Area (C (I), C (J)), Max_Area);
         end loop;
      end loop;
      Text_IO.Put_Line ("Max area: " & Max_Area'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
   begin
      null;
   end Part_Two;
end Aoc.Day_9;
