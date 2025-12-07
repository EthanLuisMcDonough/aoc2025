with Interfaces;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.String_Split;
with Aoc.Common;

use Ada;
use Interfaces;

package body Aoc.Day_7 is
   function Traverse (Input : String) return Traversal_Data is
      use GNAT.String_Split;

      Lines : constant Slice_Set := Common.Split_Lines (Input);
      Prev : String := Slice (Lines, 1);

      type Possible_Paths is array (1 .. Prev'Length)
        of Unsigned_64;

      Counts : Possible_Paths := (others => 0);
      Prev_Counts : Possible_Paths := (others => 0);
      Splits : Natural := 0;
      Total_Paths : Unsigned_64 := 0;
   begin
      Prev_Counts (Strings.Fixed.Index (Prev, "S")) := 1;
      for I in 2 .. Slice_Count (Lines) loop
         declare
            Line : String := Slice (Lines, I);
         begin
            for J in 1 .. Line'Length loop
               declare
                  C_Ind : constant Integer := Line'First + J - 1;
                  A_Ind : constant Integer := Prev'First + J - 1;
                  Current : constant Character := Line (C_Ind);
                  Above : constant Character := Prev (A_Ind);
               begin
                  if Above in 'S' | '|' then
                     case Current is
                        when '.' | '|' =>
                           Line (C_Ind) := '|';
                           Counts (J) := Counts (J) + Prev_Counts (J);
                        when '^' =>
                           Line (C_Ind + 1) := '|';
                           Line (C_Ind - 1) := '|';
                           Counts (J - 1) := Counts (J - 1) + Prev_Counts (J);
                           Counts (J + 1) := Counts (J + 1) + Prev_Counts (J);
                           Splits := Splits + 1;
                        when others =>
                           null;
                     end case;
                  end if;
               end;
            end loop;

            Prev := Line;
            Prev_Counts := Counts;
            Counts := (others => 0);
         end;
      end loop;

      for I of Prev_Counts loop
         Total_Paths := Total_Paths + I;
      end loop;

      return (Splits => Splits, Paths => Total_Paths);
   end Traverse;

   procedure Part_One (Input : String) is
      Data : constant Traversal_Data := Traverse (Input);
   begin
      Text_IO.Put_Line ("Splits: " & Data.Splits'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
      Data : constant Traversal_Data := Traverse (Input);
   begin
      Text_IO.Put_Line ("Paths: " & Data.Paths'Image);
   end Part_Two;
end Aoc.Day_7;
