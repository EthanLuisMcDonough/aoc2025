with Ada.Text_IO;
with GNAT.String_Split;
with Aoc.Common;

use Ada;

package body Aoc.Day_7 is
   procedure Part_One (Input : String) is
      use GNAT.String_Split;
      Lines : constant Slice_Set := Common.Split_Lines (Input);
      Prev : String := Slice (Lines, 1);
      Splits : Natural := 0;
   begin
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
                        when '.' =>
                           Line (C_Ind) := '|';
                        when '^' =>
                           Line (C_Ind + 1) := '|';
                           Line (C_Ind - 1) := '|';
                           Splits := Splits + 1;
                        when others =>
                           null;
                     end case;
                  end if;
               end;
            end loop;
            Prev := Line;
         end;
      end loop;
      Text_IO.Put_Line ("Splits: " & Splits'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
   begin
      null;
   end Part_Two;
end Aoc.Day_7;
