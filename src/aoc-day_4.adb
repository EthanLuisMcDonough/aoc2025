with Ada.Characters;
with Ada.Characters.Latin_1;
with Ada.Text_IO;
with GNAT;
with GNAT.String_Split;

use Ada;
use GNAT;

package body Aoc.Day_4 is
   ROLL_CHAR : constant Character := '@';

   procedure Part_One (Input : String) is
      use GNAT.String_Split;

      Lines : constant Slice_Set := Create (
        From => Input, Mode => Multiple,
        Separators => Characters.Latin_1.LF & "");
      Accessible_Rolls : Natural := 0;
   begin

      for I in 1 .. Slice_Count (Lines) loop
         declare
            Row : constant String := Slice (Lines, I);
         begin
            for J in Row'Range loop
               if Row (J) = ROLL_CHAR then
                  declare
                     Adjacent : Natural := Boolean'Pos (
                       J > Row'First and then Row (J - 1) = ROLL_CHAR) +
                       Boolean'Pos (J < Row'Last and then
                         Row (J + 1) = ROLL_CHAR);
                     J_Norm : constant Integer := J - Row'First + 1;
                     Lo_Check : constant Integer :=
                       Integer'Max (J_Norm - 1, 1);
                     Hi_Check : constant Integer :=
                       Integer'Min (J_Norm + 1, Row'Length);
                  begin
                     if I > 1 then
                        declare
                           Prev : constant String := Slice (Lines, I - 1);
                        begin
                           for K in Lo_Check .. Hi_Check loop
                              Adjacent := Adjacent + Boolean'Pos (
                                Prev (Prev'First + K - 1) = ROLL_CHAR);
                           end loop;
                        end;
                     end if;

                     if I /= Slice_Count (Lines) then
                        declare
                           Next : constant String := Slice (Lines, I + 1);
                        begin
                           for K in Lo_Check .. Hi_Check loop
                              Adjacent := Adjacent + Boolean'Pos (
                                Next (Next'First + K - 1) = ROLL_CHAR);
                           end loop;
                        end;
                     end if;

                     if Adjacent < 4 then
                        Accessible_Rolls := Accessible_Rolls + 1;
                     end if;
                  end;
               end if;
            end loop;
         end;
      end loop;

      Ada.Text_IO.Put_Line ("Accessible rolls: " & Accessible_Rolls'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
   begin
      null;
   end Part_Two;
end Aoc.Day_4;
