with Ada.Characters.Latin_1;
with Ada.Text_IO;
with GNAT.String_Split;

use GNAT;
use Ada;

package body Aoc.Day_1 is
   Split_Delim : constant String := "" & Characters.Latin_1.LF;

   function Parse_Turn (S : String) return Safe_Turn is
      Dir_Char : constant Character := S (S'First);
      Len : constant Positive := Positive'Value (
        S (S'First + 1 .. S'Last));
      Direction : constant Rot_Dir := Rot_Dir'Enum_Val (
        Character'Pos (Dir_Char));
   begin
      return (Dir => Direction, Len => Len);
   end Parse_Turn;

   procedure Turn_Safe
     (Safe : in out Safe_Recorder;
      Turn : Safe_Turn)
   is
      MOD_VAL : constant Positive := Positive'Val (
        Safe_State'Pos (Safe_State'Last) + 1);
   begin
      case Turn.Dir is
         when RIGHT =>
            declare
               Sum : constant Positive := Positive'Val (
                 Safe_State'Pos (Safe.State) + Positive'Pos (Turn.Len));
            begin
               Safe.Click := Safe.Click + Sum / MOD_VAL;
               Safe.State := Safe_State'Val (Sum mod MOD_VAL);
            end;
         when LEFT =>
            declare
               Diff : constant Integer := Safe_State'Pos (
                 Safe.State) - Positive'Pos (Turn.Len);
            begin
               if Diff > 0 then
                  Safe.State := Safe_State'Val (Diff);
               else
                  Safe.Click := Safe.Click + Positive'Val (
                    (-Diff) / MOD_VAL) + Boolean'Pos (Safe.State > 0);
                  Safe.State := Safe_State'Val (Safe_State'Pos (
                    Safe_State'Last) - ((-Diff - 1) mod MOD_VAL));
               end if;
            end;
      end case;
   end Turn_Safe;

   procedure Part_One (Input : String) is
      Lines : String_Split.Slice_Set;
      Recorder : Safe_Recorder := (State => 50, Click => 0);
      Password : Natural := 0;
   begin
      String_Split.Create (
        S => Lines, From => Input,
        Separators => Split_Delim,
        Mode => String_Split.Multiple);

      for I in 1 .. String_Split.Slice_Count (Lines) loop
         declare
            Slice : constant String := String_Split.Slice (Lines, I);
            Turn : constant Safe_Turn := Parse_Turn (Slice);
         begin
            Turn_Safe (Recorder, Turn);
            if Recorder.State = 0 then
               Password := Password + 1;
            end if;
         end;
      end loop;

      Text_IO.Put_Line ("Password: " & Password'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
      Lines : String_Split.Slice_Set;
      Recorder : Safe_Recorder := (State => 50, Click => 0);
   begin
      String_Split.Create (
        S => Lines, From => Input,
        Separators => Split_Delim,
        Mode => String_Split.Multiple);

      for I in 1 .. String_Split.Slice_Count (Lines) loop
         declare
            Slice : constant String := String_Split.Slice (Lines, I);
            Turn : constant Safe_Turn := Parse_Turn (Slice);
         begin
            Turn_Safe (Recorder, Turn);
         end;
      end loop;

      Text_IO.Put_Line ("Password: " & Recorder.Click'Image);
   end Part_Two;
end Aoc.Day_1;
