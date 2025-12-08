with Ada.Text_IO;
with Aoc.Generated;
with Ada.Environment_Variables;
with Ada.Characters;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Real_Time;

use Ada;
use Ada.Strings;

package body Aoc is
   DIR_ENV_VAR : constant String := "AOC_2025_INPUT_DIR";

   procedure Run_Timed is
      use Ada.Real_Time;
      Start_Time : constant Time := Clock;
   begin
      Run_Task;
      declare
         End_Time : constant Time := Clock;
         Diff : constant Duration := To_Duration (End_Time - Start_Time);
      begin
         Text_IO.Put_Line ("Finished " & Name & " in " &
           Duration'Image (Diff) & " seconds");
      end;
   end Run_Timed;

   procedure Handle_Day (Id : Day_Id; P : Day_Part) is
      Input_Dir : constant String := Environment_Variables.Value (
        DIR_ENV_VAR, Generated.Get_Input_Dir);
      File_Name : constant String := Input_Dir & "/" & "day_" &
        Fixed.Trim (Day_Id'Image (Id), Left) & "_input.txt";

      Input_Buf : Unbounded.Unbounded_String :=
        Unbounded.Null_Unbounded_String;
      FD : Text_IO.File_Type;
   begin
      Text_IO.Open (FD, Text_IO.In_File, File_Name);
      while not Text_IO.End_Of_File (FD) loop
         Unbounded.Append (Input_Buf, Text_IO.Get_Line (FD));
         if not Text_IO.End_Of_File (FD) then
            Unbounded.Append (Input_Buf, Characters.Latin_1.LF);
         end if;
      end loop;
      Text_IO.Close (FD);

      declare
         Input : constant String := Unbounded.To_String (Input_Buf);

         procedure Part_One is
         begin
            Aoc.Generated.Run_Day_Part_One (Id, Input);
         end Part_One;

         procedure Part_Two is
         begin
            Aoc.Generated.Run_Day_Part_Two (Id, Input);
         end Part_Two;

         procedure Timed_P1 is new Run_Timed
           (Run_Task => Part_One, Name => "Part 1");
         procedure Timed_P2 is new Run_Timed
           (Run_Task => Part_Two, Name => "Part 2");
      begin
         case P is
            when BOTH_PARTS =>
               Timed_P1;
               Text_IO.New_Line;
               Timed_P2;
            when FIRST_PART =>
               Timed_P1;
            when SECOND_PART =>
               Timed_P2;
         end case;
      end;
   end Handle_Day;
end Aoc;
