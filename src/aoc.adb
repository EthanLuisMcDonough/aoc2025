with Ada.Text_IO;
with Aoc.Generated;
with Ada.Environment_Variables;
with Ada.Characters;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

use Ada;
use Ada.Strings;

package body Aoc is
   DIR_ENV_VAR : constant String := "AOC_2025_INPUT_DIR";

   procedure Unimplemented is
   begin
      Text_IO.Put_Line (Text_IO.Standard_Error, "Unimplemented day/part");
      raise Program_Error;
   end Unimplemented;

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
      begin
         case P is
            when BOTH_PARTS =>
               Aoc.Generated.Run_Day_Part_One (Id, Input);
               Aoc.Generated.Run_Day_Part_Two (Id, Input);
            when FIRST_PART =>
               Aoc.Generated.Run_Day_Part_One (Id, Input);
            when SECOND_PART =>
               Aoc.Generated.Run_Day_Part_Two (Id, Input);
         end case;
      end;
   end Handle_Day;
end Aoc;
