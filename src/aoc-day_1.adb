with Ada.Characters.Latin_1;
with Ada.Text_IO;
with GNAT.String_Split;

use GNAT;
use Ada;

package body Aoc.Day_1 is
   function Safe_Wrap (Val : Integer) return Safe_State is
      MOD_VAL : constant Integer := Safe_State'Pos (Safe_State'Last) + 1;
   begin
      return Safe_State'Val (Val mod MOD_VAL);
   end Safe_Wrap;

   function Safe_Turn (Safe : Safe_State; Dir : Rot_Dir; Turn : Positive)
     return Safe_State
   is
   begin
      case Dir is
         when RIGHT =>
            return Safe_Wrap (Safe_State'Pos (Safe) + Turn);
         when LEFT =>
            if Safe < Safe_Wrap (Turn) then
               return Safe_State'Last - (Safe_Wrap (Turn) - Safe) + 1;
            else
               return Safe - Safe_Wrap (Turn);
            end if;
      end case;
   end Safe_Turn;

   procedure Part_One (Input : String) is
      Lines : String_Split.Slice_Set;
      Delim : constant String := "" & Characters.Latin_1.LF;
      Safe : Safe_State := 50;
      Password : Natural := 0;
   begin
      String_Split.Create (
        S => Lines, From => Input,
        Separators => Delim,
        Mode => String_Split.Multiple);

      for I in 1 .. String_Split.Slice_Count (Lines) loop
         declare
            Slice : constant String := String_Split.Slice (Lines, I);
            Direction : constant Character := Slice (Slice'First);
            Stride : constant Positive := Positive'Value (
              Slice (Slice'First + 1 .. Slice'Last));
         begin
            Safe := Safe_Turn (Safe, Rot_Dir'Enum_Val (
              Character'Pos (Direction)), Stride);

            if Safe = 0 then
               Password := Password + 1;
            end if;
         end;
      end loop;

      Text_IO.Put_Line (Password'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
   begin
      null;
   end Part_Two;
end Aoc.Day_1;
