with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Aoc;              use Aoc;

procedure Driver is
   procedure Print_Help;
   procedure Print_Help is
      PADDING : constant String := "   ";
   begin
      Put_Line ("Usage: aoc2025 <day> <part>");
      Put_Line (PADDING & "<day>   Day to run, must be a number between " &
                "1 and 12");
      Put_Line (PADDING & "<part>  Part to run, must be 1 or 2. This " &
                "argument is optional.");
   end Print_Help;

   Selected_Day  : Day_Id;
   Selected_Part : Day_Part := BOTH_PARTS;
begin
   if Argument_Count = 0 or else
     (Argument_Count = 1 and then Argument (1) = "--help")
   then
      Print_Help;
      return;
   end if;

   if Argument_Count > 2 then
      Put_Line (Standard_Error, "Invalid number of CLI arguments supplied.");
      raise Program_Error;
   end if;

   Selected_Day := Day_Id'Value (Argument (1));
   if Argument_Count = 2 then
      Selected_Part := Day_Part'Val (Natural'Value (Argument (2)));
   end if;

   Aoc.Handle_Day (Selected_Day, Selected_Part);
end Driver;
