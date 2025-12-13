with Ada.Text_IO;
with GNAT;
with Aoc.Common;
with GNAT.String_Split;

package body Aoc.Day_11 is
   use Device_Map;

   function Parse_Input (Input : String) return Map is
      use GNAT.String_Split;
      use Ada.Containers;

      Lines : constant Slice_Set := Common.Split_Lines (Input);
      M : Map;
   begin
      M.Reserve_Capacity (Count_Type (Slice_Count (Lines)));
      for I in 1 .. Slice_Count (Lines) loop
         declare
            Line : constant String := Slice (Lines, I);
            Label : constant Device_Id :=
              Line (Line'First .. Line'First + 2);
            Raw_Children : constant String :=
              Line (Line'First + 5 .. Line'Last);
            Children : constant Slice_Set := Create (
              Raw_Children, " ", Multiple);
            Device_Vec : Vector;
         begin
            Device_Vec.Reserve_Capacity (Count_Type (Slice_Count (Children)));
            for J in 1 .. Slice_Count (Children) loop
               Device_Vec.Append (Slice (Children, J));
            end loop;
            M.Insert (Label, Device_Vec);
         end;
      end loop;
      return M;
   end Parse_Input;

   procedure Part_One (Input : String) is
      M : constant Map := Parse_Input (Input);

      function Count_Paths (Label : Device_Id) return Natural is
         Paths : Natural := 0;
      begin
         for Child of M (Label) loop
            if Child = "out" then
               Paths := Paths + 1;
            else
               Paths := Paths + Count_Paths (Child);
            end if;
         end loop;
         return Paths;
      end Count_Paths;

      Total_Paths : constant Natural := Count_Paths ("you");
   begin
      Ada.Text_IO.Put_Line ("Total paths: " & Total_Paths'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
   begin
      null;
   end Part_Two;
end Aoc.Day_11;
