with Interfaces;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.String_Split;
with Aoc.Common;

package body Aoc.Day_10 is

   function Parse_Machine (Line : String) return Machine is
      use Ada.Strings.Fixed;
      use GNAT.String_Split;

      Light_Start : constant Light_Index :=
        Light_Index (Index (Line, "[") + 1);
      Light_End : constant Light_Index :=
        Light_Index (Index (Line, "]") - 1);
      Jolt_Start : constant Positive := Positive (Index (Line, "{"));
      Jolt_End : constant Positive := Positive (Index (Line, "}"));
      Button_Count : constant Button_Index := Button_Index (Count (Line, "("));
      Light_Count : constant Light_Index := Light_End - Light_Start + 1;

      M : Machine := (
        Light_Count => Light_Count,
        Button_Count => Button_Count,
        Lights => (others => False),
        Buttons => (others => (others => False)),
        Joltage_Reqs => (others => 1));
      Next_Paren : Natural := Index (Line, "(");

      Jolt_Str : constant String := Line (Jolt_Start + 1 .. Jolt_End - 1);
      Jolts : constant Slice_Set := Create (Jolt_Str, ",", Multiple);
   begin
      for I in 1 .. Light_Count loop
         M.Lights (I) := (Line (Integer (Light_Start + I - 1)) = '#');
      end loop;

      for Btn_Id in 1 .. Button_Count loop
         declare
            Closing : constant Natural := Index (
              Line, ")", Positive (Next_Paren));
            Btn_Str : constant String := Line (Next_Paren + 1 .. Closing - 1);
            Nums : constant Slice_Set := Create (Btn_Str, ",", Multiple);
         begin
            for I in 1 .. Slice_Count (Nums) loop
               declare
                  Num : constant Natural := Natural'Value (Slice (Nums, I));
                  Light_Id : constant Light_Index := Light_Index (Num + 1);
               begin
                  M.Buttons (Btn_Id, Light_Id) := True;
               end;
            end loop;

            if Btn_Id /= Button_Count then
               Next_Paren := Index (Line, "(", Closing);
            end if;
         end;
      end loop;

      for I in 1 .. Slice_Count (Jolts) loop
         M.Joltage_Reqs (Light_Index (I)) := Natural'Value (Slice (Jolts, I));
      end loop;

      return M;
   end Parse_Machine;

   function Image (L : Light_Mask) return String is
      Ret : String (1 .. L'Length + 2);
   begin
      Ret (1) := '[';
      for I in L'Range loop
         Ret (Integer (I) + 1) := (if L (I) then '#' else '.');
      end loop;
      Ret (L'Length + 2) := ']';
      return Ret;
   end Image;

   procedure Part_One (Input : String) is
      use GNAT.String_Split;
      use Interfaces;
      Lines : constant Slice_Set := Common.Split_Lines (Input);
      Total_Presses : Unsigned_64 := 0;
   begin
      for I in 1 .. Slice_Count (Lines) loop
         declare
            M : constant Machine := Parse_Machine (Slice (Lines, I));
            Button_Combos : constant Positive :=
              2 ** Positive (M.Button_Count) - 1;
            Min_Presses : Unsigned_64 := Unsigned_64'Last;
         begin
            for I in 1 .. Button_Combos loop
               declare
                  Light : Light_Mask (1 .. M.Light_Count) := (others => False);
                  Val : Natural := I;
                  Presses : Unsigned_64 := 0;
               begin
                  for J in 1 .. M.Button_Count loop
                     exit when Val = 0;
                     if Val mod 2 = 1 then
                        for K in 1 .. M.Light_Count loop
                           Light (K) := Light (K) xor M.Buttons (J, K);
                        end loop;
                        Presses := Presses + 1;
                     end if;
                     Val := Val / 2;
                  end loop;

                  if Light = M.Lights then
                     Min_Presses := Unsigned_64'Min (Presses, Min_Presses);
                  end if;
               end;
            end loop;
            Total_Presses := Total_Presses + Min_Presses;
         end;
      end loop;
      Ada.Text_IO.Put_Line ("Total presses: " & Total_Presses'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
   begin
      null;
   end Part_Two;
end Aoc.Day_10;
