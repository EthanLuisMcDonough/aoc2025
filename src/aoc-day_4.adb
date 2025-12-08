with Ada.Text_IO;
with Ada.Containers.Vectors;
with GNAT.String_Split;
with Aoc.Common;

use Ada;
use GNAT;

package body Aoc.Day_4 is
   package Int_Vec is new Containers.Vectors
     (Index_Type => Natural, Element_Type => Integer);

   function Encode_Pos
     (Rack : Roll_Rack; Row : Positive;
      Col : Positive) return Natural
   is
      R : constant Natural := Natural'Val (Row) - 1;
      C : constant Natural := Natural'Val (Col) - 1;
   begin
      return R * Rack'Length (1) + C;
   end Encode_Pos;

   procedure Decode_Pos
     (Rack : Roll_Rack; Pos : Natural;
      Row : out Positive; Col : out Positive)
   is
      C : constant Natural := Pos mod Rack'Length (1);
      R : constant Natural := Pos / Rack'Length (1);
   begin
      Row := Positive'Val (R + 1);
      Col := Positive'Val (C + 1);
   end Decode_Pos;

   function Read_Rack (Input : String) return Roll_Rack is
      Lines : constant String_Split.Slice_Set :=
        Common.Split_Lines (Input);
   begin
      declare
         Rows : constant Positive := Positive'Val (
           String_Split.Slice_Count (Lines));
         Cols : constant Positive := Positive'Val (
           String_Split.Slice (Lines, 1)'Length);
         Rack : Roll_Rack (1 .. Rows, 1 .. Cols) :=
           (others => (others => EMPTY));
      begin
         for I in 1 .. String_Split.Slice_Count (Lines) loop
            declare
               Row : constant String := String_Split.Slice (Lines, I);
            begin
               for J in Row'Range loop
                  if Row (J) = '@' then
                     Rack (Positive'Val (I),
                       Positive'Val (J - Row'First + 1)) := ROLL;
                  end if;
               end loop;
            end;
         end loop;
         return Rack;
      end;
   end Read_Rack;

   function Can_Take_Roll
     (Rack : Roll_Rack; Row : Positive;
      Col : Positive) return Boolean
   is
      Adjacent : Natural := 0;
      Lo_Check : constant Positive := Positive'Val (
        Natural'Max (Natural'Val (Col) - 1, 1));
      Hi_Check : constant Integer := Positive'Min (
        Col + 1, Rack'Last (2));
   begin
      if Rack (Row, Col) /= ROLL then
         return False;
      end if;

      Adjacent := Boolean'Pos (
        Col > 1 and then Rack (Row, Col - 1) = ROLL) +
        Boolean'Pos (Col < Rack'Length (1) and then
          Rack (Row, Col + 1) = ROLL);

      if Row > 1 then
         for K in Lo_Check .. Hi_Check loop
            if Rack (Row - 1, K) = ROLL then
               Adjacent := Adjacent + 1;
            end if;
         end loop;
      end if;

      if Row < Rack'Last (1) then
         for K in Lo_Check .. Hi_Check loop
            if Rack (Row + 1, K) = ROLL then
               Adjacent := Adjacent + 1;
            end if;
         end loop;
      end if;

      return Adjacent < 4;
   end Can_Take_Roll;

   function Take_Rolls (Rack : in out Roll_Rack) return Natural is
      To_Take : Int_Vec.Vector;
   begin
      for I in Rack'Range (1) loop
         for J in Rack'Range (2) loop
            if Can_Take_Roll (Rack, I, J) then
               Int_Vec.Append (To_Take, Encode_Pos (Rack, I, J));
            end if;
         end loop;
      end loop;

      for Pos of To_Take loop
         declare
            Row, Col : Positive;
         begin
            Decode_Pos (Rack, Pos, Row, Col);
            Rack (Row, Col) := EMPTY;
         end;
      end loop;

      return Integer'Val (Int_Vec.Length (To_Take));
   end Take_Rolls;

   procedure Part_One (Input : String) is
      Rack : Roll_Rack := Read_Rack (Input);
      Accessible_Rolls : constant Integer := Take_Rolls (Rack);
   begin
      Text_IO.Put_Line ("Accessible rolls: " & Accessible_Rolls'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
      Rack : Roll_Rack := Read_Rack (Input);
      Total_Removed : Integer := 0;
      Local_Removed : Integer := 0;
   begin
      loop
         Local_Removed := Take_Rolls (Rack);
         exit when Local_Removed = 0;
         Total_Removed := Total_Removed + Local_Removed;
      end loop;
      Text_IO.Put_Line ("Total removed: " & Total_Removed'Image);
   end Part_Two;
end Aoc.Day_4;
