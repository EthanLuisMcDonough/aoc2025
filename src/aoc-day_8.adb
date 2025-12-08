with Interfaces;
with Aoc.Common;
with GNAT.String_Split;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;
with Ada.Text_IO;

use Ada;
use Interfaces;

package body Aoc.Day_8 is
   package Int_Set is new Ada.Containers.Hashed_Sets
     (Element_Type => Positive,
      Hash => Common.Int_Hash,
      Equivalent_Elements => "=");

   package Circuit_Map is new Ada.Containers.Hashed_Maps
     (Element_Type => Int_Set.Set,
      Key_Type => Positive,
      Hash => Common.Int_Hash,
      Equivalent_Keys => "=",
      "=" => Int_Set."=");

   function Parse_Input (Input : String) return Playground is
      use GNAT.String_Split;
      Lines : constant Slice_Set := Common.Split_Lines (Input);
      Count : constant Positive := Positive'Val (Slice_Count (Lines));
      Boxes : Junction_Boxes (1 .. Count, Dimension);
   begin
      for I in 1 .. Slice_Count (Lines) loop
         declare
            Line : constant String := Slice (Lines, I);
            Nums : constant Slice_Set := Create (Line, ",", Multiple);
         begin
            for J in 1 .. Slice_Count (Nums) loop
               declare
                  Num_Str : constant String := Slice (Nums, J);
                  Num : constant Natural := Natural'Value (Num_Str);
               begin
                  Boxes (Positive'Val (I), Dimension'Val (J)) := Num;
               end;
            end loop;
         end;
      end loop;
      return (Count => Count, Boxes => Boxes);
   end Parse_Input;

   function Dist_Sq_Between
     (P : Playground; A : Positive; B : Positive)
      return Unsigned_64
   is
      Dot : Unsigned_64 := 0;
   begin
      for I in Dimension'Range loop
         declare
            I_Diff : constant Integer := Integer'Val (
              P.Boxes (A, I)) - Integer'Val (P.Boxes (B, I));
            Diff : constant Unsigned_64 := Unsigned_64'Val (abs I_Diff);
         begin
            Dot := Dot + Diff * Diff;
         end;
      end loop;
      return Dot;
   end Dist_Sq_Between;

   procedure Part_One (Input : String) is
      P : constant Playground := Parse_Input (Input);
      subtype Box_Id is Positive range 1 .. P.Count;
      subtype Connection_Count is Positive range 1 .. 1000;

      Circuit_Lookup : array (Box_Id) of Natural := (others => 0);
      Visited : array (Box_Id, Box_Id) of Boolean :=
        (others => (others => False));
      Circuits : Circuit_Map.Map;
      Max_Circuits : array (1 .. 3) of Natural := (others => 1);
      Next_Circuit_Id : Natural := 1;
      Circuit_Product : Unsigned_64 := 1;
   begin
      for I in Connection_Count'Range loop
         declare
            A, B : Box_Id;
            Dist : Unsigned_64 := Unsigned_64'Last;
            Any_Visited : Boolean := False;
         begin
            for J in 1 .. P.Count - 1 loop
               for K in J + 1 .. P.Count loop
                  if not Visited (J, K) then
                     declare
                        Local_Dist : constant Unsigned_64 :=
                          Dist_Sq_Between (P, J, K);
                     begin
                        if Local_Dist < Dist then
                           A := J;
                           B := K;
                           Dist := Local_Dist;
                           Any_Visited := True;
                        end if;
                     end;
                  end if;
               end loop;
            end loop;

            if Any_Visited then
               declare
                  Circ_A : constant Natural := Circuit_Lookup (A);
                  Circ_B : constant Natural := Circuit_Lookup (B);
               begin
                  --  Create new circuit
                  if Circ_A = 0 and then Circ_B = 0 then
                     Circuit_Lookup (A) := Next_Circuit_Id;
                     Circuit_Lookup (B) := Next_Circuit_Id;
                     declare
                        New_Circuit : Int_Set.Set;
                     begin
                        Int_Set.Reserve_Capacity (New_Circuit, 2);
                        Int_Set.Insert (New_Circuit, A);
                        Int_Set.Insert (New_Circuit, B);
                        Circuit_Map.Insert (Circuits,
                          Next_Circuit_Id, New_Circuit);
                     end;
                     Next_Circuit_Id := Next_Circuit_Id + 1;

                  --  Nothing to be done
                  elsif Circ_A = Circ_B then
                     null;

                  --  Add B to A
                  elsif Circ_A > 0 and then Circ_B = 0 then
                     Int_Set.Insert (Circuits (Circ_A), B);
                     Circuit_Lookup (B) := Circ_A;

                  --  Add A to B
                  elsif Circ_B > 0 and then Circ_A = 0 then
                     Int_Set.Insert (Circuits (Circ_B), A);
                     Circuit_Lookup (A) := Circ_B;

                  --  Merge B into A and delete B
                  else
                     for I of Circuits (Circ_B) loop
                        Circuit_Lookup (I) := Circ_A;
                     end loop;
                     Int_Set.Union (Circuits (Circ_A), Circuits (Circ_B));
                     Circuit_Map.Delete (Circuits, Circ_B);
                  end if;
               end;

               Visited (A, B) := True;
            end if;
         end;
      end loop;

      for C in Circuit_Map.Iterate (Circuits) loop
         declare
            Circuit_Size : constant Natural := Natural'Val (
              Int_Set.Length (Circuits (C)));
         begin
            for I in Max_Circuits'Range loop
               if Circuit_Size > Max_Circuits (I) then
                  for J in reverse I + 1 .. Max_Circuits'Last loop
                     Max_Circuits (J) := Max_Circuits (J - 1);
                  end loop;
                  Max_Circuits (I) := Circuit_Size;
                  exit;
               end if;
            end loop;
         end;
      end loop;

      for I of Max_Circuits loop
         Circuit_Product := Circuit_Product * Unsigned_64'Val (I);
      end loop;

      Text_IO.Put_Line ("Product: " & Circuit_Product'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
   begin
      null;
   end Part_Two;
end Aoc.Day_8;
