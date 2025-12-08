with Interfaces;
with GNAT.String_Split;
with Ada.Text_IO;

use Ada;
use Interfaces;

package body Aoc.Day_8 is
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

   function All_Connected (C : Connections) return Boolean is
      First : constant Natural := C.Lookup (1);
   begin
      if First = 0 then
         return False;
      end if;

      for I in 2 .. C.Count loop
         if C.Lookup (I) /= First then
            return False;
         end if;
      end loop;

      return True;
   end All_Connected;

   procedure Connect_Closest (P : Playground; C : in out Connections) is
      A, B : Positive;
      Dist : Unsigned_64 := Unsigned_64'Last;
      Any_Visited : Boolean := False;
   begin
      for J in 1 .. P.Count - 1 loop
         for K in J + 1 .. P.Count loop
            if not C.Visited (J, K) then
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
            Circ_A : constant Natural := C.Lookup (A);
            Circ_B : constant Natural := C.Lookup (B);
            Any_Writes : Boolean := True;
         begin
            --  Create new circuit
            if Circ_A = 0 and then Circ_B = 0 then
               C.Lookup (A) := C.Next_Id;
               C.Lookup (B) := C.Next_Id;
               declare
                  New_Circuit : Int_Set.Set;
               begin
                  Int_Set.Reserve_Capacity (New_Circuit, 2);
                  Int_Set.Insert (New_Circuit, A);
                  Int_Set.Insert (New_Circuit, B);
                  Circuit_Map.Insert (C.Circuits,
                     C.Next_Id, New_Circuit);
               end;
               C.Next_Id := C.Next_Id + 1;

            --  Nothing to be done
            elsif Circ_A = Circ_B then
               Any_Writes := False;

            --  Add B to A
            elsif Circ_A > 0 and then Circ_B = 0 then
               Int_Set.Insert (C.Circuits (Circ_A), B);
               C.Lookup (B) := Circ_A;

            --  Add A to B
            elsif Circ_B > 0 and then Circ_A = 0 then
               Int_Set.Insert (C.Circuits (Circ_B), A);
               C.Lookup (A) := Circ_B;

            --  Merge B into A and delete B
            else
               for I of C.Circuits (Circ_B) loop
                  C.Lookup (I) := Circ_A;
               end loop;
               Int_Set.Union (C.Circuits (Circ_A), C.Circuits (Circ_B));
               Circuit_Map.Delete (C.Circuits, Circ_B);
            end if;

            C.Visited (A, B) := True;
            if Any_Writes then
               C.Last_A := A;
               C.Last_B := B;
            end if;
         end;
      end if;
   end Connect_Closest;

   procedure Part_One (Input : String) is
      P : constant Playground := Parse_Input (Input);
      C : Connections := (Count => P.Count, others => <>);
      subtype Connection_Count is Positive range 1 .. 1000;
      Max_Circuits : array (1 .. 3) of Natural := (others => 1);
      Circuit_Product : Unsigned_64 := 1;
   begin
      for I in Connection_Count'Range loop
         Connect_Closest (P, C);
      end loop;

      for Circuit in Circuit_Map.Iterate (C.Circuits) loop
         declare
            Circuit_Size : constant Natural := Natural'Val (
              Int_Set.Length (C.Circuits (Circuit)));
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
      P : constant Playground := Parse_Input (Input);
      C : Connections := (Count => P.Count, others => <>);
      Finished : Boolean := False;
      X_Product : Natural := 0;
   begin
      while not Finished loop
         Connect_Closest (P, C);
         Finished := All_Connected (C);
      end loop;

      X_Product := P.Boxes (C.Last_A, 1) * P.Boxes (C.Last_B, 1);
      Text_IO.Put_Line ("X-Product: " & X_Product'Image);
   end Part_Two;
end Aoc.Day_8;
