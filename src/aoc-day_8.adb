with Ada.Text_IO;
with GNAT.String_Split;
with Ada.Containers.Generic_Array_Sort;

use Ada;

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

   function Calc_Connections (P : Playground) return Connections is
      procedure Conn_Sort is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive,
         Element_Type => Connection,
         Array_Type => Connections);
      Edge_Count : constant Positive := (P.Count * (P.Count - 1)) / 2;
      C : Connections (1 .. Edge_Count);
      Index : Positive := 1;
   begin
      for I in 1 .. P.Count - 1 loop
         for J in I + 1 .. P.Count loop
            C (Index) := (A => I, B => J,
              Dist => Dist_Sq_Between (P, I, J));
            Index := Index + 1;
         end loop;
      end loop;
      Conn_Sort (C);
      return C;
   end Calc_Connections;

   function Connect
     (C : in out Circuits; A : Positive; B : Positive)
      return Boolean
   is
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

      return Any_Writes;
   end Connect;

   procedure Part_One (Input : String) is
      P : constant Playground := Parse_Input (Input);
      Conn : constant Connections := Calc_Connections (P);
      Circ : Circuits := (Count => P.Count, others => <>);
      Max_Circuits : array (1 .. 3) of Natural := (others => 1);
      Circuit_Product : Unsigned_64 := 1;
   begin
      for I in Conn'First .. Positive'Min (Conn'Last, 1000) loop
         declare
            C : constant Connection := Conn (I);
            Unused : constant Boolean := Connect (Circ, C.A, C.B);
         begin
            pragma Unreferenced (Unused);
         end;
      end loop;

      for Circuit in Circuit_Map.Iterate (Circ.Circuits) loop
         declare
            Circuit_Size : constant Natural := Natural'Val (
              Int_Set.Length (Circ.Circuits (Circuit)));
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
      Conn : constant Connections := Calc_Connections (P);
      Circ : Circuits := (Count => P.Count, others => <>);
      Last_A, Last_B : Positive;
      X_Product : Natural := 0;
   begin
      for Connection of Conn loop
         if Connect (Circ, Connection.A, Connection.B) then
            Last_A := Connection.A;
            Last_B := Connection.B;
         end if;
      end loop;

      X_Product := P.Boxes (Last_A, 1) * P.Boxes (Last_B, 1);
      Text_IO.Put_Line ("X-Product: " & X_Product'Image);
   end Part_Two;
end Aoc.Day_8;
