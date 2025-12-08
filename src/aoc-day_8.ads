with Interfaces;
with Aoc.Common;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;

package Aoc.Day_8 is
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

   type Junction_Boxes is array (Positive range <>,
     Positive range <>) of Natural;
   subtype Dimension is Positive range 1 .. 3;

   type Playground (Count : Positive) is record
      Boxes : Junction_Boxes (1 .. Count, Dimension);
   end record;

   function Dist_Sq_Between
     (P : Playground; A : Positive; B : Positive)
      return Interfaces.Unsigned_64;

   function Parse_Input (Input : String) return Playground;

   type Circuit_Lookup is array (Positive range <>) of Natural;
   type Visited_Lookup is array (Positive range <>,
     Positive range <>) of Boolean;

   type Connections (Count : Positive) is record
      Lookup : Circuit_Lookup (1 .. Count) := (others => 0);
      Visited : Visited_Lookup (1 .. Count, 1 .. Count) :=
        (others => (others => False));
      Circuits : Circuit_Map.Map;
      Next_Id : Natural := 1;
      Last_A, Last_B : Natural := 0;
   end record;

   procedure Connect_Closest (P : Playground; C : in out Connections);
   function All_Connected (C : Connections) return Boolean;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_8;
