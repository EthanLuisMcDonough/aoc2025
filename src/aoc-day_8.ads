with Interfaces;
with Aoc.Common;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;

use Interfaces;

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

   type Circuit_Lookup is array (Positive range <>) of Natural;
   type Visited_Lookup is array (Positive range <>,
     Positive range <>) of Boolean;

   type Connection is record
      A, B : Positive;
      Dist : Unsigned_64;
   end record;
   function "<" (L, R : Connection) return Boolean
     is (L.Dist < R.Dist);

   type Connections is array (Positive range <>) of Connection;

   type Circuits (Count : Positive) is record
      Lookup : Circuit_Lookup (1 .. Count) := (others => 0);
      Circuits : Circuit_Map.Map;
      Next_Id : Natural := 1;
   end record;

   function Dist_Sq_Between
     (P : Playground; A : Positive; B : Positive)
      return Unsigned_64;
   function Calc_Connections (P : Playground)
     return Connections;
   function Connect
     (C : in out Circuits; A : Positive; B : Positive)
      return Boolean;

   function Parse_Input (Input : String) return Playground;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_8;
