with Interfaces;

package Aoc.Day_8 is
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

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_8;
