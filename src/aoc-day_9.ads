with Interfaces;
with Aoc.Common;

package Aoc.Day_9 is
   package Coord is new Common.Numeric_Pair
     (Num => Natural, Delim => ',');

   function Corner_Area (A : Coord.Pair; B : Coord.Pair)
     return Interfaces.Unsigned_64;

   type Coords is array (Positive range <>) of Coord.Pair;
   function Parse_Inputs (Input : String) return Coords;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_9;
