with Aoc.Common;

package Aoc.Day_9 is
   type Coords is array (Positive range <>) of Common.Id_Pair;
   function Parse_Inputs (Input : String) return Coords;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_9;
