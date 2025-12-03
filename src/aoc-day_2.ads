with Interfaces;

package Aoc.Day_2 is
   type Id_Pair is record
      First  : Interfaces.Unsigned_64;
      Second : Interfaces.Unsigned_64;
   end record;

   function Parse_Pair (S : String) return Id_Pair;

   function Check_Repeat (Id : Interfaces.Unsigned_64)
     return Boolean;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_2;
