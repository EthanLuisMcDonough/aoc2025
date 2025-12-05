with Interfaces;

package Aoc.Common is
   type Id_Pair is record
      First  : Interfaces.Unsigned_64;
      Second : Interfaces.Unsigned_64;
   end record;

   function Parse_Pair (S : String) return Id_Pair;
end Aoc.Common;
