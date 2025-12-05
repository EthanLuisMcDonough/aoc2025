with Interfaces;

package Aoc.Common is
   type Id_Pair is record
      First  : Interfaces.Unsigned_64;
      Second : Interfaces.Unsigned_64;
   end record;

   function Parse_Pair (S : String) return Id_Pair;
   function Pair_Membership
     (Id : Interfaces.Unsigned_64;
      Pair : Id_Pair) return Boolean;
   function Pair_Overlap (A : Id_Pair; B : Id_Pair)
     return Boolean;
end Aoc.Common;
