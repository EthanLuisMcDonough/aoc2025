with Interfaces;

package Aoc.Day_7 is
   type Traversal_Data is record
      Splits : Natural;
      Paths : Interfaces.Unsigned_64;
   end record;

   function Traverse (Input : String)
     return Traversal_Data;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_7;
