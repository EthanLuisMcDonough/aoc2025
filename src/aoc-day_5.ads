with Interfaces;
with Ada.Containers.Vectors;
with Aoc.Common;

use Interfaces;
use Aoc.Common;

package Aoc.Day_5 is
   package Ingredient_Range is new Common.Numeric_Pair
     (Num => Unsigned_64);

   package Dates_Vec is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Ingredient_Range.Pair,
      "=" => Ingredient_Range."=");
   package Uint64_Vec is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Unsigned_64);

   type Database is record
      Fresh_Ranges : Dates_Vec.Vector;
      Ingredients  : Uint64_Vec.Vector;
   end record;

   function Parse_Input (Input : String) return Database;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_5;
