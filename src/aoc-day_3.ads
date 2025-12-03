with Interfaces;

package Aoc.Day_3 is
   function Max_Joltage
     (Line : String;
      Digit_Count : Positive)
      return Interfaces.Unsigned_64;

   function Sum_Joltage
     (Input : String;
      Digit_Count : Positive)
      return Interfaces.Unsigned_64;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_3;
