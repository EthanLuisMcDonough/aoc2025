package Aoc.Day_3 is
   type Joltage is range 11 .. 99;
   type Dec_Digit is range 1 .. 9;

   function Char_To_Digit (C : Character) return Dec_Digit;

   function Max_Joltage (Line : String) return Joltage;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_3;
