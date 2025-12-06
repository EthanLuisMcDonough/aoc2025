package Aoc.Day_6 is
   type Operator is (Plus, Mult);

   type Grid is array (Positive range <>,
     Positive range <>) of Positive;
   type Operators is array (Positive range <>)
     of Operator;

   type Homework (Rows : Positive; Cols : Positive) is record
      Nums : Grid (1 .. Rows, 1 .. Cols);
      Ops : Operators (1 .. Cols);
   end record;

   function Parse_Homework (Input : String) return Homework;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_6;
