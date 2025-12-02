package Aoc is
   type Day_Id is new Integer range 1 .. 12;
   type Day_Part is (BOTH_PARTS, FIRST_PART, SECOND_PART);

   procedure Unimplemented;
   procedure Handle_Day (Id : Day_Id; P : Day_Part);
end Aoc;
