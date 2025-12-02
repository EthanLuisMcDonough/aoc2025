package Aoc.Day_1 is
   type Rot_Dir is (LEFT, RIGHT);
   for Rot_Dir use (
     LEFT  => Character'Pos ('L'),
     RIGHT => Character'Pos ('R'));

   type Safe_State is new Integer range 0 .. 99;

   function Safe_Turn (Safe : Safe_State;
     Dir : Rot_Dir; Turn : Positive)
     return Safe_State;

   function Safe_Wrap (Val : Integer) return Safe_State;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_1;
