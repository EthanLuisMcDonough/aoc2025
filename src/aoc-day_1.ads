package Aoc.Day_1 is
   type Rot_Dir is (LEFT, RIGHT);
   for Rot_Dir use (
     LEFT  => Character'Pos ('L'),
     RIGHT => Character'Pos ('R'));

   type Safe_State is new Integer range 0 .. 99;

   type Safe_Recorder is record
      State : Safe_State;
      Click : Natural;
   end record;

   type Safe_Turn is record
      Dir : Rot_Dir;
      Len : Positive;
   end record;

   function Parse_Turn (S : String)
     return Safe_Turn;

   procedure Turn_Safe
     (Safe : in out Safe_Recorder;
      Turn : Safe_Turn);

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_1;
