package Aoc.Day_4 is
   type Rack_Slot is (EMPTY, ROLL);
   type Roll_Rack is array (Positive range <>,
     Positive range <>) of Rack_Slot;

   function Read_Rack (Input : String) return Roll_Rack;

   function Encode_Pos
     (Rack : Roll_Rack; Row : Positive;
      Col : Positive) return Natural;
   procedure Decode_Pos
     (Rack : Roll_Rack; Pos : Natural;
      Row : out Positive; Col : out Positive);

   function Can_Take_Roll
     (Rack : Roll_Rack; Row : Positive;
      Col : Positive) return Boolean;
   function Take_Rolls (Rack : in out Roll_Rack)
     return Natural;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_4;
