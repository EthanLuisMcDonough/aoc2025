package Aoc.Day_10 is
   type Light_Index is new Positive;
   type Button_Index is new Positive;

   type Light_Mask is array (Light_Index range <>) of Boolean;
   type Joltages is array (Light_Index range <>) of Natural;

   type Button_Mat is array (Button_Index range <>,
     Light_Index range <>) of Boolean;

   function Image (L : Light_Mask) return String;

   type Machine
     (Light_Count : Light_Index;
      Button_Count : Button_Index) is
   record
      Lights : Light_Mask (1 .. Light_Count);
      Buttons : Button_Mat (1 .. Button_Count, 1 .. Light_Count);
      Joltage_Reqs : Joltages (1 .. Light_Count);
   end record;

   function Parse_Machine (Line : String) return Machine;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_10;
