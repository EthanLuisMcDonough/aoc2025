with Interfaces;
with Ada.Strings.Hash;
with Ada.Containers.Hashed_Maps;

package Aoc.Day_11 is
   subtype Device_Str is String (1 .. 3);
   package Str_Key is new Ada.Containers.Hashed_Maps
     (Key_Type => Device_Str, Element_Type => Positive,
      Hash => Ada.Strings.Hash, Equivalent_Keys => "=");

   type Device_Mat is array
     (Positive range <>, Positive range <>)
      of Boolean;

   type Devices (Count : Positive) is record
      Device_Names : Str_Key.Map;
      Connections : Device_Mat (1 .. Count, 1 .. Count);
   end record;

   type Marking is (None, FFT, DAC, Both);
   function "and" (L, R : Marking) return Marking;

   function Parse_Input (Input : String) return Devices;

   generic
      with function Predicate (Ind : Positive)
        return Boolean;
   function Count_Paths (D : Devices; Start : Positive)
     return Interfaces.Unsigned_64;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_11;
