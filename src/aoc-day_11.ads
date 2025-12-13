with Ada.Strings.Hash;
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

package Aoc.Day_11 is
   subtype Device_Id is String (1 .. 3);
   package Devices is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Device_Id);

   use Devices;
   package Device_Map is new
     Ada.Containers.Hashed_Maps
       (Key_Type => Device_Id,
        Element_Type => Devices.Vector,
        Hash => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   function Parse_Input (Input : String)
     return Device_Map.Map;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_11;
