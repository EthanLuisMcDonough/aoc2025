with Interfaces;
with Ada.Strings.Fixed;

use Ada;
use Interfaces;
use Ada.Strings;

package body Aoc.Common is
   function Parse_Pair (S : String) return Id_Pair is
      Split_Ind : constant Natural := Fixed.Index (S, "-");
      Str_One   : constant String := S (S'First .. Split_Ind - 1);
      Str_Two   : constant String := S (Split_Ind + 1 .. S'Last);
      First_Id  : constant Unsigned_64 := Unsigned_64'Value (Str_One);
      Second_Id : constant Unsigned_64 := Unsigned_64'Value (Str_Two);
   begin
      return (First => First_Id, Second => Second_Id);
   end Parse_Pair;
end Aoc.Common;
