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

   function Pair_Membership
     (Id : Interfaces.Unsigned_64;
      Pair : Id_Pair) return Boolean
   is
   begin
      return Id >= Pair.First and then Id <= Pair.Second;
   end Pair_Membership;

   function Pair_Overlap (A : Id_Pair; B : Id_Pair)
     return Boolean
   is
   begin
      return Pair_Membership (A.First, B) or else
        Pair_Membership (B.First, A) or else
        Pair_Membership (A.Second, B) or else
        Pair_Membership (B.Second, A);
   end Pair_Overlap;
end Aoc.Common;
