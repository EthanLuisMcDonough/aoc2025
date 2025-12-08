with GNAT;
with Interfaces;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;

use Ada;
use GNAT;
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

   function Split_Lines (Input : String)
     return String_Split.Slice_Set
   is
   begin
      return String_Split.Create (Input,
        Characters.Latin_1.LF & "",
        String_Split.Multiple);
   end Split_Lines;

   function Split_Space_Trim (S : String)
     return String_Split.Slice_Set
   is
      Trimmed : constant String := Fixed.Trim (S, Both);
      Set : constant String_Split.Slice_Set :=
        String_Split.Create (Trimmed, " ", String_Split.Multiple);
   begin
      return Set;
   end Split_Space_Trim;

   function Uint64_Hash (H : Unsigned_64)
     return Ada.Containers.Hash_Type
   is
      MOD_MAX : constant Unsigned_64 := Unsigned_64'Val (
        Ada.Containers.Hash_Type'Last) + 1;
   begin
      return Ada.Containers.Hash_Type'Val (H mod MOD_MAX);
   end Uint64_Hash;

   function Int_Hash (H : Integer) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type'Val (H);
   end Int_Hash;
end Aoc.Common;
