with GNAT;
with Interfaces;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;

use Ada;
use GNAT;
use Interfaces;
use Ada.Strings;

package body Aoc.Common is
   package body Numeric_Pair is
      function Parse (S : String) return Pair is
         Split_Ind : constant Natural := Fixed.Index (S, "" & Delim);
         Str_One   : constant String := S (S'First .. Split_Ind - 1);
         Str_Two   : constant String := S (Split_Ind + 1 .. S'Last);
         First_Id  : constant Num := Num'Value (Str_One);
         Second_Id : constant Num := Num'Value (Str_Two);
      begin
         return (First => First_Id, Second => Second_Id);
      end Parse;

      function Membership (I : Num; P : Pair) return Boolean is
      begin
         return I >= Get_Lower (P) and then I <= Get_Upper (P);
      end Membership;

      function Overlap (A : Pair; B : Pair) return Boolean is
      begin
         return Membership (A.First, B) or else
           Membership (B.First, A) or else
           Membership (A.Second, B) or else
           Membership (B.Second, A);
      end Overlap;
   end Numeric_Pair;

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
