with Interfaces;
with GNAT.String_Split;
with Ada.Containers;

package Aoc.Common is
   type Id_Pair is record
      First  : Interfaces.Unsigned_64;
      Second : Interfaces.Unsigned_64;
   end record;

   function Parse_Pair (S : String) return Id_Pair;
   function Pair_Membership
     (Id : Interfaces.Unsigned_64;
      Pair : Id_Pair) return Boolean;
   function Pair_Overlap (A : Id_Pair; B : Id_Pair)
     return Boolean;

   function Split_Lines (Input : String)
     return GNAT.String_Split.Slice_Set;
   function Split_Space_Trim (S : String)
     return GNAT.String_Split.Slice_Set;

   function Uint64_Hash (H : Interfaces.Unsigned_64)
     return Ada.Containers.Hash_Type;
   function Int_Hash (H : Integer)
     return Ada.Containers.Hash_Type;
end Aoc.Common;
