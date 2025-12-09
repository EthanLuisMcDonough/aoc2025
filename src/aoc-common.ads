with Interfaces;
with GNAT.String_Split;
with Ada.Containers;

package Aoc.Common is
   generic
      type Num is (<>);
      Delim : Character := '-';
   package Numeric_Pair is
      type Pair is record
         First  : Num;
         Second : Num;
      end record;

      overriding function "=" (A : Pair; B : Pair) return Boolean
        is (A.First = B.First and then A.Second = B.Second);
      function Parse (S : String) return Pair;
      function Overlap (A : Pair; B : Pair) return Boolean;
      function Membership (I : Num; P : Pair) return Boolean;
      function Get_Upper (P : Pair) return Num
        is (Num'Max (P.First, P.Second));
      function Get_Lower (P : Pair) return Num
        is (Num'Min (P.First, P.Second));
   end Numeric_Pair;

   function Split_Lines (Input : String)
     return GNAT.String_Split.Slice_Set;
   function Split_Space_Trim (S : String)
     return GNAT.String_Split.Slice_Set;

   function Uint64_Hash (H : Interfaces.Unsigned_64)
     return Ada.Containers.Hash_Type;
   function Int_Hash (H : Integer)
     return Ada.Containers.Hash_Type;
end Aoc.Common;
