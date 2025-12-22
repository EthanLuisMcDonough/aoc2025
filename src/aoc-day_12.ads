with Ada.Containers.Vectors;
with Aoc.Common;

package Aoc.Day_12 is
   package Box_Bounds is new Common.Numeric_Pair
     (Num => Positive, Delim => 'x');

   type Present_Row is new Positive range 1 .. 3;
   type Present_Col is new Positive range 1 .. 3;

   type Present_Shape is array
     (Present_Row'Range, Present_Col'Range) of Boolean
   with Default_Component_Value => False;

   function Image (S : Present_Shape) return String;
   function Parse_Shape (S : String) return Present_Shape;
   procedure Flip_X (S : in out Present_Shape);
   procedure Flip_Y (S : in out Present_Shape);
   procedure Rotate (S : in out Present_Shape);

   type Shape_Variant is
     (Base, Rot_90, Rot_180, Rot_270,
      Flipped_X, Flipped_Y);
   type Shape_Variants is array
     (Positive range <>, Shape_Variant range <>)
      of Present_Shape;
   type Variant_Uniqueness is array
     (Positive range <>, Shape_Variant range <>)
      of Boolean;

   type Box_Arr is array (Positive range <>)
     of Box_Bounds.Pair;
   type Count_Requirements is array
     (Positive range <>, Positive range <>)
      of Natural;

   type Box_Requirements
     (Shape_Count : Positive;
      Box_Count : Positive) is
   record
      Shapes : Shape_Variants (1 .. Shape_Count, Shape_Variant'Range);
      Is_Unique : Variant_Uniqueness (1 .. Shape_Count, Shape_Variant'Range);
      Dimensions : Box_Arr (1 .. Box_Count);
      Requirements : Count_Requirements (1 .. Box_Count, 1 .. Shape_Count);
   end record;

   function Parse_Input (Input : String) return Box_Requirements;
   function Sum_Required_Boxes
     (Reqs : Box_Requirements; Box_Id : Positive)
      return Natural;
   function Can_Fit_All_In_Region
     (Reqs : Box_Requirements; Box_Id : Positive)
      return Boolean;

   type Gift_Region is array
     (Positive range <>, Positive range <>)
      of Boolean
   with Default_Component_Value => False;

   function Image (R : Gift_Region) return String;

   type Shape_Index is record
      Variant : Shape_Variant;
      Index : Positive;
   end record;

   type Attempt is record
      Shape : Shape_Index;
      Pos : Box_Bounds.Pair;
   end record;

   function Map_Col
     (Pos : Box_Bounds.Pair;
      J : Present_Col)
      return Positive is (Positive (Natural (J) -
                          Natural (Present_Col'First) +
                          Natural (Pos.Second)));
   function Map_Row
     (Pos : Box_Bounds.Pair;
      I : Present_Row)
      return Positive is (Positive (Natural (I) -
                          Natural (Present_Row'First) +
                          Natural (Pos.First)));

   function Can_Apply
     (A : Attempt; Reqs : Box_Requirements;
      Region : Gift_Region) return Boolean;

   procedure Apply
     (A : Attempt;
      Reqs : Box_Requirements;
      Region : in out Gift_Region;
      Do_Apply : Boolean);

   package Attempts is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Attempt);

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_12;
