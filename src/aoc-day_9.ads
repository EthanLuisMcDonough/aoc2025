with Interfaces;
with Aoc.Common;
with Ada.Containers.Vectors;

package Aoc.Day_9 is
   package Coord is new Common.Numeric_Pair
     (Num => Natural, Delim => ',');
   package Wall_Span is new Common.Numeric_Pair
     (Num => Natural);

   type Orientation is (Horizontal, Vertical);

   type Green_Wall is record
      Span : Wall_Span.Pair;
      Pos : Natural;
      Orient : Orientation;
   end record;

   --  Checks if two walls intersect
   function Intersect (A : Green_Wall; B : Green_Wall)
     return Boolean;

   function Asc (W : Green_Wall) return Boolean
     is (W.Span.First < W.Span.Second);
   function Desc (W : Green_Wall) return Boolean
     is (W.Span.First > W.Span.Second);

   package Walls is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Green_Wall);

   subtype Box_Bounds is Walls.Vector;

   --  Gets the area of a rectangle defined by two points
   function Corner_Area (A : Coord.Pair; B : Coord.Pair)
     return Interfaces.Unsigned_64;

   type Coords is array (Positive range <>) of Coord.Pair;
   function Parse_Inputs (Input : String) return Coords;

   --  Get outer bounds as a set of walls
   function Calc_Bounds (C : Coords) return Box_Bounds;

   --  Create a wall that outlines the outer bounds
   function Calc_Outline (B : Box_Bounds) return Box_Bounds;

   --  Uses raycasting to determine if a point lies inside
   --  the given bounds
   function In_Bounds (P : Coord.Pair; B : Box_Bounds)
     return Boolean;

   --  Checks if a square defined by two points does not intersect
   --  any of the given box bounds
   function Rect_In_Bounds
     (A : Coord.Pair; B : Coord.Pair;
      Outline : Box_Bounds) return Boolean;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_9;
