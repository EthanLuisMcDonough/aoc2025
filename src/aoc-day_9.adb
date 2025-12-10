with Interfaces;
with Ada.Text_IO;
with GNAT.String_Split;

use Ada;
use Interfaces;

package body Aoc.Day_9 is
   function Corner_Area (A : Coord.Pair; B : Coord.Pair)
     return Interfaces.Unsigned_64
   is
      A_X : constant Integer_64 := Integer_64 (A.First);
      B_X : constant Integer_64 := Integer_64 (B.First);
      A_Y : constant Integer_64 := Integer_64 (A.Second);
      B_Y : constant Integer_64 := Integer_64 (B.Second);
      Diff_X : constant Unsigned_64 := Unsigned_64 (abs (A_X - B_X) + 1);
      Diff_Y : constant Unsigned_64 := Unsigned_64 (abs (A_Y - B_Y) + 1);
   begin
      return Diff_X * Diff_Y;
   end Corner_Area;

   function Parse_Inputs (Input : String) return Coords is
      use GNAT.String_Split;
      Lines : constant Slice_Set := Common.Split_Lines (Input);
      Coord_Count : constant Positive :=
        Positive'Val (Slice_Count (Lines));
      C : Coords (1 .. Coord_Count);
   begin
      for I in 1 .. Slice_Count (Lines) loop
         declare
            Line : constant String := Slice (Lines, I);
         begin
            C (Positive'Val (I)) := Coord.Parse (Line);
         end;
      end loop;
      return C;
   end Parse_Inputs;

   function Calc_Bounds (C : Coords) return Box_Bounds is
      use Coord;
      use Walls;
      Bounds : Box_Bounds;
   begin
      Bounds.Reserve_Capacity (C'Length);
      for I in C'Range loop
         declare
            Next_I : constant Positive := Positive (
              (Natural (I) mod C'Length) + 1);
            Current : constant Pair := C (I);
            Next : constant Pair := C (Next_I);

            procedure Handle_Vertex
              (O : Orientation; S : Natural;
               E : Natural; Pos : Natural)
            is
               Span : constant Wall_Span.Pair :=
                 (First => S, Second => E);
            begin
               if not Bounds.Is_Empty and then
                 Bounds.Last_Element.Orient = O
               then
                  Bounds (Bounds.Last_Index).Span.Second := E;
               else
                  Walls.Append (Bounds, (Span => Span,
                    Pos => Pos, Orient => O));
               end if;
            end Handle_Vertex;
         begin
            if Current = Next then
               pragma Assert (False);

            elsif Current.First = Next.First then
               Handle_Vertex (Vertical, Current.Second,
                 Next.Second, Current.First);

            elsif Current.Second = Next.Second then
               Handle_Vertex (Horizontal, Current.First,
                 Next.First, Current.Second);

            else
               pragma Assert (False);
            end if;
         end;
      end loop;
      return Bounds;
   end Calc_Bounds;

   function Calc_Outline (B : Box_Bounds) return Box_Bounds is
      use Walls;
      O : Box_Bounds;
   begin
      O.Reserve_Capacity (B.Length);
      for B_Ind in B.First_Index .. B.Last_Index loop
         declare
            Next_Ind : constant Positive :=
              (if B_Ind = B.Last_Index then
               1 else B_Ind + 1);
            Prev_Ind : constant Positive :=
              (if B_Ind = B.First_Index then
               B.Last_Index else B_Ind - 1);

            Curr : constant Green_Wall := B (B_Ind);
            Next : constant Green_Wall := B (Next_Ind);
            Prev : constant Green_Wall := B (Prev_Ind);

            Wall : Green_Wall;
            Pos : Natural;

            First_Outer : constant Natural :=
              (if Asc (Curr) then
               Curr.Span.First - 1 else
               Curr.Span.First + 1);
            Second_Outer : constant Natural :=
              (if Asc (Curr) then
               Curr.Span.Second + 1 else
               Curr.Span.Second - 1);

            First_Inner : constant Natural :=
              (if Asc (Curr) then
               Curr.Span.First + 1 else
               Curr.Span.First - 1);
            Second_Inner : constant Natural :=
              (if Asc (Curr) then
               Curr.Span.Second - 1 else
               Curr.Span.Second + 1);

            First, Second : Natural;

            Mid : constant Integer := (
              Wall_Span.Get_Upper (Curr.Span) -
              Wall_Span.Get_Lower (Curr.Span)) / 2 +
              Wall_Span.Get_Lower (Curr.Span);
            Check_Point : constant Coord.Pair := (
               First => (if Curr.Orient = Vertical then
                         Curr.Pos + 1 else Mid),
               Second => (if Curr.Orient = Vertical then
                          Mid else Curr.Pos + 1));
            Is_Above : constant Boolean := not In_Bounds (Check_Point, B);
         begin
            if Is_Above then
               Pos := Curr.Pos + 1;
            else
               Pos := Curr.Pos - 1;
            end if;

            --  Above + previous ascending (top left angle)
            if Is_Above and then Asc (Prev) then
               First := First_Outer;

            --  Above + prev descending (bottom left angle)
            elsif Is_Above and then Desc (Prev) then
               First := First_Inner;

            --  Below + previous asending (top right angle)
            elsif not Is_Above and then Asc (Prev) then
               First := First_Inner;

            --  Below + previous descending (bottom right angle)
            else
               First := First_Outer;
            end if;

            --  Above + next ascending (bottom left angle)
            if Is_Above and then Asc (Next) then
               Second := Second_Inner;

            --  Above + next descending (top right angle)
            elsif Is_Above and then Desc (Next) then
               Second := Second_Outer;

            --  Below + next ascending (bottom left angle)
            elsif not Is_Above and then Asc (Next) then
               Second := Second_Outer;

            --  Below + next descending (top left angle)
            else
               Second := Second_Inner;
            end if;

            Wall := (Span => (First => First, Second => Second),
              Pos => Pos, Orient => Curr.Orient);
            O.Append (Wall);
         end;
      end loop;
      return O;
   end Calc_Outline;

   function Intersect (A : Green_Wall; B : Green_Wall) return Boolean is
   begin
      return A.Orient /= B.Orient and then
        Wall_Span.Membership (A.Pos, B.Span) and then
        Wall_Span.Membership (B.Pos, A.Span);
   end Intersect;

   function Rect_In_Bounds
     (A : Coord.Pair; B : Coord.Pair;
      Outline : Box_Bounds) return Boolean
   is
      X_Span : constant Wall_Span.Pair := (
        First => A.First, Second => B.First);
      Y_Span : constant Wall_Span.Pair := (
        First => A.Second, Second => B.Second);

      --  (A.X, A.Y), (B.X, A.Y)
      HW1 : constant Green_Wall := (
        Orient => Horizontal,
        Pos => A.Second,
        Span => X_Span);

      --  (A.X, B.Y), (B.X, B.Y)
      HW2 : constant Green_Wall := (
        Orient => Horizontal,
        Pos => B.Second,
        Span => X_Span);

      --  (A.X, A.Y), (A.X, B.Y)
      VW1 : constant Green_Wall := (
        Orient => Vertical,
        Pos => B.First,
        Span => Y_Span);

      --  (B.X, A.Y), (B.X, B.Y)
      VW2 : constant Green_Wall := (
        Orient => Vertical,
        Pos => B.Second,
        Span => Y_Span);

      Box_Walls : constant array (1 .. 4) of Green_Wall :=
        (HW1, HW2, VW1, VW2);
   begin
      for Wall of Outline loop
         for Box_Wall of Box_Walls loop
            if Intersect (Wall, Box_Wall) then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Rect_In_Bounds;

   function In_Bounds (P : Coord.Pair; B : Box_Bounds) return Boolean is
      --  Do not include end points for vertical span range
      --  For each horizontal:
      --    * Both ends if neighbors are up and down
      --    * Lower end if both are up
      --    * Upper end if both are down
      Hits : Natural := 0;
   begin
      for B_Ind in B.First_Index .. B.Last_Index loop
         declare
            Next_Ind : constant Positive :=
              (if B_Ind = B.Last_Index then
               1 else B_Ind + 1);
            Prev_Ind : constant Positive :=
              (if B_Ind = B.First_Index then
               B.Last_Index else B_Ind - 1);
            Current : constant Green_Wall := B (B_Ind);
            Next : constant Green_Wall := B (Next_Ind);
            Prev : constant Green_Wall := B (Prev_Ind);
            Lower : constant Natural :=
              Wall_Span.Get_Lower (Current.Span);
            Upper : constant Natural :=
              Wall_Span.Get_Upper (Current.Span);
         begin
            case Current.Orient is
               when Horizontal =>
                  if P.Second = Current.Pos and then
                    Wall_Span.Membership (P.First, Current.Span)
                  then
                     return True;

                  elsif P.Second = Current.Pos and then P.First > Lower then
                     if Asc (Prev) and then Asc (Next) then
                        Hits := Hits + 1;

                     elsif Desc (Prev) and then Desc (Next) then
                        Hits := Hits + Boolean'Pos (P.First > Upper);

                     else
                        Hits := Hits + 1 + Boolean'Pos (P.First > Upper);
                     end if;
                  end if;

               when Vertical =>
                  if P.First = Current.Pos and then
                    Wall_Span.Membership (P.Second, Current.Span)
                  then
                     return True;

                  elsif P.First > Current.Pos and then
                    P.Second > Lower and then P.Second < Upper
                  then
                     Hits := Hits + 1;
                  end if;
            end case;
         end;
      end loop;
      return Hits mod 2 = 1;
   end In_Bounds;

   procedure Part_One (Input : String) is
      C : constant Coords := Parse_Inputs (Input);
      Max_Area : Unsigned_64 := 0;
   begin
      for I in C'First .. C'Last - 1 loop
         for J in I + 1 .. C'Last loop
            Max_Area := Unsigned_64'Max (Corner_Area (C (I), C (J)), Max_Area);
         end loop;
      end loop;
      Text_IO.Put_Line ("Max area: " & Max_Area'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
      C : constant Coords := Parse_Inputs (Input);
      B : constant Box_Bounds := Calc_Bounds (C);
      O : constant Box_Bounds := Calc_Outline (B);
      Max_Area : Unsigned_64 := 0;
   begin
      for I in C'First .. C'Last - 1 loop
         for J in I + 1 .. C'Last loop
            if Rect_In_Bounds (C (I), C (J), O) then
               Max_Area := Unsigned_64'Max (
                 Corner_Area (C (I), C (J)), Max_Area);
            end if;
         end loop;
      end loop;
      Text_IO.Put_Line ("Max area: " & Max_Area'Image);
   end Part_Two;
end Aoc.Day_9;
