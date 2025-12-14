with Interfaces;
with Ada.Text_IO;
with GNAT;
with Aoc.Common;
with GNAT.String_Split;

use Interfaces;

package body Aoc.Day_11 is
   function Parse_Input (Input : String) return Devices is
      use GNAT.String_Split;
      use Str_Key;
      Lines : constant Slice_Set := Common.Split_Lines (Input);
      Devices : constant Positive := Positive (Slice_Count (Lines)) + 1;
      Connections : Device_Mat (1 .. Devices, 1 .. Devices) :=
        (others => (others => False));
      M : Map;
   begin
      M.Reserve_Capacity (Ada.Containers.Count_Type (Devices));
      for I in 1 .. Slice_Count (Lines) loop
         declare
            Line : constant String := Slice (Lines, I);
            Label : constant Device_Str :=
              Line (Line'First .. Line'First + 2);
         begin
            M.Insert (Label, Positive (I));
         end;
      end loop;
      M.Insert ("out", Devices);

      for I in 1 .. Slice_Count (Lines) loop
         declare
            Line : constant String := Slice (Lines, I);
            Raw_Children : constant String :=
              Line (Line'First + 5 .. Line'Last);
            Children : constant Slice_Set := Create (
              Raw_Children, " ", Multiple);
         begin
            for J in 1 .. Slice_Count (Children) loop
               declare
                  Name : constant Device_Str := Slice (Children, J);
                  Connection_Id : constant Positive := M (Name);
               begin
                  Connections (Positive (I), Connection_Id) := True;
               end;
            end loop;
         end;
      end loop;
      return (Count => Devices,
        Device_Names => M,
        Connections => Connections);
   end Parse_Input;

   function Count_Paths (D : Devices; Start : Positive) return Unsigned_64 is
      function Count_Internal (Label : Positive)
        return Unsigned_64;

      Saved : array (1 .. D.Count) of Unsigned_64 := (others => 0);

      function Count_Internal (Label : Positive) return Unsigned_64 is
         Paths : Unsigned_64 := 0;
      begin
         if not Predicate (Label) then
            return 0;
         end if;

         if Saved (Label) > 0 then
            return Saved (Label);
         end if;

         if D.Connections (Label, D.Count) then
            Paths := Paths + 1;
         end if;

         for I in 1 .. D.Count - 1 loop
            if D.Connections (Label, I) then
               Paths := Paths + Count_Internal (I);
            end if;
         end loop;

         Saved (Label) := Paths;
         return Paths;
      end Count_Internal;
   begin
      return Count_Internal (Start);
   end Count_Paths;

   function "and" (L, R : Marking) return Marking is
   begin
      case L is
         when None =>
            return R;
         when Both =>
            return L;
         when FFT =>
            case R is
               when DAC | Both =>
                  return Both;
               when None | FFT =>
                  return FFT;
            end case;
         when DAC =>
            case R is
               when FFT | Both =>
                  return Both;
               when None | DAC =>
                  return DAC;
            end case;
      end case;
   end "and";

   procedure Part_One (Input : String) is
      D : constant Devices := Parse_Input (Input);

      function Accept_All (Ind : Positive) return Boolean;
      function Accept_All (Ind : Positive) return Boolean is
         pragma Unreferenced (Ind);
      begin
         return True;
      end Accept_All;

      function Count_All is new Count_Paths
        (Predicate => Accept_All);

      Start : constant Positive := D.Device_Names ("you");
      Total_Paths : constant Unsigned_64 := Count_All (D, Start);
   begin
      Ada.Text_IO.Put_Line ("Total paths: " & Total_Paths'Image);
   end Part_One;

   procedure Part_Two (Input : String) is
      D : constant Devices := Parse_Input (Input);
      Markings : array (1 .. D.Count) of Marking := (others => None);

      procedure Mark_Devices (Device : Positive; Mark : Marking) is
         procedure Mark_Downward (Id : Positive) is
         begin
            if Id /= Device and then
              Markings (Id) in Mark | Both
            then
               return;
            end if;

            for I in 1 .. D.Count loop
               if D.Connections (Id, I) then
                  Mark_Downward (I);
               end if;
            end loop;

            Markings (Id) := Markings (Id) and Mark;
         end Mark_Downward;

         procedure Mark_Upward (Id : Positive) is
         begin
            if Id /= Device and then
              Markings (Id) in Mark | Both
            then
               return;
            end if;

            for I in 1 .. D.Count loop
               if D.Connections (I, Id) then
                  Mark_Upward (I);
               end if;
            end loop;

            Markings (Id) := Markings (Id) and Mark;
         end Mark_Upward;
      begin
         Mark_Downward (Device);
         Mark_Upward (Device);
      end Mark_Devices;

      FFT_Id : constant Positive := D.Device_Names ("fft");
      DAC_Id : constant Positive := D.Device_Names ("dac");
      Start : constant Positive := D.Device_Names ("svr");

      function Visited_Both (Label : Positive) return Boolean
        is (Markings (Label) = Both);
      function Count_Visited_Both is new Count_Paths
        (Predicate => Visited_Both);

      Total_Paths : constant Unsigned_64 := Count_Visited_Both (D, Start);
   begin
      Mark_Devices (FFT_Id, FFT);
      Mark_Devices (DAC_Id, DAC);
      Ada.Text_IO.Put_Line (Total_Paths'Image);
   end Part_Two;
end Aoc.Day_11;
