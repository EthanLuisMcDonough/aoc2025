package Aoc.Day_10 is
   subtype Light_Index is Positive;
   subtype Button_Index is Positive;
   subtype Joltage is Natural range 0 .. 255;

   type Light_Mask is array (Light_Index range <>) of Boolean;
   type Joltages is array (Light_Index range <>) of Joltage;

   type Button_Mat is array
     (Button_Index range <>,
      Light_Index range <>) of Boolean;

   function Image (L : Light_Mask) return String;

   type Machine
     (Light_Count : Light_Index;
      Button_Count : Button_Index) is
   record
      Lights : Light_Mask (1 .. Light_Count);
      Buttons : Button_Mat (1 .. Button_Count, 1 .. Light_Count);
      Joltage_Reqs : Joltages (1 .. Light_Count);
   end record;

   type Int_Mat is array (Positive range <>, Positive range <>) of Integer;
   type Int_Mat_Row is array (Positive range <>) of Integer;
   type Int_Mat_Col is array (Positive range <>) of Integer;

   function GCD (A : Integer; B : Integer) return Integer;

   procedure Swap_Rows
     (M : in out Int_Mat;
      A : Positive;
      B : Positive);
   procedure Add_Rows
     (M : in out Int_Mat;
      Target : Positive;
      Source : Positive);
   procedure Mul_Row
     (M : in out Int_Mat;
      Row : Positive;
      Scale : Integer);
   procedure Add_Mul_Rows
     (M : in out Int_Mat;
      Target : Positive;
      Source : Positive;
      Scale_Target : Integer;
      Scale_Source : Integer);
   procedure Normalize_Row
     (M : in out Int_Mat;
      Row : Positive);

   function Get_Row (M : Int_Mat; R : Positive) return Int_Mat_Row;
   function Get_Col (M : Int_Mat; C : Positive) return Int_Mat_Col;
   function Sum_Rows (M : Int_Mat) return Int_Mat_Row;

   function Image (M : Int_Mat) return String;
   function Image (R : Int_Mat_Row) return String;
   function Image (C : Int_Mat_Col) return String;

   type Col_Is_Pivot is array (Positive range <>) of Boolean;
   type RREF_Data (Lower : Positive; Upper : Positive) is record
      Pivots : Natural;
      Free_Vars : Natural;
      Is_Pivot : Col_Is_Pivot (Lower .. Upper);
      Zero_Rows : Natural;
      Has_Solutions : Boolean;
   end record;

   function RREF (M : in out Int_Mat) return RREF_Data;

   type Matrix_Mapping is array (Positive range <>) of Positive;
   type Paramaterized
     (Rows : Positive;
      Cols : Positive;
      Vars : Positive) is
   record
      M : Int_Mat (1 .. Rows, 1 .. Cols);
      Var_Mapping : Matrix_Mapping (1 .. Vars);
      Pivot_Vals : Int_Mat_Col (1 .. Rows);
   end record;

   --  Parameterize RREF result with free variables
   function Paramaterize
     (Data : RREF_Data; M : Int_Mat)
      return Paramaterized;

   function Parse_Machine (Line : String) return Machine;

   function Presses (M : Machine) return Int_Mat;
   function Min_Presses (M : Machine) return Natural;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_10;
