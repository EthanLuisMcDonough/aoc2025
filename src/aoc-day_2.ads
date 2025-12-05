with Interfaces;

package Aoc.Day_2 is
   function Check_Repeat (Id : Interfaces.Unsigned_64)
     return Boolean;

   function Check_Repeat_Exhaustive
     (Id : Interfaces.Unsigned_64)
      return Boolean;

   generic
      with function Check_Err
        (Id : Interfaces.Unsigned_64)
         return Boolean;
   function Sum_Errs (Input : String)
     return Interfaces.Unsigned_64;

   procedure Part_One (Input : String);
   procedure Part_Two (Input : String);
end Aoc.Day_2;
