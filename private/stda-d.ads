package Stda.D is

   type Points_Range is record
      From     : UTF8.Code_Point;
      To       : UTF8.Code_Point;
      Category : UTF8.General_Category;
   end record;
   type Range_Index is range 1 .. 2077;
   type Range_Array is array (Range_Index) of Points_Range;

   procedure Initialize (M : in out Stda.D.Range_Array);

end Stda.D;
