private package Stda.A is

   type Categorization is record
      Code  : UTF8.Code_Point;
      Upper : UTF8.Code_Point;
      Lower : UTF8.Code_Point;
   end record;
   type Categorization_Index is range 1 .. 3070;
   type Categorization_Array is array (Categorization_Index) of Categorization;

   procedure Initialize (M : in out Categorization_Array);
   --  Does partial initialization of the input argument M.
   --  The initialization of M is spread out over several procedure calls.
   --  Hence, the input argument is of "in out" type instead of "out"
   --  in order to not erase previous initialization.

   function To_String (This : Ada_Code_Location) return String;

end Stda.A;
