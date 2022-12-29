with Stda.A;

private package Stda.B is

   procedure Initialize (M : in out Stda.A.Categorization_Array);
   --  Does partial initialization of the input argument M.
   --  The initialization of M is spread out over several procedure calls.
   --  Hence, the input argument is of "in out" type instead of "out"
   --  in order to not erase previous initialization.

end Stda.B;
