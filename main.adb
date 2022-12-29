with Ada.Command_Line;
with Ac21;
with Ac22;

procedure Main is
   function Argument_Count return Natural is
   begin
      return Ada.Command_Line.Argument_Count;
   end Argument_Count;

   function Argument (Number : Positive) return String is
   begin
      return Ada.Command_Line.Argument (Number);
   end Argument;
begin
   if Argument_Count > 0 then
      if Argument (1) = "2021" then
         Ac21.Solve_Puzzle;
      elsif Argument (1) = "2022" then
         Ac22.Solve_Puzzle;
      end if;
   end if;
end Main;
