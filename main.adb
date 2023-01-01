with Ada.Text_IO;
with Ada.Command_Line;
with Ac21;
with Ac22;

procedure Main is
   procedure Put_Line (Item : String) renames Ada.Text_IO.Put_Line;

   procedure Print_Help is
   begin
      Put_Line ("On Linux or Mac OS X:");
      Put_Line ("Try 'aoc tests', 'aoc 2021' or 'aoc 2022'");
      Put_Line ("On Windows or FreeDOS:");
      Put_Line ("Try 'aoc.exe tests', 'aoc.exe 2021' or 'aoc.exe 2022'");
   end Print_Help;

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
   else
      Print_Help;
   end if;
end Main;
