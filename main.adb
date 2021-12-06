with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;

with Advent_Of_Code_2021;
pragma Elaborate_All (Advent_Of_Code_2021);

procedure Main is

   package Text_IO renames Ada.Text_IO;

   package Command_Line renames Ada.Command_Line;

   procedure Print_Help is
   begin
      Text_IO.Put_Line ("Welcome to the Advent of Code application (aoc)!");
      Text_IO.Put_Line ("It requires at least three arguments.");
      Text_IO.New_Line;
      Text_IO.Put_Line ("1. The year (possible value is only 2021)");
      Text_IO.Put_Line ("2. The day (valid values are 1 to 24)");
      Text_IO.Put_Line ("3. Challenge (valid values are 1 or 2)");
      Text_IO.New_Line;
      Text_IO.Put_Line ("For example, to run the application for challenge");
      Text_IO.Put_Line ("two on the third of December 2021 execute:");
      Text_IO.Put_Line ("aoc 2021 3 2");

   end Print_Help;

   type Day_Type is new Integer range 1 .. 24;

   Day : Day_Type;
begin
   if Command_Line.Argument_Count < 3 then
      Print_Help;
      return;
   end if;

   declare
      Arg1 : constant String := Command_Line.Argument (1);
      Arg2 : constant String := Command_Line.Argument (2);
      Arg3 : constant String := Command_Line.Argument (3);
   begin
      if Arg1 = "2021" then
         begin
            Day := Day_Type'Value (Arg2);
         exception
            when Error : others =>
               Text_IO.Put_Line ("The day had invalid value '" & Arg2 & "'");
               Print_Help;
               return;
         end;
         if Arg3 = "1" then
            case Day is
               when 1 => Advent_Of_Code_2021.Day_1.Part_One.Run;
               when 2 => Advent_Of_Code_2021.Day_2.Part_One.Run;
               when 3 => Advent_Of_Code_2021.Day_3.Part_One.Run;
               when 4 => Advent_Of_Code_2021.Day_4.Part_One.Run;
               when 5 => Advent_Of_Code_2021.Day_5.Part_One.Run;
               when 6 => Advent_Of_Code_2021.Day_6.Part_One.Run;
               when 7 .. 24 =>
                  Text_IO.Put_Line ("No puzzle implemented.");
            end case;
         elsif Arg3 = "2" then
            case Day is
               when 1 => Advent_Of_Code_2021.Day_1.Part_Two.Run;
               when 2 => Advent_Of_Code_2021.Day_2.Part_Two.Run;
               when 3 => Advent_Of_Code_2021.Day_3.Part_Two.Run;
               when 4 => Advent_Of_Code_2021.Day_4.Part_Two.Run;
               when 5 => Advent_Of_Code_2021.Day_5.Part_Two.Run;
               when 6 => Advent_Of_Code_2021.Day_6.Part_Two.Run;
               when 7 .. 24 =>
                  Text_IO.Put_Line ("No puzzle implemented.");
            end case;
         else
            Text_IO.Put_Line ("The challenge has invalid value " & Arg3);
         end if;
      else
         Text_IO.Put_Line ("The year had invalid value " & Arg1);
         Print_Help;
         return;
      end if;
   end;
end Main;
