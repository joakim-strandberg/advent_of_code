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
      Text_IO.Put_Line ("4. Optional value 'test' means run test suite");
      Text_IO.New_Line;
      Text_IO.Put_Line ("For example, to run the application for challenge");
      Text_IO.Put_Line ("two on the third of December 2021 execute:");
      Text_IO.Put_Line ("aoc 2021 3 2");
      Text_IO.New_Line;
      Text_IO.Put_Line ("Or run the associated test suite:");
      Text_IO.Put_Line ("aoc 2021 3 2 test");
   end Print_Help;

   type Application_To_Run is (AoC_2021_Day_1_Part_One,
                               AoC_2021_Day_1_Part_One_Test,
                               AoC_2021_Day_1_Part_Two,
                               AoC_2021_Day_1_Part_Two_Test);

   procedure Run (Application : Application_To_Run) is
   begin
      case Application is
         when AoC_2021_Day_1_Part_One =>
            Advent_Of_Code_2021.Day_1_Part_One.Run;

         when AoC_2021_Day_1_Part_One_Test =>
            Advent_Of_Code_2021.Day_1_Part_One.Run_Test_Suite;

         when AoC_2021_Day_1_Part_Two =>
            Advent_Of_Code_2021.Day_1_Part_Two.Run;

         when AoC_2021_Day_1_Part_Two_Test =>
            Advent_Of_Code_2021.Day_1_Part_Two.Run_Test_Suite;
      end case;
   end Run;

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
         if Day < 2 then
            if Arg3 = "1" then
               if
                 Command_Line.Argument_Count > 3 and then
                 Command_Line.Argument (4) = "test"
               then
                  Run (AoC_2021_Day_1_Part_One_Test);
               else
                  Run (AoC_2021_Day_1_Part_One);
               end if;
            elsif Arg3 = "2" then
               if
                 Command_Line.Argument_Count > 3 and then
                 Command_Line.Argument (4) = "test"
               then
                  Run (AoC_2021_Day_1_Part_Two_Test);
               else
                  Run (AoC_2021_Day_1_Part_Two);
               end if;
            else
               Text_IO.Put_Line ("The challenge has invalid value " & Arg3);
            end if;
         else
            Text_IO.Put_Line ("The day had invalid value " & Arg2);
            Text_IO.Put_Line ("Valid values are 1 to 1");
            Print_Help;
         end if;

      else
         Text_IO.Put_Line ("The year had invalid value " & Arg1);
         Print_Help;
         return;
      end if;
   end;
end Main;
