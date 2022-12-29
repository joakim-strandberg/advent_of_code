with Ada.Command_Line;
with Ada.Text_IO;

with Ac22.Aa;

package body Ac22 is

   package Acl renames Ada.Command_Line;

   package body Day_1 is

      package body Part_One is

         procedure Run is
         begin
            Ac22.Aa.Day_1_Part_One.Run;
         end Run;

      end Part_One;

      package body Part_Two is

         procedure Run is
         begin
            Ac22.Aa.Day_1_Part_Two.Run;
         end Run;

      end Part_Two;

   end Day_1;

   package body Day_2 is

      package body Part_One is

         procedure Run is
         begin
            Ac22.Aa.Day_2_Part_One.Run;
         end Run;

      end Part_One;

      package body Part_Two is

         procedure Run is
         begin
            Ac22.Aa.Day_2_Part_Two.Run;
         end Run;

      end Part_Two;

   end Day_2;

   package body Day_3 is

      package body Part_One is

         procedure Run is
         begin
            Ac22.Aa.Day_3_Part_One.Run;
         end Run;

      end Part_One;

      package body Part_Two is

         procedure Run is
         begin
            Ac22.Aa.Day_3_Part_Two.Run;
         end Run;

      end Part_Two;

   end Day_3;

   procedure Put (Item : String) renames Ada.Text_IO.Put;

   procedure Put_Line (Item : String) renames Ada.Text_IO.Put_Line;

   procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1) renames
     Ada.Text_IO.New_Line;

   type Day_Type is range 1 .. 24;

   procedure Run_Part_One (Day : Day_Type) is
   begin
      case Day is
         when  1 => Day_1.Part_One.Run;
         when  2 => Day_2.Part_One.Run;
         when  3 => Day_3.Part_One.Run;
            --when  4 => Day_4.Part_One.Run;
            --when  5 => Day_5.Part_One.Run;
            --when  6 => Day_6.Part_One.Run;
            --when  7 => Day_7.Part_One.Run;
            --when  8 => Day_8.Part_One.Run;
            --when  9 => Day_9.Part_One.Run;
            --when 10 => Day_10.Part_One.Run;
         when 4 .. 24 =>
            Put_Line ("No puzzle implemented.");
      end case;
   end Run_Part_One;

   procedure Run_Part_Two (Day : Day_Type) is
   begin
      case Day is
         when  1 => Day_1.Part_Two.Run;
         when  2 => Day_2.Part_Two.Run;
         when  3 => Day_3.Part_Two.Run;
            --when  4 => Day_4.Part_Two.Run;
            --when  5 => Day_5.Part_Two.Run;
            --when  6 => Day_6.Part_Two.Run;
            --when  7 => Day_7.Part_Two.Run;
            --when  8 => Day_8.Part_Two.Run;
            --when  9 => Day_9.Part_Two.Run;
            --when 10 => Day_10.Part_Two.Run;
         when 4 .. 24 =>
            Put_Line ("No puzzle implemented.");
      end case;
   end Run_Part_Two;

   procedure Print_Help is
   begin
      Put_Line ("Welcome to the Advent of Code application 2022!");
      Put_Line ("It requires at least three arguments.");
      New_Line;
      Put_Line ("1. The year (possible value is only 2021)");
      Put_Line ("2. The day (valid values are 1 to 24)");
      Put_Line ("3. Challenge (valid values are 1 or 2)");
      New_Line;
      Put_Line ("For example, to run the application for challenge");
      Put_Line ("two on the third of December 2022 execute:");
      Put_Line ("aoc 2022 3 2");
      New_Line;
      Put_Line ("To run the unit-tests:");
      Put_Line ("aoc tests");
   end Print_Help;

   procedure Solve_Specific_Puzzle is
      Day : Day_Type;
      Arg1 : constant String := Acl.Argument (1);
      Arg2 : constant String := Acl.Argument (2);
      Arg3 : constant String := Acl.Argument (3);
   begin
      if Arg1 = "2022" then
         begin
            Day := Day_Type'Value (Arg2);
         exception
            when others =>
               Put_Line ("The day had invalid value '" & Arg2 & "'");
               Print_Help;
               return;
         end;
         if Arg3 = "1" then
            Run_Part_One (Day);
         elsif Arg3 = "2" then
            Run_Part_Two (Day);
         else
            Put_Line ("The challenge has invalid value " & Arg3);
         end if;
      else
         Put_Line ("The year had invalid value " & Arg1);
         Print_Help;
         return;
      end if;
   end Solve_Specific_Puzzle;

   procedure Solve_Puzzle is
   begin
      if
        Acl.Argument_Count = 1 and then
        Acl.Argument (1) = "all"
      then
         for Day in Day_Type range 1 .. 5 loop
            Run_Part_One (Day);
            Run_Part_Two (Day);
         end loop;
      else
         if Acl.Argument_Count < 3 then
            Print_Help;
         else
            Solve_Specific_Puzzle;
         end if;
      end if;
   end Solve_Puzzle;

end Ac22;
