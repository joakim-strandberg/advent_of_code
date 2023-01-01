with Ada.Finalization;

with Std;
pragma Elaborate_All (Std);

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Exceptions;
with Ac21.Aa;
with Ac21.Ab;

pragma Elaborate_All (Std);

package body Ac21 is

   use Std.Types;

   package Acl renames Ada.Command_Line;

   package Nat32_IO renames Ac21.Aa.Nat32_IO;
   package Positive_IO is new Ada.Text_IO.Integer_IO (Positive);

   procedure Raise_Exception
     (E : Ada.Exceptions.Exception_Id; Message : String := "") renames
     Ada.Exceptions.Raise_Exception;

   procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1) renames
     Ada.Text_IO.New_Line;

   procedure Put (Item : String) renames Ada.Text_IO.Put;

   procedure Put_Repeated (Item   : String;
                           Repeat : Pos32) is
   begin
      for I in Pos32 range 1 .. Repeat loop
         Put (Item);
      end loop;
   end Put_Repeated;

   procedure Put      (Number : Nat32) renames Ac21.Aa.Put;
   procedure Put_Line (Number : Nat32) renames Ac21.Aa.Put_Line;

   procedure Put (Number : Positive) is
   begin
      Positive_IO.Put (Item  => Number,
                       Width => 0);
   end Put;

   procedure Put_Line (Item : String) renames Ada.Text_IO.Put_Line;

   package body Day_1 is

      package body Part_One is

         procedure Run is
         begin
            Ac21.Aa.Day_1_Part_One.Run;
         end Run;

      end Part_One;

      package body Part_Two is

         procedure Run is
         begin
            Ac21.Aa.Day_1_Part_Two.Run;
         end Run;

      end Part_Two;

   end Day_1;

   package body Day_2 is

      package body Part_One is

         procedure Run renames Ac21.Aa.Day_2_Part_One.Run;

      end Part_One;

      package body Part_Two is

         procedure Run renames Ac21.Aa.Day_2_Part_Two.Run;

      end Part_Two;

   end Day_2;

   package body Day_3 is

      package body Part_One is

         procedure Run renames Ac21.Aa.Day_3_Part_One.Run;

      end Part_One;

      package body Part_Two is

         procedure Run renames Ac21.Aa.Day_3_Part_Two.Run;

      end Part_Two;

   end Day_3;

   package body Day_4 is

      package body Part_One is

         procedure Run renames Ac21.Aa.Day_4_Part_One.Run;

      end Part_One;

      package body Part_Two is

         procedure Run renames Ac21.Ab.Day_4_Part_Two.Run;

      end Part_Two;

   end Day_4;

   package body Day_5 is

      package body Part_One is

         procedure Run renames Ac21.Ab.Day_5_Part_One.Run;

      end Part_One;

      package body Part_Two is

         procedure Run renames Ac21.Ab.Day_5_Part_Two.Run;

      end Part_Two;

   end Day_5;

   type Day_Type is range 1 .. 24;

   procedure Run_Part_One (Day : Day_Type) is
   begin
      case Day is
         when  1 => Day_1.Part_One.Run;
         when  2 => Day_2.Part_One.Run;
         when  3 => Day_3.Part_One.Run;
         when  4 => Day_4.Part_One.Run;
         when  5 => Day_5.Part_One.Run;
            --when  6 => Day_6.Part_One.Run;
            --when  7 => Day_7.Part_One.Run;
            --when  8 => Day_8.Part_One.Run;
            --when  9 => Day_9.Part_One.Run;
            --when 10 => Day_10.Part_One.Run;
         when 6 .. 24 =>
            Put_Line ("No puzzle implemented.");
      end case;
   end Run_Part_One;

   procedure Run_Part_Two (Day : Day_Type) is
   begin
      case Day is
         when  1 => Day_1.Part_Two.Run;
         when  2 => Day_2.Part_Two.Run;
         when  3 => Day_3.Part_Two.Run;
         when  4 => Day_4.Part_Two.Run;
         when  5 => Day_5.Part_Two.Run;
            --when  6 => Day_6.Part_Two.Run;
            --when  7 => Day_7.Part_Two.Run;
            --when  8 => Day_8.Part_Two.Run;
            --when  9 => Day_9.Part_Two.Run;
            --when 10 => Day_10.Part_Two.Run;
         when 6 .. 24 =>
            Put_Line ("No puzzle implemented.");
      end case;
   end Run_Part_Two;

   procedure Print_Help is
   begin
      Put_Line ("Welcome to the Advent of Code application (aoc)!");
      Put_Line ("It requires at least three arguments.");
      New_Line;
      Put_Line ("1. The year (possible value is only 2021)");
      Put_Line ("2. The day (valid values are 1 to 24)");
      Put_Line ("3. Challenge (valid values are 1 or 2)");
      New_Line;
      Put_Line ("For example, to run the application for challenge");
      Put_Line ("two on the third of December 2021 execute:");
      Put_Line ("aoc 2021 3 2");
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
      if Arg1 = "2021" then
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

end Ac21;
