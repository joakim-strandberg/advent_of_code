with Ada.Text_IO;
with Std;
with Ac22_Tst;
with Ada.Command_Line;
with Ada.Exceptions;

package body Ac22.Aa is

   use Std.Types;

   package Test_Suite renames Ac22_Tst.Test_Suite;

   package Nat32_IO is new Ada.Text_IO.Integer_IO (Nat32);

   subtype Positive_Count is Ada.Text_IO.Positive_Count;
   use type Positive_Count;

   package Positive_Count_IO is new Ada.Text_IO.Integer_IO (Positive_Count);

   procedure New_Line (Spacing : Positive_Count := 1)
                       renames Ada.Text_IO.New_Line;

   procedure Put      (Text : String) renames Ada.Text_IO.Put;
   procedure Put_Line (Text : String) renames Ada.Text_IO.Put_Line;

   procedure Put (Number : Nat32) is
   begin
      Nat32_IO.Put (Item  => Number,
                    Width => 0);
   end Put;

   procedure Put_Line (Number : Nat32) is
   begin
      Nat32_IO.Put (Item  => Number,
                    Width => 0);
      New_Line;
   end Put_Line;

   ----------------------------------------------------------------------------
   --
   --              Deepend - Dynamic Pools for Ada 2005 and Ada 2012
   --
   --           B A S I C   B O U N D E D   D Y N A M I C   P O O L S
   --
   --                                B o d y
   --
   --                  Copyright (C) 2011, Bradley J. Moore
   --
   --  Deepend is free software;  you can  redistribute it  and/or modify it
   --  under  terms of the  GNU General Public License  as  published  by the
   --  Free Software  Foundation;  either version 2,  or (at your option) any
   --  later  version.  Paraffin is  distributed in the hope that it  will be
   --  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
   --  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU
   --  General Public License for  more details.  You should have  received a
   --  copy of the GNU General Public License distributed with Deepend;  see
   --  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,
   --  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.
   --
   --  As a  special exception, if other files  instantiate generics from
   --  this unit,  or you link this  unit with other files  to produce an
   --  executable,  this unit  does  not by  itself  cause the  resulting
   --  executable to be covered by  the GNU General Public License.  This
   --  exception does  not however invalidate  any other reasons  why the
   --  executable file might be covered by the GNU Public License.
   ----------------------------------------------------------------------------
   package body Basic_Bounded_Dynamic_Pools is

      procedure Allocate
        (Pool : in out Basic_Dynamic_Pool;
         Storage_Address : out Address;
         Size_In_Storage_Elements : Storage_Elements.Storage_Count;
         Alignment : Storage_Elements.Storage_Count)
      is
      begin
         if Size_In_Storage_Elements >
           Active'Length - Pool.Next_Allocation
         then
            raise Storage_Error;
         end if;

         Storage_Address := Active (Pool.Next_Allocation)'Address;

         Pool.Next_Allocation :=
           Pool.Next_Allocation + Size_In_Storage_Elements;

      end Allocate;

      --------------------------------------------------------------

      procedure Deallocate
        (Pool : in out Basic_Dynamic_Pool;
         Storage_Address : Address;
         Size_In_Storage_Elements : Storage_Elements.Storage_Count;
         Alignment : Storage_Elements.Storage_Count) is
      begin
         null;
      end Deallocate;

      --------------------------------------------------------------

      procedure Finalize   (Pool : in out Basic_Dynamic_Pool) is
      begin
         Owned := False;
      end Finalize;

      --------------------------------------------------------------

      procedure Initialize (Pool : in out Basic_Dynamic_Pool) is
      begin
         Pool.Next_Allocation := 1;
         if Owned then
            Ada.Exceptions.Raise_Exception
              (E       => Constraint_Error'Identity,
               Message => "");
         end if;
         Owned := True;
      end Initialize;

      --------------------------------------------------------------

      function Storage_Size
        (Pool : Basic_Dynamic_Pool)
         return Storage_Elements.Storage_Count is
      begin
         return Size;
      end Storage_Size;

      --------------------------------------------------------------

      function Storage_Used
        (Pool : Basic_Dynamic_Pool)
         return Storage_Elements.Storage_Count is
      begin
         return Pool.Next_Allocation - 1;
      end Storage_Used;

   end Basic_Bounded_Dynamic_Pools;

   package body Day_1_Part_One is

      type Calorie_Array is array (Pos32 range <>) of Nat32;

      procedure Run (File_Name : String) is
         Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool;

         type Calorie_Array_Ptr is access Calorie_Array;
         for Calorie_Array_Ptr'Storage_Pool use Pool;

         Controlled_File : Std.File_IO.Text_File;
         File : Ada.Text_IO.File_Type renames Controlled_File.File;
         N : Nat32;
         Current_Line  : Positive_Count;
         Previous_Line : Positive_Count;
         Number_Of_Elves : Nat32 := 1;
         Elf_Index : Pos32;
         Elf_Carrying_The_Most : Pos32;
         Calories : Calorie_Array_Ptr;
      begin
         --  Calculate number of elves:
         Ada.Text_IO.Open (File => File,
                           Mode => Ada.Text_IO.In_File,
                           Name => File_Name);
         Previous_Line := Ada.Text_IO.Line (File);
         Nat32_IO.Get (File  => File,
                       Item  => N);
         while not Ada.Text_IO.End_Of_File (File) loop
            Nat32_IO.Get (File  => File,
                          Item  => N);
            Current_Line := Ada.Text_IO.Line (File);
            if Previous_Line + 1 /= Current_Line then
               Number_Of_Elves := Number_Of_Elves + 1;
            end if;
            Previous_Line := Ada.Text_IO.Line (File);
            --              Put (N);
            --              New_Line;
            --              Put ("Line ");
            --              Positive_Count_IO.Put (Text_IO.Line (File));
            --              New_Line;
         end loop;
         Ada.Text_IO.Close (File);
         Put ("Number of Elves ");
         Put (Number_Of_Elves);
         New_Line;

         Calories := new Calorie_Array (1 .. Number_Of_Elves);
         for I in Calories'Range loop
            Calories (I) := 0;
         end loop;

         --  Populate calorie array:
         Elf_Index := 1;
         Ada.Text_IO.Open (File => File,
                           Mode => Ada.Text_IO.In_File,
                           Name => File_Name);
         Previous_Line := Ada.Text_IO.Line (File);
         Nat32_IO.Get (File  => File,
                       Item  => N);
         Calories (Elf_Index) := N;
         Put ("Read value ");
         Put (N);
         New_Line;
         while not Ada.Text_IO.End_Of_File (File) loop
            Nat32_IO.Get (File  => File,
                          Item  => N);
            Current_Line := Ada.Text_IO.Line (File);
            if Previous_Line + 1 /= Current_Line then
               Put ("Elf ");
               Put (Elf_Index);
               Put (" carries ");
               Put (Calories (Elf_Index));
               Elf_Index := Elf_Index + 1;
            end if;
            Put ("Read value ");
            Put (N);
            New_Line;
            Calories (Elf_Index) := Calories (Elf_Index) + N;
            Previous_Line := Ada.Text_IO.Line (File);
         end loop;
         Ada.Text_IO.Close (File);

         Put_Line ("Calories vector:");
         Elf_Carrying_The_Most := 1;
         for I in Calories'Range loop
            if Calories (I) > Calories (Elf_Carrying_The_Most) then
               Elf_Carrying_The_Most := I;
            end if;
            --  Put ("Index ");
            --  Put (I);
            --  Put (" ");
            --  Put (Calories (I));
            --  New_Line;
         end loop;
         New_Line;
         Put ("Answer: ");
         Put (Calories (Elf_Carrying_The_Most));
         New_Line;
      end Run;

      procedure Run is
      begin
         Run ("2022_01r.txt");
      end Run;

      package Result_Should_Be_24_000_Test is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Result_Should_Be_24_000_Test;

      package body Result_Should_Be_24_000_Test is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Day 01, part 1. Result should be 24000");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            Run ("2022_01t.txt");
         end Run;

         procedure Verify (Test : Unit_Test) is
         begin
            Test_Suite.Find_In_Standard_Output
              (Searched_For => "Answer: 24000",
               Location     => (0801813638, 1090323787));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Result_Should_Be_24_000_Test;

      package Result_Should_Be_71_924_Test is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Result_Should_Be_71_924_Test;

      package body Result_Should_Be_71_924_Test is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Day 01, part 1. Result should be 71924");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            Run ("2022_01r.txt");
         end Run;

         procedure Verify (Test : Unit_Test) is
         begin
            Test_Suite.Find_In_Standard_Output
              (Searched_For => "Answer: 71924",
               Location     => (-1090558316, -0660055777));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Result_Should_Be_71_924_Test;

   end Day_1_Part_One;

   package body Day_1_Part_Two is

      type Calorie_Array is array (Pos32 range <>) of Nat32;

      procedure Run (File_Name : String) is
         Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool;

         type Binary_Node;
         type Binary_Node_Ptr is access Binary_Node;
         for Binary_Node_Ptr'Storage_Pool use Pool;

         type Binary_Node is record
            Value   : Nat32;
            Handled : Boolean := False;
            Parent  : Binary_Node_Ptr;
            Left    : Binary_Node_Ptr;
            Right   : Binary_Node_Ptr;
         end record;

         Root_Node : Binary_Node_Ptr;
         Current_Node : Binary_Node_Ptr;

         type Calorie_Array_Ptr is access Calorie_Array;
         for Calorie_Array_Ptr'Storage_Pool use Pool;

         Controlled_File : Std.File_IO.Text_File;
         File : Ada.Text_IO.File_Type renames Controlled_File.File;
         N : Nat32;
         Current_Line  : Positive_Count;
         Previous_Line : Positive_Count;
         Number_Of_Elves : Nat32 := 1;
         Elf_Index : Pos32;
         Calories : Calorie_Array_Ptr;
         Sum : Nat32;
         L : Pos32;
         Shall_Continue : Boolean;
      begin
         --  Calculate number of elves:
         Ada.Text_IO.Open (File => File,
                           Mode => Ada.Text_IO.In_File,
                           Name => File_Name);
         Previous_Line := Ada.Text_IO.Line (File);
         Nat32_IO.Get (File  => File,
                       Item  => N);
         while not Ada.Text_IO.End_Of_File (File) loop
            Nat32_IO.Get (File  => File,
                          Item  => N);
            Current_Line := Ada.Text_IO.Line (File);
            if Previous_Line + 1 /= Current_Line then
               Number_Of_Elves := Number_Of_Elves + 1;
            end if;
            Previous_Line := Ada.Text_IO.Line (File);
            --              Put (N);
            --              New_Line;
            --              Put ("Line ");
            --              Positive_Count_IO.Put (Text_IO.Line (File));
            --              New_Line;
         end loop;
         Ada.Text_IO.Close (File);
         Put ("Number of Elves ");
         Put (Number_Of_Elves);
         New_Line;

         Calories := new Calorie_Array (1 .. Number_Of_Elves);
         for I in Calories'Range loop
            Calories (I) := 0;
         end loop;

         --  Populate calorie array:
         Elf_Index := 1;
         Ada.Text_IO.Open (File => File,
                           Mode => Ada.Text_IO.In_File,
                           Name => File_Name);
         Previous_Line := Ada.Text_IO.Line (File);
         Nat32_IO.Get (File  => File,
                       Item  => N);
         Calories (Elf_Index) := N;
         Put ("Read value ");
         Put (N);
         New_Line;
         while not Ada.Text_IO.End_Of_File (File) loop
            Nat32_IO.Get (File  => File,
                          Item  => N);
            Current_Line := Ada.Text_IO.Line (File);
            if Previous_Line + 1 /= Current_Line then
               Put ("Elf ");
               Put (Elf_Index);
               Put (" carries ");
               Put (Calories (Elf_Index));
               Elf_Index := Elf_Index + 1;
            end if;
            Put ("Read value ");
            Put (N);
            New_Line;
            Calories (Elf_Index) := Calories (Elf_Index) + N;
            Previous_Line := Ada.Text_IO.Line (File);
         end loop;
         Ada.Text_IO.Close (File);

         --  Put_Line ("Calories vector:");
         for I in Calories'Range loop
            if Root_Node = null then
               Root_Node := new Binary_Node;
               Root_Node.Value := Calories (I);
            else
               Shall_Continue := True;
               Current_Node := Root_Node;
               while Shall_Continue loop
                  if Calories (I) <= Current_Node.Value then
                     if Current_Node.Left = null then
                        Current_Node.Left := new Binary_Node;
                        Current_Node.Left.Value := Calories (I);
                        Current_Node.Left.Parent := Current_Node;
                        Shall_Continue := False;
                     else
                        Current_Node := Current_Node.Left;
                     end if;
                  else
                     if Current_Node.Right = null then
                        Current_Node.Right := new Binary_Node;
                        Current_Node.Right.Value := Calories (I);
                        Current_Node.Right.Parent := Current_Node;
                        Shall_Continue := False;
                     else
                        Current_Node := Current_Node.Right;
                     end if;
                  end if;
               end loop;
            end if;
            --  if Calories (I) > Calories (Elf_Carrying_The_Most) then
            --     Elf_Carrying_The_Most := I;
            --  end if;
            --  Put ("Index ");
            --  Put (I);
            --  Put (" ");
            --  Put (Calories (I));
            --  New_Line;
         end loop;

         Elf_Index := 1;
         Current_Node := Root_Node;
         while Current_Node /= null loop
            if Current_Node.Left /= null then
               if Current_Node.Left.Handled then
                  if Current_Node.Right /= null then
                     if Current_Node.Right.Handled then
                        Current_Node := Current_Node.Parent;
                     else
                        Calories (Elf_Index) := Current_Node.Value;
                        Elf_Index := Elf_Index + 1;
                        Put (Current_Node.Value); New_Line;
                        Current_Node.Handled := True;
                        Current_Node := Current_Node.Right;
                     end if;
                  else
                     Calories (Elf_Index) := Current_Node.Value;
                     Elf_Index := Elf_Index + 1;
                     Put (Current_Node.Value); New_Line;
                     Current_Node.Handled := True;
                     Current_Node := Current_Node.Parent;
                  end if;
               else
                  Current_Node := Current_Node.Left;
               end if;
            elsif Current_Node.Right /= null then
               if Current_Node.Right.Handled then
                  Current_Node := Current_Node.Parent;
               else
                  Calories (Elf_Index) := Current_Node.Value;
                  Elf_Index := Elf_Index + 1;
                  Put (Current_Node.Value); New_Line;
                  Current_Node.Handled := True;
                  Current_Node := Current_Node.Right;
               end if;
            else
               Calories (Elf_Index) := Current_Node.Value;
               Elf_Index := Elf_Index + 1;
               Put (Current_Node.Value); New_Line;
               Current_Node.Handled := True;
               Current_Node := Current_Node.Parent;
            end if;
         end loop;
         L := Calories'Last;
         Sum := Calories (L) + Calories (L - 1) + Calories (L - 2);
         New_Line;
         Put ("Answer: ");
         Put (Sum);
         New_Line;
      end Run;

      procedure Run is
      begin
         Run ("2022_01r.txt");
      end Run;

      package Result_Should_Be_45_000_Test is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Result_Should_Be_45_000_Test;

      package body Result_Should_Be_45_000_Test is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Day 01, part 1. Result should be 45000");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            Run ("2022_01t.txt");
         end Run;

         procedure Verify (Test : Unit_Test) is
         begin
            Test_Suite.Find_In_Standard_Output
              (Searched_For => "Answer: 45000",
               Location     => (0340046716, 0088287561));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Result_Should_Be_45_000_Test;

      package Result_Should_Be_210_406_Test is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Result_Should_Be_210_406_Test;

      package body Result_Should_Be_210_406_Test is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Day 01, part 1. Result should be 210406");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            Run ("2022_01r.txt");
         end Run;

         procedure Verify (Test : Unit_Test) is
         begin
            Test_Suite.Find_In_Standard_Output
              (Searched_For => "Answer: 210406",
               Location     => (-1826961430, 0687537036));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Result_Should_Be_210_406_Test;

   end Day_1_Part_Two;

   package body Day_2_Part_One is

      type Opponent_Choice is (A,  --  Rock
                               B,  --  Paper,
                               C); --  Scissors

      type Own_Choice is (X, --  Rock
                          Y, --  Paper
                          Z);  --  Scissors

      package Opponent_IO is new Ada.Text_IO.Enumeration_IO (Opponent_Choice);
      package Own_IO      is new Ada.Text_IO.Enumeration_IO (Own_Choice);

      procedure Run (File_Name : String) is
         Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool;

         Controlled_File : Std.File_IO.Text_File;
         File : Ada.Text_IO.File_Type renames Controlled_File.File;
         Me : Own_Choice;
         Opponent : Opponent_Choice;
         Space : Character;
         Score : Nat32 := 0;
      begin
         Ada.Text_IO.Open (File => File,
                           Mode => Ada.Text_IO.In_File,
                           Name => File_Name);
         while not Ada.Text_IO.End_Of_File (File) loop
            Opponent_IO.Get (File => File,
                             Item => Opponent);
            Ada.Text_IO.Get (File => File,
                             Item => Space);
            Own_IO.Get (File => File,
                        Item => Me);
            case Opponent is
               when A =>
                  case Me is
                     when X =>
                        Score := Score + 1 + 3;
                     when Y =>
                        Score := Score + 2 + 6;
                     when Z =>
                        Score := Score + 3 + 0;
                  end case;
               when B =>
                  case Me is
                     when X =>
                        Score := Score + 1 + 0;
                     when Y =>
                        Score := Score + 2 + 3;
                     when Z =>
                        Score := Score + 3 + 6;
                  end case;
               when C =>
                  case Me is
                     when X =>
                        Score := Score + 1 + 6;
                     when Y =>
                        Score := Score + 2 + 0;
                     when Z =>
                        Score := Score + 3 + 3;
                  end case;
            end case;
            --Put (Score);
            --New_Line;
         end loop;
         Ada.Text_IO.Close (File);
         Put ("Answer: ");
         Put (Score);
         New_Line;
      end Run;

      procedure Run is
      begin
         Run ("2022_02r.txt");
      end Run;

      package Result_Should_Be_15_Test is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Result_Should_Be_15_Test;

      package body Result_Should_Be_15_Test is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Day 02, part 1. Result should be 15");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            Run ("2022_02t.txt");
         end Run;

         procedure Verify (Test : Unit_Test) is
         begin
            Test_Suite.Find_In_Standard_Output
              (Searched_For => "Answer: 15",
               Location     => (-1746101437, -1423104263));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Result_Should_Be_15_Test;

   end Day_2_Part_One;

   package body Day_2_Part_Two is

      type Opponent_Choice is (Rock, Paper, Scissors);

      type Own_Player_Choice is (Rock, Paper, Scissors);

      type Input_Opponent_Choice is (A,  --  Rock
                                     B,  --  Paper,
                                     C); --  Scissors

      type Input_Desired_Outcome is (X,   --  Lose
                                     Y,   --  Draw
                                     Z);  --  Win

      type Desired_Outcome is (Lose, Draw, Win);

      To_Desired_Outcome : array (Input_Desired_Outcome) of Desired_Outcome :=
        (X => Lose,
         Y => Draw,
         Z => Win);

      To_Opponent_Choice : array (Input_Opponent_Choice) of Opponent_Choice :=
        (A => Rock,
         B => Paper,
         C => Scissors);

      package Opponent_IO is new
        Ada.Text_IO.Enumeration_IO (Input_Opponent_Choice);

      package Own_IO is new
        Ada.Text_IO.Enumeration_IO (Input_Desired_Outcome);

      procedure Run (File_Name : String) is
         Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool;

         Controlled_File : Std.File_IO.Text_File;
         File : Ada.Text_IO.File_Type renames Controlled_File.File;
         Outcome : Input_Desired_Outcome;
         Opponent : Input_Opponent_Choice;
         Space : Character;
         Score : Nat32 := 0;
         Own_Choice : Own_Player_Choice;
      begin
         Ada.Text_IO.Open (File => File,
                           Mode => Ada.Text_IO.In_File,
                           Name => File_Name);
         while not Ada.Text_IO.End_Of_File (File) loop
            Opponent_IO.Get (File => File,
                             Item => Opponent);
            Ada.Text_IO.Get (File => File,
                             Item => Space);
            Own_IO.Get (File => File,
                        Item => Outcome);
            case To_Opponent_Choice (Opponent) is
               when Rock =>
                  case To_Desired_Outcome (Outcome) is
                     when Lose => Own_Choice := Scissors;
                     when Draw => Own_Choice := Rock;
                     when Win  => Own_Choice := Paper;
                  end case;
                  case Own_Choice is
                     when Rock =>
                        Score := Score + 1 + 3;
                     when Paper =>
                        Score := Score + 2 + 6;
                     when Scissors =>
                        Score := Score + 3 + 0;
                  end case;
               when Paper =>
                  case To_Desired_Outcome (Outcome) is
                     when Lose => Own_Choice := Rock;
                     when Draw => Own_Choice := Paper;
                     when Win  => Own_Choice := Scissors;
                  end case;
                  case Own_Choice is
                     when Rock =>
                        Score := Score + 1 + 0;
                     when Paper =>
                        Score := Score + 2 + 3;
                     when Scissors =>
                        Score := Score + 3 + 6;
                  end case;
               when Scissors =>
                  case To_Desired_Outcome (Outcome) is
                     when Lose => Own_Choice := Paper;
                     when Draw => Own_Choice := Scissors;
                     when Win  => Own_Choice := Rock;
                  end case;
                  case Own_Choice is
                     when Rock =>
                        Score := Score + 1 + 6;
                     when Paper =>
                        Score := Score + 2 + 0;
                     when Scissors =>
                        Score := Score + 3 + 3;
                  end case;
            end case;
            --Put (Score);
            --New_Line;
         end loop;
         Ada.Text_IO.Close (File);
         Put ("Answer: ");
         Put (Score);
         New_Line;
      end Run;

      procedure Run is
      begin
         Run ("2022_02r.txt");
      end Run;

      package Result_Should_Be_12_Test is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Result_Should_Be_12_Test;

      package body Result_Should_Be_12_Test is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Day 02, part 1. Result should be 12");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            Run ("2022_02t.txt");
         end Run;

         procedure Verify (Test : Unit_Test) is
         begin
            Test_Suite.Find_In_Standard_Output
              (Searched_For => "Answer: 12",
               Location     => (1058002580, 0168186146));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Result_Should_Be_12_Test;

   end Day_2_Part_Two;

   package body Day_3_Part_One is

      type Line_Length_Array is array (Positive_Count range <>) of Nat32;

      type Enum_Values is
        ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
        'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
        'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z');

      P1 : array (Character range 'a' .. 'z') of Pos32;
      P2 : array (Character range 'A' .. 'Z') of Pos32;

      function Priority (C : Character) return Pos32 is
      begin
         if C in 'a' .. 'z' then
            return P1 (C);
         elsif C in 'A' .. 'Z' then
            return P2 (C);
         else
            Ada.Exceptions.Raise_Exception (Constraint_Error'Identity, "");
         end if;
      end Priority;

      procedure Run (File_Name : String) is
         Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool;

         type Line_Length_Array_Ptr is access Line_Length_Array;
         for Line_Length_Array_Ptr'Storage_Pool use Pool;

         Controlled_File : Std.File_IO.Text_File;
         File : Ada.Text_IO.File_Type renames Controlled_File.File;
         C : Character;
         Score : Nat32 := 0;
         Number_Of_Rucksacks : Positive_Count;
         L : Line_Length_Array_Ptr;
         Number_Of_Items : Nat32 := 0;
         Current_Line : Positive_Count := 1;
         Value : Pos32;
      begin
         Value := 1;
         for I in P1'Range loop
            P1 (I) := Value;
            Value := Value + 1;
         end loop;
         Value := 27;
         for I in P2'Range loop
            P2 (I) := Value;
            Value := Value + 1;
         end loop;

         Ada.Text_IO.Open (File => File,
                           Mode => Ada.Text_IO.In_File,
                           Name => File_Name);
         while not Ada.Text_IO.End_Of_File (File) loop
            Ada.Text_IO.Get (File => File,
                             Item => C);
            Number_Of_Rucksacks := Ada.Text_IO.Line (File);
         end loop;
         Ada.Text_IO.Close (File);
         Put ("Number of rucksacks: ");
         New_Line;
         Positive_Count_IO.Put (Number_Of_Rucksacks);
         L := new Line_Length_Array (1 .. Number_Of_Rucksacks);

         Ada.Text_IO.Open (File => File,
                           Mode => Ada.Text_IO.In_File,
                           Name => File_Name);
         while not Ada.Text_IO.End_Of_File (File) loop
            Ada.Text_IO.Get (File => File,
                             Item => C);
            if Current_Line = Ada.Text_IO.Line (File) then
               Number_Of_Items := Number_Of_Items + 1;
            elsif Current_Line + 1 = Ada.Text_IO.Line (File) then
               Put ("Num of items ");
               Put (Number_Of_Items);
               New_Line;
               if Number_Of_Items mod 2 /= 0 then
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity, "");
               end if;
               L (Current_Line) := Number_Of_Items;
               Current_Line := Ada.Text_IO.Line (File);
               Number_Of_Items := 1;
            else
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity, "");
            end if;
         end loop;
         Put ("Num of items ");
         Put (Number_Of_Items);
         New_Line;
         L (L'Last) := Number_Of_Items;
         Ada.Text_IO.Close (File);

         Ada.Text_IO.Open (File => File,
                           Mode => Ada.Text_IO.In_File,
                           Name => File_Name);

         for Row in L'Range loop
            declare
               Last : constant Positive := Positive (L (Row));
               S : String (1 .. Last);
               S1 : String (1 .. Last / 2);
               S2 : String (1 .. Last / 2);
            begin
               for I in S'Range loop
                  Ada.Text_IO.Get (File => File,
                                   Item => S (I));
               end loop;
               for I in Positive range 1 .. Last / 2 loop
                  S1 (I) := S (I);
               end loop;
               for I in Positive range Last / 2 + 1.. Last loop
                  S2 (I - Last / 2) := S (I);
               end loop;
               for I in S1'Range loop
                  declare
                     Searched_For : Character renames S1 (I);
                     Found : Boolean := False;
                  begin
                     for K in S2'Range loop
                        if S2 (K) = Searched_For then
                           Found := True;
                           exit;
                        end if;
                     end loop;
                     if Found then
                        Put ("Found ");
                        Ada.Text_IO.Put (Searched_For);
                        New_Line;
                        Score := Score + Priority (Searched_For);
                        Put ("Priority ");
                        Put (Priority (Searched_For));
                        New_Line;
                        exit;
                     end if;
                  end;
               end loop;
            end;
         end loop;
         Ada.Text_IO.Close (File);

         Put ("Answer: ");
         Put (Score);
         New_Line;
      end Run;

      procedure Run is
      begin
         Run ("2022_03r.txt");
      end Run;

      package Result_Should_Be_157_Test is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Result_Should_Be_157_Test;

      package body Result_Should_Be_157_Test is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Day 03, part 1. Result should be 157");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            Run ("2022_03t.txt");
         end Run;

         procedure Verify (Test : Unit_Test) is
         begin
            Test_Suite.Find_In_Standard_Output
              (Searched_For => "Answer: 157",
               Location     => (-0640423757, 0556117254));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Result_Should_Be_157_Test;

   end Day_3_Part_One;

   package body Day_3_Part_Two is

      type Line_Length_Array is array (Positive_Count range <>) of Nat32;

      type Enum_Values is
        ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
        'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
        'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z');

      P1 : array (Character range 'a' .. 'z') of Pos32;
      P2 : array (Character range 'A' .. 'Z') of Pos32;

      function Priority (C : Character) return Pos32 is
      begin
         if C in 'a' .. 'z' then
            return P1 (C);
         elsif C in 'A' .. 'Z' then
            return P2 (C);
         else
            Ada.Exceptions.Raise_Exception (Constraint_Error'Identity, "");
         end if;
      end Priority;

      procedure Run (File_Name : String) is
         Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool;

         type Line_Length_Array_Ptr is access Line_Length_Array;
         for Line_Length_Array_Ptr'Storage_Pool use Pool;

         Controlled_File : Std.File_IO.Text_File;
         File : Ada.Text_IO.File_Type renames Controlled_File.File;
         C : Character;
         Score : Nat32 := 0;
         Number_Of_Rucksacks : Positive_Count;
         L : Line_Length_Array_Ptr;
         Number_Of_Items : Nat32 := 0;
         Current_Line : Positive_Count := 1;
         Value : Pos32;
      begin
         Value := 1;
         for I in P1'Range loop
            P1 (I) := Value;
            Value := Value + 1;
         end loop;
         Value := 27;
         for I in P2'Range loop
            P2 (I) := Value;
            Value := Value + 1;
         end loop;

         Ada.Text_IO.Open (File => File,
                           Mode => Ada.Text_IO.In_File,
                           Name => File_Name);
         while not Ada.Text_IO.End_Of_File (File) loop
            Ada.Text_IO.Get (File => File,
                             Item => C);
            Number_Of_Rucksacks := Ada.Text_IO.Line (File);
         end loop;
         Ada.Text_IO.Close (File);
         Put ("Number of rucksacks: ");
         New_Line;
         Positive_Count_IO.Put (Number_Of_Rucksacks);
         L := new Line_Length_Array (1 .. Number_Of_Rucksacks);

         Ada.Text_IO.Open (File => File,
                           Mode => Ada.Text_IO.In_File,
                           Name => File_Name);
         while not Ada.Text_IO.End_Of_File (File) loop
            Ada.Text_IO.Get (File => File,
                             Item => C);
            if Current_Line = Ada.Text_IO.Line (File) then
               Number_Of_Items := Number_Of_Items + 1;
            elsif Current_Line + 1 = Ada.Text_IO.Line (File) then
               Put ("Num of items ");
               Put (Number_Of_Items);
               New_Line;
               if Number_Of_Items mod 2 /= 0 then
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity, "");
               end if;
               L (Current_Line) := Number_Of_Items;
               Current_Line := Ada.Text_IO.Line (File);
               Number_Of_Items := 1;
            else
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity, "");
            end if;
         end loop;
         Put ("Num of items ");
         Put (Number_Of_Items);
         New_Line;
         L (L'Last) := Number_Of_Items;
         Ada.Text_IO.Close (File);

         if L'Length mod 3 /= 0 then
            raise Constraint_Error;
         end if;

         Ada.Text_IO.Open (File => File,
                           Mode => Ada.Text_IO.In_File,
                           Name => File_Name);

         for Group_Number in Positive_Count range 1 .. L'Last / 3 loop
            Put ("Group number ");
            PUt (Pos32 (Group_Number));
            New_Line;
            declare
               Row1 : constant Positive_Count := (Group_Number - 1) * 3 + 1;
               Last1 : constant Positive := Positive (L (Row1));
               S1 : String (1 .. Last1);

               Row2 : constant Positive_Count := (Group_Number - 1) * 3 + 2;
               Last2 : constant Positive := Positive (L (Row2));
               S2 : String (1 .. Last2);

               Row3 : constant Positive_Count := (Group_Number - 1) * 3 + 3;
               Last3 : constant Positive := Positive (L (Row3));
               S3 : String (1 .. Last3);

               Searched_For : Character;
               Found : Boolean;
            begin
               for I in S1'Range loop
                  Ada.Text_IO.Get (File => File,
                                   Item => S1 (I));
               end loop;
               for I in S2'Range loop
                  Ada.Text_IO.Get (File => File,
                                   Item => S2 (I));
               end loop;
               for I in S3'Range loop
                  Ada.Text_IO.Get (File => File,
                                   Item => S3 (I));
               end loop;

               Outer_Loop : for I in S1'Range loop
                  Searched_For := S1 (I);
                  Found := False;

                  for K in S2'Range loop
                     if S2 (K) = Searched_For then
                        Found := True;
                        exit;
                     end if;
                  end loop;
                  if Found then
                     for K in S3'Range loop
                        if S3 (K) = Searched_For then
                           Put ("Found ");
                           Ada.Text_IO.Put (Searched_For);
                           New_Line;
                           Score := Score + Priority (Searched_For);
                           Put ("Priority ");
                           Put (Priority (Searched_For));
                           New_Line;
                           exit Outer_Loop;
                        end if;
                     end loop;
                  end if;
               end loop Outer_Loop;
            end;
         end loop;
         Ada.Text_IO.Close (File);

         Put ("Answer: ");
         Put (Score);
         New_Line;
      end Run;

      procedure Run is
      begin
         Run ("2022_03r.txt");
      end Run;

      package Result_Should_Be_70_Test is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Result_Should_Be_70_Test;

      package body Result_Should_Be_70_Test is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Day 03, part 2. Result should be 70");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            Run ("2022_03t.txt");
         end Run;

         procedure Verify (Test : Unit_Test) is
         begin
            Test_Suite.Find_In_Standard_Output
              (Searched_For => "Answer: 70",
               Location     => (0055204880, 1502922054));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Result_Should_Be_70_Test;

   end Day_3_Part_Two;

begin
   if
     Ada.Command_Line.Argument_Count = 1 and then
     Ada.Command_Line.Argument (1) = "tests"
   then
      Put ("Total number of tests:");
      Put (Test_Suite.Total_Test_Count);
      New_Line;

      Put ("Tests failed:");
      Put (Test_Suite.Failed_Test_Count);
      New_Line;

      Put ("Tests passed:");
      Put (Test_Suite.Passed_Test_Count);
      New_Line;
      New_Line;
   end if;
end Ac22.Aa;
