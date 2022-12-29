with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

package body Stdb is

   use Stda.Types;

   package String_Split renames Stda.String_Split;

   function To_String (This : Ada_Code_Location) return String renames
     Stda.Conversions.To_String;

   use type Ada.Text_IO.Count;

   package Nat32_IO    is new Ada.Text_IO.Integer_IO (Nat32);
   package Positive_IO is new Ada.Text_IO.Integer_IO (Positive);

   package Test_Suite is

      function Total_Test_Count  return Nat32;
      function Passed_Test_Count return Nat32;
      function Failed_Test_Count return Nat32;

      Assertion_Violated : exception;

      procedure Assert (Statement : Boolean;
                        Location  : Ada_Code_Location);

      procedure Assert_Equal (Left     : String;
                              Right    : String;
                              Location : Ada_Code_Location);

      generic
         type Element_Type is private;
         with procedure Put (Element : Element_Type);
      procedure Generic_Assert_Equal (Left     : Element_Type;
                                      Right    : Element_Type;
                                      Location : Ada_Code_Location);

      procedure Find_In_Standard_Output
        (Searched_For : String;
         Location     : Ada_Code_Location);

      type Unit_Test is abstract tagged null record;
      --  All unit tests inherit from this tagged type.

      procedure Put_Name (Test : Unit_Test) is abstract;
      --  Prints a human readable name of a test to standard out.

      procedure Run (Test : Unit_Test) is abstract;
      --  During the execution of this subprogram all of the text messages
      --  send to standard out has been redirected to a text file.

      procedure Verify (Test : Unit_Test) is abstract;
      --  Here the expected output and results from running the test
      --  is verified.

      procedure Run_Test (Test : Unit_Test'Class);

   private

      Shall_Run_Test : Boolean := False;

      My_Total_Test_Count  : Nat32;
      My_Passed_Test_Count : Nat32;
      My_Failed_Test_Count : Nat32;

      procedure Increase_Total_Test_Count;
      procedure Increase_Passed_Test_Count;
      procedure Increase_Failed_Test_Count;

   end Test_Suite;

   package Conversions renames Stda.Conversions;

   procedure Put (Item : String) renames Ada.Text_IO.Put;

   use type Ada.Text_IO.Count;

   procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1) renames
     Ada.Text_IO.New_Line;

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

   procedure Put_Line (Item : String) renames Ada.Text_IO.Put_Line;

   procedure Put_Repeated (Item   : String;
                           Repeat : Pos32) is
   begin
      for I in Pos32 range 1 .. Repeat loop
         Put (Item);
      end loop;
   end Put_Repeated;

   package body Test_Suite is

      Standard_Output_File_Name : constant String := "output.txt";

      procedure Increase_Total_Test_Count is
      begin
         My_Total_Test_Count := My_Total_Test_Count + 1;
      end Increase_Total_Test_Count;

      procedure Increase_Passed_Test_Count is
      begin
         My_Passed_Test_Count := My_Passed_Test_Count + 1;
      end Increase_Passed_Test_Count;

      procedure Increase_Failed_Test_Count is
      begin
         My_Failed_Test_Count := My_Failed_Test_Count + 1;
      end Increase_Failed_Test_Count;

      function Total_Test_Count return Nat32 is
      begin
         return My_Total_Test_Count;
      end Total_Test_Count;

      --           function Total_Test_Count return String is
      --           begin
      --              return Nat32'Image (My_Total_Test_Count);
      --           end Total_Test_Count;

      function Passed_Test_Count return Nat32 is
      begin
         return My_Passed_Test_Count;
      end Passed_Test_Count;

      --           function Passed_Test_Count return String is
      --           begin
      --              return Nat32'Image (My_Passed_Test_Count);
      --           end Passed_Test_Count;

      function Failed_Test_Count return Nat32 is
      begin
         return My_Failed_Test_Count;
      end Failed_Test_Count;

      --           function Failed_Test_Count return String is
      --           begin
      --              return Nat32'Image (My_Failed_Test_Count);
      --           end Failed_Test_Count;

      procedure Assert (Statement : Boolean;
                        Location  : Ada_Code_Location) is
      begin
         if not Statement then
            Put ("FAILED");
            New_Line;
            Put ("Assertion failed, location: (");
            Put (Conversions.To_String (Location));
            Put (")");
            New_Line;
            Ada.Exceptions.Raise_Exception (Assertion_Violated'Identity);
         end if;
      end Assert;

      procedure Assert_Equal (Left     : String;
                              Right    : String;
                              Location : Ada_Code_Location) is
      begin
         if Left /= Right then
            Put ("FAILED");
            New_Line;
            Put ("Assertion failed, location: (");
            Put (Conversions.To_String (Location));
            Put (")");
            New_Line;
            Put ("Expected equal, but was '");
            Put (Left);
            Put ("' and '");
            Put (Right);
            Put ("'");
            New_Line;
            Ada.Exceptions.Raise_Exception (Assertion_Violated'Identity);
         end if;
      end Assert_Equal;

      procedure Generic_Assert_Equal (Left     : Element_Type;
                                      Right    : Element_Type;
                                      Location : Ada_Code_Location) is
      begin
         if Left /= Right then
            Put ("FAILED");
            New_Line;
            Put ("Assertion failed, location: (");
            Put (Conversions.To_String (Location));
            Put (")");
            New_Line;
            Put ("Expected equal, but was:");
            New_Line;
            Put (Left);
            New_Line;
            Put ("and:");
            New_Line;
            Put (Right);
            New_Line;
            Ada.Exceptions.Raise_Exception (Assertion_Violated'Identity);
         end if;
      end Generic_Assert_Equal;

      subtype Full_String is String (1 .. Positive (Pos16'Last));

      procedure Find_In_Standard_Output
        (Searched_For : String;
         Location     : Ada_Code_Location)
      is
         Is_Infinite_Loop_Detected : Boolean := True;
         File : Ada.Text_IO.File_Type;
         Read_Line : Full_String;
         Last : Natural;
      begin
         Ada.Text_IO.Open (File => File,
                           Mode => Ada.Text_IO.In_File,
                           Name => Standard_Output_File_Name);
         begin
            for I in Nat32 range 1 .. Infinite_Loop_Max loop
               if Ada.Text_IO.End_Of_File (File) then
                  Ada.Exceptions.Raise_Exception (Constraint_Error'Identity,
                                                  "end of file reached");
               end if;
               Ada.Text_IO.Get_Line (File => File,
                                     Item => Read_Line,
                                     Last => Last);
               if Read_Line (1 .. Last) = Searched_For then
                  Is_Infinite_Loop_Detected := False;
                  exit;
               end if;
            end loop;

            if Is_Infinite_Loop_Detected then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity,
                                               "infinite loop detected");
            end if;
            Ada.Text_IO.Close (File);
         exception
            when Error : others =>
               Ada.Text_IO.Close (File);
               raise;
         end;
      end Find_In_Standard_Output;

      procedure TRun (Test : Unit_Test'Class) is
         File : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Create (File => File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => Standard_Output_File_Name);
         begin
            --  From here on, everything written to standard out is
            --  redirected to the file.
            Ada.Text_IO.Set_Output (File);
            Run (Test);
            Ada.Text_IO.Flush;
            --  Turn off redirection of standard out.
            Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
            Ada.Text_IO.Close (File);
         exception
            when Error : others =>
               Ada.Text_IO.Flush;
               Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
               Ada.Text_IO.Close (File);
               raise;
         end;
         Verify (Test);
      end TRun;

      procedure Run_Test (Test : Unit_Test'Class) is
         Line : Full_String;
         Last : Natural;
         File : Ada.Text_IO.File_Type;
         Is_Infinite_Loop_Detected : Boolean;
      begin
         if not Shall_Run_Test then
            return;
         end if;
         Increase_Total_Test_Count;
         begin
            Put_Name (Test);
            Ada.Text_IO.Put (": ");
            TRun (Test);
            Increase_Passed_Test_Count;
            if Ada.Text_IO.Col < 78 then
               Ada.Text_IO.Set_Col (78);
            end if;
            Put_Line ("OK");
         exception
            when Error : others =>
               Put_Line ("TEST FAILED");
               Put_Line (Ada.Exceptions.Exception_Message (Error));
               Increase_Failed_Test_Count;
               Put_Line ("--  Start Output  --");
               Ada.Text_IO.Open (File => File,
                                 Mode => Ada.Text_IO.In_File,
                                 Name => Standard_Output_File_Name);
               begin
                  Is_Infinite_Loop_Detected := True;
                  for J in Nat32 range 1 .. Infinite_Loop_Max loop
                     if Ada.Text_IO.End_Of_File (File) then
                        Is_Infinite_Loop_Detected := False;
                        exit;
                     end if;
                     Ada.Text_IO.Get_Line (File => File,
                                           Item => Line,
                                           Last => Last);
                     Put_Line (Line (1 .. Last));
                  end loop;

                  if Is_Infinite_Loop_Detected then
                     Ada.Exceptions.Raise_Exception
                       (Constraint_Error'Identity,
                        "infinite loop detected");
                  end if;
                  Ada.Text_IO.Close (File);
               exception
                  when others =>
                     Ada.Text_IO.Close (File);
                     raise;
               end;
               Put_Line ("--    End Output  --");
         end;
      end Run_Test;

   begin
      if
        Ada.Command_Line.Argument_Count = 1 and then
        Ada.Command_Line.Argument (1) = "tests"
      then
         Shall_Run_Test := True;

         Put_Line ("Unit tests of the Stda and Stdb packages");
         Put_Repeated (Item   => "-",
                       Repeat => 80);
         New_Line;
      end if;
   end Test_Suite;

   procedure Assert_Equal
     (Left     : String_Split.Variable_Length_Interval_Array;
      Right    : String_Split.Variable_Length_Interval_Array;
      Location : Ada_Code_Location)
   is
      use type String_Split.Variable_Length_Interval_Array;
      procedure Put (Text : String_Split.Variable_Length_Interval_Array)
                     renames String_Split.Put;
   begin
      if Left /= Right then
         Put ("FAILED");
         New_Line;
         Put ("Assertion failed, location: (");
         Put (Conversions.To_String (Location));
         Put (")");
         New_Line;
         Put ("Expected equal, but was:");
         New_Line;
         Put (Left);
         New_Line;
         Put ("and:");
         New_Line;
         Put (Right);
         New_Line;
         Ada.Exceptions.Raise_Exception
           (Test_Suite.Assertion_Violated'Identity);
      end if;
   end Assert_Equal;

   procedure Put (Number : Positive) is
   begin
      Positive_IO.Put (Item  => Number,
                       Width => 0);
   end Put;

   package Split_String_Tests is

      package Split_String_Test_1 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Split_String_Test_1;


      package Split_String_Test_2 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Split_String_Test_2;

      package Split_String_Test_3 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Split_String_Test_3;

      package Split_String_Test_4 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Split_String_Test_4;

      package Split_String_Test_5 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Split_String_Test_5;

      package Split_String_Test_6 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Split_String_Test_6;

      package Split_String_Test_7 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Split_String_Test_7;

      package Split_String_Test_8 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Split_String_Test_8;

      package Split_String_Test_9 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Split_String_Test_9;


      package Split_String_Test_10 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Split_String_Test_10;


      package Split_String_Test_11 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Split_String_Test_11;


      package Split_String_Test_12 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Split_String_Test_12;


   end Split_String_Tests;

   package body Split_String_Tests is

      package body Split_String_Test_1 is

         Input : constant String (1 .. 3) := "A B";

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Split string test '");
            Put (Input);
            Put ("', separator ' '");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Result : String_Split.Variable_Length_Interval_Array :=
              String_Split.Get_Slices (Line   => Input,
                                       Sep    => ' ');
         begin
            Assert_Equal (Left  => Result,
                          Right => (Last     => 2,
                                    Interval => (1 => (First => 1,
                                                       Last  => 1),
                                                 2 => (First => 3,
                                                       Last  => 3))),
                          Location  => (-0077271503, 1237402416));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Split_String_Test_1;

      package body Split_String_Test_2 is

         Input : constant String (1 .. 4) := " A B";

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Split string test '");
            Put (Input);
            Put ("', separator ' '");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Result : String_Split.Variable_Length_Interval_Array :=
              String_Split.Get_Slices (Line   => Input,
                                       Sep    => ' ');
         begin
            Assert_Equal (Left => Result,
                          Right => (Last     => 2,
                                    Interval => (1 => (First => 2,
                                                       Last  => 2),
                                                 2 => (First => 4,
                                                       Last  => 4))),
                          Location  => (-0903219679, 1384386973));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Split_String_Test_2;

      package body Split_String_Test_3 is

         Input : constant String (1 .. 4) := "A B ";

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Split string test '");
            Put (Input);
            Put ("', separator ' '");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Result : String_Split.Variable_Length_Interval_Array :=
              String_Split.Get_Slices (Line   => Input,
                                       Sep    => ' ');
         begin
            Assert_Equal (Left => Result,
                          Right => (Last     => 2,
                                    Interval => (1 => (First => 1,
                                                       Last  => 1),
                                                 2 => (First => 3,
                                                       Last  => 3))),
                          Location  => (1545655896, -1694372685));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Split_String_Test_3;

      package body Split_String_Test_4 is

         Input : constant String (1 .. 4) := "A  B";

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Split string test '");
            Put (Input);
            Put ("', separator ' '");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Result : String_Split.Variable_Length_Interval_Array :=
              String_Split.Get_Slices (Line   => Input,
                                       Sep    => ' ');
         begin
            Assert_Equal (Left => Result,
                          Right => (Last     => 2,
                                    Interval => (1 => (First => 1,
                                                       Last  => 1),
                                                 2 => (First => 4,
                                                       Last  => 4))),
                          Location  => (-0801636336, 0504291141));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Split_String_Test_4;

      package body Split_String_Test_5 is

         Input : constant String (1 .. 4) := "AA B";

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Split string test '");
            Put (Input);
            Put ("', separator ' '");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Result : String_Split.Variable_Length_Interval_Array :=
              String_Split.Get_Slices (Line   => Input,
                                       Sep    => ' ');
         begin
            Assert_Equal (Left  => Result,
                          Right => (Last     => 2,
                                    Interval => (1 => (First => 1,
                                                       Last  => 2),
                                                 2 => (First => 4,
                                                       Last  => 4))),
                          Location  => (1024995234, 0347603526));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Split_String_Test_5;
      package body Split_String_Test_6 is

         Input : constant String (1 .. 5) := " AA B";

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Split string test '");
            Put (Input);
            Put ("', separator ' '");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Result : String_Split.Variable_Length_Interval_Array :=
              String_Split.Get_Slices (Line   => Input,
                                       Sep    => ' ');
         begin
            Assert_Equal (Left => Result,
                          Right => (Last     => 2,
                                    Interval => (1 => (First => 2,
                                                       Last  => 3),
                                                 2 => (First => 5,
                                                       Last  => 5))),
                          Location  => (1573200655, -0027340231));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Split_String_Test_6;

      package body Split_String_Test_7 is

         Input : constant String (1 .. 5) := "AA B ";

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Split string test '");
            Put (Input);
            Put ("', separator ' '");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Result : String_Split.Variable_Length_Interval_Array :=
              String_Split.Get_Slices (Line   => Input,
                                       Sep    => ' ');
         begin
            Assert_Equal (Left => Result,
                          Right => (Last     => 2,
                                    Interval => (1 => (First => 1,
                                                       Last  => 2),
                                                 2 => (First => 4,
                                                       Last  => 4))),
                          Location  => (-2017270159, 2016253767));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Split_String_Test_7;
      package body Split_String_Test_8 is

         Input : constant String (1 .. 5) := "AA  B";

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Split string test '");
            Put (Input);
            Put ("', separator ' '");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Result : String_Split.Variable_Length_Interval_Array :=
              String_Split.Get_Slices (Line   => Input,
                                       Sep    => ' ');
         begin
            Assert_Equal (Left => Result,
                          Right => (Last     => 2,
                                    Interval => (1 => (First => 1,
                                                       Last  => 2),
                                                 2 => (First => 5,
                                                       Last  => 5))),
                          Location  => (0199168442, -2012530170));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Split_String_Test_8;

      package body Split_String_Test_9 is

         Input : constant String (1 .. 4) := "A BB";

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Split string test '");
            Put (Input);
            Put ("', separator ' '");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Result : String_Split.Variable_Length_Interval_Array :=
              String_Split.Get_Slices (Line   => Input,
                                       Sep    => ' ');
         begin
            Assert_Equal (Left  => Result,
                          Right => (Last     => 2,
                                    Interval => (1 => (First => 1,
                                                       Last  => 1),
                                                 2 => (First => 3,
                                                       Last  => 4))),
                          Location  => (1353715627, 0622249930));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Split_String_Test_9;

      package body Split_String_Test_10 is

         Input : constant String (1 .. 5) := " A BB";

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Split string test '");
            Put (Input);
            Put ("', separator ' '");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Result : String_Split.Variable_Length_Interval_Array :=
              String_Split.Get_Slices (Line   => Input,
                                       Sep    => ' ');
         begin
            Assert_Equal (Left => Result,
                          Right => (Last     => 2,
                                    Interval => (1 => (First => 2,
                                                       Last  => 2),
                                                 2 => (First => 4,
                                                       Last  => 5))),
                          Location  => (-0067856382, -2143945385));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Split_String_Test_10;

      package body Split_String_Test_11 is

         Input : constant String (1 .. 5) := "A BB ";

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Split string test '");
            Put (Input);
            Put ("', separator ' '");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Result : String_Split.Variable_Length_Interval_Array :=
              String_Split.Get_Slices (Line   => Input,
                                       Sep    => ' ');
         begin
            Assert_Equal (Left => Result,
                          Right => (Last     => 2,
                                    Interval => (1 => (First => 1,
                                                       Last  => 1),
                                                 2 => (First => 3,
                                                       Last  => 4))),
                          Location  => (-0045513239, 2076274195));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Split_String_Test_11;
      package body Split_String_Test_12 is

         Input : constant String (1 .. 5) := "A  BB";

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Split string test '");
            Put (Input);
            Put ("', separator ' '");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Result : String_Split.Variable_Length_Interval_Array :=
              String_Split.Get_Slices (Line   => Input,
                                       Sep    => ' ');
         begin
            Assert_Equal (Left => Result,
                          Right => (Last     => 2,
                                    Interval => (1 => (First => 1,
                                                       Last  => 1),
                                                 2 => (First => 4,
                                                       Last  => 5))),
                          Location  => (-0376161859, 0780563437));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Split_String_Test_12;

   end Split_String_Tests;

   package Code_Location_Tests is

      package Ada_Code_Location_Test_1 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Ada_Code_Location_Test_1;


      package Ada_Code_Location_Test_2 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Ada_Code_Location_Test_2;


      package Ada_Code_Location_Test_3 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Ada_Code_Location_Test_3;


      package Ada_Code_Location_Test_4 is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Ada_Code_Location_Test_4;

   end Code_Location_Tests;

   package body Code_Location_Tests is

      package body Ada_Code_Location_Test_1 is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Ada code location test (1625143615, 0529906411))");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Location : Ada_Code_Location := (1625143615, 0529906411);
         begin
            Test_Suite.Assert_Equal (Left     => To_String (Location),
                                     Right    => "1625143615, 0529906411",
                                     Location => (0315022744, -0978935607));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Ada_Code_Location_Test_1;

      package body Ada_Code_Location_Test_2 is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Ada code location test (0000000001, -0000000005))");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Location : Ada_Code_Location := (0000000001, -0000000005);
         begin
            Test_Suite.Assert_Equal (Left     => To_String (Location),
                                     Right    => "0000000001, -0000000005",
                                     Location => (-1850903624, 0370454473));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Ada_Code_Location_Test_2;

      package body Ada_Code_Location_Test_3 is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Ada code location test (-0000000013, 0000000487))");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Location : Ada_Code_Location := (-0000000013, 0000000487);
         begin
            Test_Suite.Assert_Equal (Left     => To_String (Location),
                                     Right    => "-0000000013, 0000000487",
                                     Location => (-0887531396, -2044025757));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Ada_Code_Location_Test_3;

      package body Ada_Code_Location_Test_4 is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Ada code location test (-2022574034, -0087287826))");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            null;
         end Run;

         procedure Verify (Test : Unit_Test) is
            Location : Ada_Code_Location := (-2022574034, -0087287826);
         begin
            Test_Suite.Assert_Equal (Left     => To_String (Location),
                                     Right    => "-2022574034, -0087287826",
                                     Location => (0081169522, 0725774117));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Ada_Code_Location_Test_4;

   end Code_Location_Tests;

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
end Stdb;
