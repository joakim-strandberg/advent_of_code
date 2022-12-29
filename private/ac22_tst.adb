with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

package body Ac22_Tst is

   package Conversions renames Std.Conversions;

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

         Put_Line ("Unit tests of the puzzles for Advent of Code 2022");
         Put_Repeated (Item   => "-",
                       Repeat => 80);
         New_Line;

      end if;
   end Test_Suite;

end Ac22_Tst;
