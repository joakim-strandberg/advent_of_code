with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Interfaces;

package body Stdx is

   subtype size_t is Interfaces.C.size_t;

   use type size_t;
   use type Nat32;
   use type Stda.Types.Ada_Code_Location;
   use type Stda.Types.Subprogram_Call_Result;

   protected body Test_Statistics is

      procedure Reset is
      begin
         My_Failed_Test_Count := 0;
         My_Passed_Test_Count := 0;
         My_Total_Test_Count  := 0;
      end Reset;

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

      function Total_Test_Count return String is
      begin
         return Nat32'Image (My_Total_Test_Count);
      end Total_Test_Count;

      function Passed_Test_Count return Nat32 is
      begin
         return My_Passed_Test_Count;
      end Passed_Test_Count;

      function Passed_Test_Count return String is
      begin
         return Nat32'Image (My_Passed_Test_Count);
      end Passed_Test_Count;

      function Failed_Test_Count return Nat32 is
      begin
         return My_Failed_Test_Count;
      end Failed_Test_Count;

      function Failed_Test_Count return String is
      begin
         return Nat32'Image (My_Failed_Test_Count);
      end Failed_Test_Count;

   end Test_Statistics;

   package body Test_Defs2 is

      procedure Assert (Statement : Boolean;
                        Location  : Stda.Types.Ada_Code_Location) is
      begin
         if not Statement then
            Ada.Exceptions.Raise_Exception (Assertion_Violated'Identity,
                                            "Location: "       & (+Location));
         end if;
      end Assert;

      procedure Validate_Call_Result
        (Call_Result : Subprogram_Call_Result;
         Location    : Stda.Types.Ada_Code_Location) is
      begin
         if Call_Result.Has_Failed then
            --  Put_Line (Call_Result, Location);
            Ada.Exceptions.Raise_Exception
              (Assertion_Violated'Identity,
               "Location: "       & (+Location) &
                 "; Error Code: " & (+Call_Result));
         end if;
      end Validate_Call_Result;

      procedure Initialize (Suite : in out Test_Suite) is
      begin
         Suite.Allocated_Memory := new Unit_Test_Vs.Elements_Array;
         Unit_Test_Vs.Init (V    => Suite.Tests,
                            Item => Suite.Allocated_Memory);
      end Initialize;

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Unit_Test_Vs.Elements_Array,
         Name   => Unit_Test_Vs.Elements_Ptr);

      procedure Finalize (Suite : in out Test_Suite) is
      begin
         Free (Suite.Allocated_Memory);
      end Finalize;

      procedure Run_Tests (Suite : Test_Suite) is
      begin
         declare
            First : constant Unit_Test_Vs.Index := Unit_Test_Vs.Index'First;
            Last  : constant Unit_Test_Vs.Extended_Index
              := Unit_Test_Vs.Last_Index (Suite.Tests);
            Test : Unit_Test_Any_Ptr;
         begin
            for I in Unit_Test_Vs.Index range First .. Last loop
               Test := Unit_Test_Vs.Element (Suite.Tests, I);
               Test_Statistics.Increase_Total_Test_Count;
               begin
                  Ada.Text_IO.Put (Name (Test.all) & ": ");
                  Run (Test.all);
                  Test_Statistics.Increase_Passed_Test_Count;
                  Ada.Text_IO.Put_Line ("OK");
               exception
                  when Error : others =>
                     Ada.Text_IO.Put_Line ("FAILED");
                     Ada.Text_IO.Put_Line
                       (Ada.Exceptions.Exception_Message (Error));
                     Test_Statistics.Increase_Failed_Test_Count;
               end;
            end loop;
         end;
      end Run_Tests;

      procedure Add (Suite : in out Test_Suite;
                     Test  : Unit_Test_Any_Ptr) is
      begin
         Unit_Test_Vs.Append
           (Container => Suite.Tests,
            New_Item  => Test);
      end Add;

   end Test_Defs2;

   package body Text_Files is

      package Text_IO renames Ada.Text_IO;

      function Count_Lines_In_File (File_Name : String) return Nat32 is
         Input_File : Text_IO.File_Type;

         Result : Nat32 := 0;

         Is_Infinite_Loop_Detected : Boolean := True;
      begin
         Text_IO.Open (File => Input_File,
                       Mode => Text_IO.In_File,
                       Name => File_Name);
         begin
            for I in Nat32 range 1 .. Stda.Infinite_Loop_Max loop
               if Text_IO.End_Of_File (Input_File) then
                  Is_Infinite_Loop_Detected := False;
                  exit;
               end if;
               declare
                  Line : constant String := Text_IO.Get_Line (Input_File);
               begin
                  Result := Result + 1;
               end;
            end loop;

            if Is_Infinite_Loop_Detected then
               raise Constraint_Error with "infinite loop detected";
            end if;

            Text_IO.Close (Input_File);
         exception
            when others =>
               Text_IO.Close (Input_File);
               raise;
         end;

         return Result;
      end Count_Lines_In_File;

      procedure Read_Line_By_Line (Parser    : in out Text_Parser;
                                             File_Name : String)
      is
         Input_File : Text_IO.File_Type;

         Is_Infinite_Loop_Detected : Boolean := True;
      begin
         Text_IO.Open (File => Input_File,
                       Mode => Text_IO.In_File,
                       Name => File_Name);
         begin
            for I in Nat32 range 1 .. Stda.Infinite_Loop_Max loop
               if Text_IO.End_Of_File (Input_File) then
                  Is_Infinite_Loop_Detected := False;
                  exit;
               end if;
               Handle_Line (Parser, Text_IO.Get_Line (Input_File));
            end loop;

            if Is_Infinite_Loop_Detected then
               raise Constraint_Error with "infinite loop detected";
            end if;

            Text_IO.Close (Input_File);
         exception
            when others =>
               Text_IO.Close (Input_File);
               raise;
         end;
      end Read_Line_By_Line;

   end Text_Files;

--     procedure Open (File        : in out Input_File;
--                     Name        : in     String;
--                     Call_Result : in out Subprogram_Call_Result)
--     is
--     begin
--        File.Open (File, Name, Call_Result);
--     end Open;
--
--     function End_Of_File (File : Input_File) return Boolean is
--     begin
--        return File.End_Of_File (File);
--     end End_Of_File;
--
--
--     procedure Read (File : in out Input_File) is
--     begin
--        File.Read (File);
--     end Read;
--
--     function Read_Data (File : Input_File) return Byte_Array is
--     begin
--        return File.Read_Data (File);
--     end Read_Data;
--
--     procedure Close (File        : in out Input_File;
--                      Call_Result : in out Subprogram_Call_Result) is
--     begin
--        File.Close (File, Call_Result);
--     end Close;
--
--     procedure Create (File        : in out Output_File;
--                       Name        : in     String;
--                       Call_Result : in out Subprogram_Call_Result) is
--     begin
--        File.Create (File, Name, Call_Result);
--     end Create;
--
--     procedure Write (File        : in out Output_File;
--                      Buffer      : in     Byte_Array;
--                      Call_Result : in out Subprogram_Call_Result) is
--     begin
--        File.Write (File, Buffer, Call_Result);
--     end Write;
--
--     function Bytes_Written_Count (File : Output_File) return Nat32 is
--     begin
--        return File.My.Count;
--     end Bytes_Written_Count;
--
--     procedure New_Line (File        : in out Output_File;
--                         Call_Result : in out Subprogram_Call_Result) is
--     begin
--        File.New_Line (File, Call_Result);
--     end New_Line;
--
--     procedure Close (File        : in out Output_File;
--                      Call_Result : in out Subprogram_Call_Result) is
--     begin
--        File.Close (File, Call_Result);
--     end Close;
--

   procedure Put_Line (Io   : in out Std_Io_Target;
                       Text : String) is
   begin
      Ada.Text_IO.Put_Line (Text);
   end Put_Line;

--     procedure Put_Line_Impl (Terminal : Io_Target;
--                              Text     : String) is
--     begin
--        Ada.Text_IO.Put_Line (Text);
--     end Put_Line_Impl;
--
--     function Make_Std_Out return Io_Target is
--        Result : constant Io_Target := (Kind     => Std_Out_Terminal,
--                                        Put_Line => Put_Line_Impl'Access);
--     begin
--        return Result;
--     end Make_Std_Out;
--

   package body Text_IO is

      My_Std_Io_Target : aliased Std_Io_Target;

      function Std_Io return Io_Target_Ptr is
      begin
         return My_Std_Io_Target'Access;
      end Std_Io;

   end Text_IO;

--     procedure Put_Line_Test (Terminal : Io_Target;
--                              Text     : String) is
--     begin
--        Terminal.Term.Owner.Append (Text);
--     end Put_Line_Test;
--

   package body Text_IO_Tests is

      function "+" (Text : String)
                 return Text_IO_Tests.Printed_Strings.Bounded_String is
         Result : Text_IO_Tests.Printed_Strings.Bounded_String
           := Text_IO_Tests.Printed_Strings.Empty;
      begin
         Text_IO_Tests.Printed_Strings.Append (This => Result,
                                               Text => Text);
         return Result;
      end "+";

      protected body Std_Out_Owner is

         procedure Append (Text : String) is
         begin
            Text_IO_Tests.Line_Vs.Append (My_Lines, +Text);
         end Append;

         function Lines return Text_IO_Tests.Line_Vs.Vector is
         begin
            return My_Lines;
         end Lines;

      end Std_Out_Owner;

      procedure Put_Line (Terminal : in out Test_Io_Target;
                          Text     : String) is
      begin
         Terminal.Owner.Append (Text);
      end Put_Line;


--        My_Proxy : aliased Std_Out_Proxy;

      My_Terminal : aliased Test_Io_Target;

      function Test_Io return Text_IO.Io_Target_Ptr is
      begin
         return My_Terminal'Access;
      end Test_Io;

      function Lines return Text_IO_Tests.Line_Vs.Vector is
      begin
         return My_Terminal.Owner.Lines;
      end Lines;

   end Text_IO_Tests;
--
--     function Create (Target : Io_Target) return Input_File is
--     begin
--        case Target.Kind is
--           when Std_Out_Terminal => raise Constraint_Error;
--           when Test_Terminal    => raise Constraint_Error;
--           when Win32_Target     => return Create_Win32_Input_File (Target);
--        end case;
--     end Create;
--
--     function Create (Target : Io_Target) return Output_File is
--     begin
--        case Target.Kind is
--           when Std_Out_Terminal => raise Constraint_Error;
--           when Test_Terminal    => raise Constraint_Error;
--           when Win32_Target     => return Create_Win32_Output_File (Target);
--        end case;
--     end Create;

end Stdx;
