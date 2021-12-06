with Ada.Finalization;
with Interfaces.C;
with System;

with Stda;
pragma Elaborate_All (Stda);

--  The file IO subprograms provided by the standard Ada library are built
--  upon exceptions. This API returns error codes instead of
--  raising exceptions.
package Stdx is

   subtype Nat32 is Stda.Types.Nat32;

   subtype Byte_Array is Stda.Types.Byte_Array;

   subtype Subprogram_Call_Result is Stda.Types.Subprogram_Call_Result;

   subtype Limited_Controlled is Ada.Finalization.Limited_Controlled;

   protected Test_Statistics is

      procedure Reset;

      procedure Increase_Total_Test_Count;
      procedure Increase_Passed_Test_Count;
      procedure Increase_Failed_Test_Count;

      function Total_Test_Count return Nat32;
      function Total_Test_Count return String;

      function Passed_Test_Count return Nat32;
      function Passed_Test_Count return String;

      function Failed_Test_Count return Nat32;
      function Failed_Test_Count return String;

   private
      My_Total_Test_Count  : Nat32;
      My_Passed_Test_Count : Nat32;
      My_Failed_Test_Count : Nat32;
   end Test_Statistics;

   package Test_Defs2 is

      Assertion_Violated : exception;

      procedure Assert (Statement : Boolean;
                        Location  : Stda.Types.Ada_Code_Location);

      procedure Validate_Call_Result
        (Call_Result : Subprogram_Call_Result;
         Location    : Stda.Types.Ada_Code_Location);

      type Test_Suite is new Limited_Controlled with private;

      procedure Initialize (Suite : in out Test_Suite);
      procedure Finalize   (Suite : in out Test_Suite);

      procedure Run_Tests (Suite : Test_Suite);

      type Unit_Test is abstract tagged null record;

      function Name (Test : Unit_Test) return String is abstract;

      procedure Run (Test : in out Unit_Test) is abstract;

      type Unit_Test_Any_Ptr is access all Unit_Test'Class;

      procedure Add (Suite : in out Test_Suite;
                     Test  : Unit_Test_Any_Ptr);

   private

      type Unit_Test_Index is range 1 .. 128;

      package Unit_Test_Vs is new Stda.Record_Bounded_Vectors2
        (Element_Type => Unit_Test_Any_Ptr,
         Index_Type   => Unit_Test_Index);

      type Test_Suite is new Limited_Controlled with record
         Allocated_Memory : Unit_Test_Vs.Elements_Ptr;
         Tests            : Unit_Test_Vs.Vector;
      end record;

   end Test_Defs2;

   package Text_Files is

      function Count_Lines_In_File (File_Name : String) return Nat32;

      generic
         Type Text_Parser (<>) is limited private;
         with procedure Handle_Line (Parser : in out Text_Parser;
                                     Line  : String);
      procedure Read_Line_By_Line (Parser    : in out Text_Parser;
                                   File_Name : String);

   end Text_Files;

   package Text_IO is

      type Io_Target is abstract tagged limited null record;

      procedure Put_Line (Io   : in out Io_Target;
                          Text : String) is abstract;
      --  This is the procedure to use for printing to standard out!

      type Io_Target_Ptr is access all Io_Target'Class;

      function Std_Io return Io_Target_Ptr;
      --  Returns an access-to-object variable that references a unique
      --  global instance of terminal type that directs text to standard out.

   end Text_IO;

--     Bytes_To_Read : constant := 1024;
--
--     type Input_File (<>) is private;
--
--     function Create (Target : Io_Target) return Input_File;
--     --  Should not be called during elaboration time.
--     --  Creates an input file object.
--
--     procedure Open (File        : in out Input_File;
--                     Name        : in     String;
--                     Call_Result : in out Subprogram_Call_Result);
--
--     function End_Of_File (File : Input_File) return Boolean;
--
--     procedure Read (File : in out Input_File);
--
--     function Read_Data (File : Input_File) return Byte_Array;
--     --  Returns the data read the latest time the Read procedure was called.
--
--     procedure Close (File        : in out Input_File;
--                      Call_Result : in out Subprogram_Call_Result);
--
--     type Output_File (<>) is private;
--
--     function Create (Target : Io_Target) return Output_File;
--     --  Should not be called during elaboration time.
--     --  Creates an output file object.
--
--     procedure Create (File        : in out Output_File;
--                       Name        : in     String;
--                       Call_Result : in out Subprogram_Call_Result);
--     --  Creates a file for writing to. Will delete any already existing
--     --  file with the same file name.
--
--     procedure Write (File        : in out Output_File;
--                      Buffer      : in     Byte_Array;
--                      Call_Result : in out Subprogram_Call_Result);
--
--     function Bytes_Written_Count (File : Output_File) return Nat32;
--
--     procedure New_Line (File        : in out Output_File;
--                         Call_Result : in out Subprogram_Call_Result);
--
--     procedure Close (File        : in out Output_File;
--                      Call_Result : in out Subprogram_Call_Result);
--
   --
   --  The following definitions are only used for automated tests and usually
   --  not used in an application.
   --

   package Text_IO_Tests is

      Max_Lines : constant := 5;

      package Printed_Strings is new Stda.Bounded_Strings (Max_Length => 80);

      type Line_Index is range 1 .. Max_Lines;

      package Line_Vs is new Stda.Bounded_Vectors
        (Element_Type => Printed_Strings.Bounded_String,
         Index        => Line_Index);

      function Test_Io return Text_IO.Io_Target_Ptr;

      function Lines return Line_Vs.Vector;

      type Test_Io_Target is new Text_IO.Io_Target with private;

      procedure Put_Line (Terminal : in out Test_Io_Target;
                          Text     : String);

   private

      protected type Std_Out_Owner is
         procedure Append (Text : String);
         function Lines return Text_IO_Tests.Line_Vs.Vector;
      private
         My_Lines : Text_IO_Tests.Line_Vs.Vector;
      end Std_Out_Owner;

      type Std_Out_Proxy is record
         Owner : Std_Out_Owner;
      end record;

      type Std_Out_Proxy_Ptr is access all Std_Out_Proxy;
      for Std_Out_Proxy_Ptr'Storage_Size use 0;

      type Test_Io_Target is new Text_IO.Io_Target with record
         Owner : Std_Out_Owner;
      end record;

   end Text_IO_Tests;

private

   type Std_Io_Target is new Text_IO.Io_Target with null record;

   procedure Put_Line (Io   : in out Std_Io_Target;
                       Text : String);

--     package Private_Defs is
--
--        type Win32_Handle_Type is new Interfaces.C.int;
--
--        type Win32_Dword_Type is new Interfaces.C.unsigned;
--        --  On 64-bit target:
--        --  type Win32_DWORD is new Interfaces.C.unsigned_long;
--
--        type Win32_Bool is new Interfaces.C.int;
--
--        type Io_Target_Kind is
--          (Std_Out_Terminal,
--           --  The ordinary Ada standard library is used to implement all
--           --  the input-output functionality.
--
--           Win32_Target,
--
--           Test_Terminal
--           --  Everything that the application outputs will be saved to disk
--           --  that it can be analyzed with minimized usage of RAM memory.
--          );
--
--        type Open_Cmd_Ptr is access procedure
--          (File        : in out Input_File;
--           Name        : in     String;
--           Call_Result : in out Subprogram_Call_Result);
--
--        type End_Of_File_Cmd_Ptr is access function (File : Input_File)
--                                                     return Boolean;
--
--        type Read_Cmd_Ptr is access procedure (File : in out Input_File);
--
--        type Read_Data_Cmd_Ptr is access function (File : Input_File)
--                                                   return Byte_Array;
--
--        type Close_Cmd_Ptr is access procedure
--          (File        : in out Input_File;
--           Call_Result : in out Subprogram_Call_Result);
--
--        type Create_Cmd_Ptr is access procedure
--          (File        : in out Output_File;
--           Name        : in     String;
--           Call_Result : in out Subprogram_Call_Result);
--
--        type Write_Cmd_Ptr is access procedure
--          (File        : in out Output_File;
--           Buffer      : in     Byte_Array;
--           Call_Result : in out Subprogram_Call_Result);
--
--        type New_Line_Cmd_Ptr is access procedure
--          (File        : in out Output_File;
--           Call_Result : in out Subprogram_Call_Result);
--
--        type Close_Output_File_Cmd_Ptr is access procedure
--          (File        : in out Output_File;
--           Call_Result : in out Subprogram_Call_Result);
--
--        package Exported_Defs is
--
--           Std_Out_Terminal : constant Io_Target_Kind
--             := Private_Defs.Std_Out_Terminal;
--           Win32_Target     : constant Io_Target_Kind
--             := Private_Defs.Win32_Target;
--           Test_Terminal    : constant Io_Target_Kind
--             := Private_Defs.Test_Terminal;
--
--        end Exported_Defs;
--
--     end Private_Defs;
--
--     use Private_Defs.Exported_Defs;
--
--     --  This type definition exists to be able to take advantage of Ada's
--     --  aggregate feature when initializing an input file record
--     --  but not having to initialize the buffer which may be large.
--     type My_Input_File is record
--        Descriptor     : Private_Defs.Win32_Handle_Type;
--        Is_End_Of_File : Boolean;
--        Bytes_Read     : Private_Defs.Win32_Dword_Type;
--        Last_Call      : Private_Defs.Win32_Bool;
--        --  The result when the Read procedure was last called.
--
--        Count : Nat32;
--      --  The number of bytes read latest time the Read procedure was called.
--     end record;
--
--     type Input_File (Target : Private_Defs.Io_Target_Kind) is record
--        Buffer      : aliased Interfaces.C.char_array (1 .. Bytes_To_Read);
--        Open        : Private_Defs.Open_Cmd_Ptr;
--        End_Of_File : Private_Defs.End_Of_File_Cmd_Ptr;
--        Read        : Private_Defs.Read_Cmd_Ptr;
--        Read_Data   : Private_Defs.Read_Data_Cmd_Ptr;
--        Close       : Private_Defs.Close_Cmd_Ptr;
--        case Target is
--           when Std_Out_Terminal => null;
--           when Test_Terminal    => null;
--           when Win32_Target     => My : My_Input_File;
--        end case;
--     end record;
--
--     --  This type definition exists to be able to take advantage of Ada's
--     --  aggregate feature when initializing an input file record
--     --  but not having to initialize the buffer which may be large.
--     type My_Win32_Output_File is record
--        Descriptor     : Private_Defs.Win32_Handle_Type;
--        Count          : Nat32;
--        Is_End_Of_File : Boolean;
--     end record;
--
--     type Output_File (Target : Private_Defs.Io_Target_Kind) is record
--        Buffer   : aliased Interfaces.C.char_array (1 .. Bytes_To_Read);
--        Create   : Private_Defs.Create_Cmd_Ptr;
--        Write    : Private_Defs.Write_Cmd_Ptr;
--        New_Line : Private_Defs.New_Line_Cmd_Ptr;
--        Close    : Private_Defs.Close_Output_File_Cmd_Ptr;
--        case Target is
--           when Std_Out_Terminal => null;
--           when Test_Terminal    => null;
--           when Win32_Target     => My : My_Win32_Output_File;
--        end case;
--     end record;
--

--     type Put_Line_Ptr is access procedure (Io   : Io_Target;
--                                            Text : String);
--
--  --     type Io_Target
--  --       (Kind : Private_Defs.Io_Target_Kind := Std_Out_Terminal)
--  --     is record
--  --        Put_Line : Put_Line_Ptr;
--  --        case Kind is
--  --           when Std_Out_Terminal => null;
--  --           when Win32_Target     => null;
--  --           when Test_Terminal    => Term : Std_Out_Proxy_Ptr;
--  --        end case;
--  --     end record;
--
--     function Make_Std_Out return Io_Target;
--
--     function Make_Test (Proxy : Std_Out_Proxy_Ptr) return Io_Target;
--
--     type Create_Input_File_Cmd_Ptr is access function (Target : Io_Target)
--                                                        return Input_File;
--
--     type Create_Output_File_Cmd_Ptr is access function (Target : Io_Target)
--                                                         return Output_File;
--
--     procedure Put_Line_Impl (Terminal : Io_Target;
--                              Text     : String);
--
--     Create_Win32_Input_File : Create_Input_File_Cmd_Ptr;
--
--     Create_Win32_Output_File : Create_Output_File_Cmd_Ptr;

end Stdx;
