with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Command_Line;

package body Ac21.Aa is

   package String_Split renames Std.String_Split;

   package File_IO renames Std.File_IO;

   package Positive_IO is new Ada.Text_IO.Integer_IO (Positive);

   package Conversions renames Std.Conversions;

   use type Ada.Text_IO.Count;

   procedure Raise_Exception
     (E : Ada.Exceptions.Exception_Id; Message : String := "") renames
     Ada.Exceptions.Raise_Exception;

   procedure Put (Item : String) renames Ada.Text_IO.Put;

   procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1) renames
     Ada.Text_IO.New_Line;

   procedure Put (Number : Nat32) is
   begin
      Nat32_IO.Put (Item  => Number,
                    Width => 0);
   end Put;

   procedure Put (Number : Positive) is
   begin
      Positive_IO.Put (Item  => Number,
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

   In_File : constant Ada.Text_IO.File_Mode := Ada.Text_IO.In_File;

   procedure Open (File : in out Ada.Text_IO.File_Type;
                   Mode : Ada.Text_IO.File_Mode;
                   Name : String) is
   begin
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Name);
   end Open;

   procedure Close (File : in out Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Close (File);
   end Close;

   function End_Of_File (File : Ada.Text_IO.File_Type) return Boolean is
   begin
      return Ada.Text_IO.End_Of_File (File);
   end End_Of_File;

   function End_Of_Line (File : Ada.Text_IO.File_Type) return Boolean is
   begin
      return Ada.Text_IO.End_Of_Line (File);
   end End_Of_Line;

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

      --        procedure Free_Storage_Array is new Ada.Unchecked_Deallocation
      --          (Object => Storage_Array,
      --           Name => Storage_Array_Access);

      --------------------------------------------------------------

      procedure Allocate
        (Pool : in out Basic_Dynamic_Pool;
         Storage_Address : out Address;
         Size_In_Storage_Elements : Storage_Elements.Storage_Count;
         Alignment : Storage_Elements.Storage_Count)
      is
      begin

         --        pragma Assert (Is_Owner (Pool, Current_Task));

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

      type Depth_Type is range 0 .. Nat32'Last;
      --  The exact unit of a depth value is unspecified.
      --  This means it is not known if it is expressed in meters or not.

      package Depth_IO is new Ada.Text_IO.Integer_IO (Depth_Type);

      type Depth_Index is range 1 .. 8_048;

      type Depth_Array is array (Depth_Index range <>) of Depth_Type;

      procedure Run (File_Name : String) is

         Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool;

         type Depth_Array_Ptr is access Depth_Array;
         for Depth_Array_Ptr'Storage_Pool use Pool;

         function Allocate_Array return Depth_Array_Ptr is
            Result : Depth_Array_Ptr;

            Unused_Depth : Depth_Type;

            Last : Nat32 := 0;

            Text_File : File_IO.Text_File_With_Finalization;
            F : Ada.Text_IO.File_Type renames Text_File.Handle;
         begin
            Open (File => F,
                  Mode => In_File,
                  Name => File_Name);
            while not End_Of_File (F) loop
               Depth_IO.Get (File  => F,
                             Item  => Unused_Depth);
               Last := Last + 1;
            end loop;
            Ada.Text_IO.Close (F);
            Result := new Depth_Array (1 .. Depth_Index (Last));
            Open (File => F,
                  Mode => In_File,
                  Name => File_Name);
            for I in Depth_Index range 1 .. Depth_Index (Last) loop
               Depth_IO.Get (File  => F,
                             Item  => Result (I));
            end loop;
            Close (F);
            return Result;
         end Allocate_Array;

         Allocated_Depth_Array : constant Depth_Array_Ptr := Allocate_Array;

         Depth : Depth_Array renames Allocated_Depth_Array.all;

         Number_Of_Increases : Nat32 := 0;
      begin
         for I in Depth_Index range 2 .. Depth'Last loop
            if Depth (I) > Depth (I - 1) then
               Number_Of_Increases := Number_Of_Increases + 1;
            end if;
         end loop;

         Put ("Answer: ");
         Put (Number_Of_Increases);
         New_Line;
      end Run;

      procedure Run is
      begin
         Run ("2021_01r.txt");
      end Run;

      package Result_Should_Be_7_Test is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Result_Should_Be_7_Test;

      package body Result_Should_Be_7_Test is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Day 01, part 1. Result should be 7");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            Run ("2021_01t.txt");
         end Run;

         procedure Verify (Test : Unit_Test) is
         begin
            Test_Suite.Find_In_Standard_Output
              (Searched_For => "Answer: 7",
               Location     => (2094655197, -0933542565));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Result_Should_Be_7_Test;

   end Day_1_Part_One;

   package body Day_1_Part_Two is

      type Depth_Type is range 0 .. Nat32'Last;
      --  The exact unit of a depth value is unspecified.
      --  This means it is not known if it is expressed in meters or not.

      package Depth_IO is new Ada.Text_IO.Integer_IO (Depth_Type);

      type Depth_Index is range 1 .. 8_048;

      type Depth_Array is array (Depth_Index range <>) of Depth_Type;

      procedure Run (File_Name : String) is

         Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool;

         type Depth_Array_Ptr is access Depth_Array;
         for Depth_Array_Ptr'Storage_Pool use Pool;

         function Allocate_Array return Depth_Array_Ptr is
            Result : Depth_Array_Ptr;

            Unused_Depth : Depth_Type;

            Last : Nat32 := 0;

            Text_File : File_IO.Text_File_With_Finalization;
            F : Ada.Text_IO.File_Type renames Text_File.Handle;
         begin
            Open (File => F,
                  Mode => In_File,
                  Name => File_Name);
            while not End_Of_File (F) loop
               Depth_IO.Get (File  => F,
                             Item  => Unused_Depth);
               Last := Last + 1;
            end loop;
            Close (F);
            Result := new Depth_Array (1 .. Depth_Index (Last));
            Open (File => F,
                  Mode => In_File,
                  Name => File_Name);
            for I in Depth_Index range 1 .. Depth_Index (Last) loop
               Depth_IO.Get (File  => F,
                             Item  => Result (I));
            end loop;
            Close (F);
            return Result;
         end Allocate_Array;

         Allocated_Depth_Array : constant Depth_Array_Ptr := Allocate_Array;

         Depth : Depth_Array renames Allocated_Depth_Array.all;

         Allocated_Window_Array : constant Depth_Array_Ptr :=
           new Depth_Array (1 .. Depth'Last - 2);

         Window : Depth_Array renames Allocated_Window_Array.all;

         Number_Of_Increases : Nat32 := 0;
      begin
         for I in Depth_Index range 1 .. Depth'Last - 2 loop
            Window (I) := Depth (I) + Depth (I + 1) + Depth(I + 2);
         end loop;

         for I in Depth_Index range 2 .. Window'Last loop
            if Window (I) > Window (I - 1) then
               Number_Of_Increases := Number_Of_Increases + 1;
            end if;
         end loop;

         Put ("Answer: ");
         Put (Number_Of_Increases);
         New_Line;
      end Run;

      procedure Run is
      begin
         Run ("2021_01r.txt");
      end Run;

      package Result_Should_Be_5_Test is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);

         procedure Run (Test : Unit_Test);

         procedure Verify (Test : Unit_Test);

      end Result_Should_Be_5_Test;

      package body Result_Should_Be_5_Test is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Day 01, part 2. Result should be 5");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            Run ("2021_01t.txt");
         end Run;

         procedure Verify (Test : Unit_Test) is
         begin
            Test_Suite.Find_In_Standard_Output
              (Searched_For => "Answer: 5",
               Location     => (2094655193, -0933142565));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Result_Should_Be_5_Test;

   end Day_1_Part_Two;

   package body Day_2_Part_One is

      type Movement_Type is (Forward, Down, Up);

      package Movement_IO is new Ada.Text_IO.Enumeration_IO (Movement_Type);

      procedure Run (File_Name : String) is
         Text_File : File_IO.Text_File_With_Finalization;
         F : Ada.Text_IO.File_Type renames Text_File.Handle;

         X : Nat32 := 0;
         Y : Nat32 := 0;

         Direction : Movement_Type;
         Value     : Nat32;
      begin
         Open (File => F,
               Mode => In_File,
               Name => File_Name);
         while not End_Of_File (F) loop
            Movement_IO.Get (File  => F,
                             Item  => Direction);
            Nat32_IO.Get (File  => F,
                          Item  => Value);

            case Direction is
               when Forward => X := X + Value;
               when Up      => Y := Y - Value;
               when Down    => Y := Y + Value;
            end case;
         end loop;
         Close (F);

         Put ("Answer: ");
         Put (X * Y);
         New_Line;
      end Run;

      procedure Run is
      begin
         Run ("2021_02r.txt");
      end Run;

      package Result_Should_Be_150_Test is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Result_Should_Be_150_Test;

      package body Result_Should_Be_150_Test is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Day 02, part 1. Result should be 150");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            Run ("2021_02t.txt");
         end Run;

         procedure Verify (Test : Unit_Test) is
         begin
            Test_Suite.Find_In_Standard_Output
              (Searched_For => "Answer: 150",
               Location     => (-0471962700, -0020094807));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Result_Should_Be_150_Test;

   end Day_2_Part_One;

   package body Day_2_Part_Two is

      type Movement_Type is (Forward, Down, Up);

      package Movement_IO is new Ada.Text_IO.Enumeration_IO (Movement_Type);

      procedure Run (File_Name : String) is
         Text_File : File_IO.Text_File_With_Finalization;
         F : Ada.Text_IO.File_Type renames Text_File.Handle;

         X   : Nat32 := 0;
         Y   : Nat32 := 0;
         Aim : Nat32 := 0;

         Direction : Movement_Type;
         Value     : Nat32;
      begin
         Open (File => F,
               Mode => In_File,
               Name => File_Name);
         while not End_Of_File (F) loop
            Movement_IO.Get (File  => F,
                             Item  => Direction);
            Nat32_IO.Get (File  => F,
                          Item  => Value);
            case Direction is
               when Forward => X := X + Value; Y := Y + Value * Aim;
               when Up      => Aim := Aim - Value;
               when Down    => Aim := Aim + Value;
            end case;
         end loop;
         Close (F);

         Put ("Answer: ");
         Put (X * Y);
         New_Line;
      end Run;

      procedure Run is
      begin
         Run ("2021_02r.txt");
      end Run;

      package Result_Should_Be_900_Test is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Result_Should_Be_900_Test;

      package body Result_Should_Be_900_Test is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Day 02, part 2. Result should be 900");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            Run ("2021_02t.txt");
         end Run;

         procedure Verify (Test : Unit_Test) is
         begin
            Test_Suite.Find_In_Standard_Output
              (Searched_For => "Answer: 900",
               Location     => (-0716431279, -0535269018));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Result_Should_Be_900_Test;

   end Day_2_Part_Two;

   package body Day_3_Part_One is

      procedure Run (File_Name : String) is

         --  This function returns an integer of Positive type since
         --  the index type of String types is Positive.
         function Calculate_Line_Length return Positive is
            Text_File : File_IO.Text_File_With_Finalization;
            F : Ada.Text_IO.File_Type renames Text_File.Handle;

            C : Character;
            L : Natural := 0;
         begin
            Open (File => F,
                  Mode => In_File,
                  Name => File_Name);
            while not End_Of_Line (File => F) loop
               Ada.Text_IO.Get (File => F,
                                Item => C);
               L := L + 1;
            end loop;
            Close (File => F);
            return L;
         end Calculate_Line_Length;

         Line_Length : constant Positive := Calculate_Line_Length;

         subtype Column_Number is Positive range 1 .. Line_Length;
         --  This type is a subtype of Positive in order to be compatible
         --  with the Positive index type used in Ada's standard String.

         L : String (1 .. Line_Length);

         Text_File : File_IO.Text_File_With_Finalization;
         F : Ada.Text_IO.File_Type renames Text_File.Handle;

         One_Count  : Nat32;
         Zero_Count : Nat32;

         Gamma_Input   : String (Column_Number'Range);
         Epsilon_Input : String (Column_Number'Range);

         Gamma   : Nat32;
         Epsilon : Nat32;
      begin
         Put ("Line length: ");
         Put (L'Last);
         New_Line;
         for I in Column_Number'Range loop
            Zero_Count := 0;
            One_Count  := 0;

            Open (File => F,
                  Mode => In_File,
                  Name => File_Name);
            while not End_Of_File (File => F) loop
               for K in L'Range loop
                  Ada.Text_IO.Get (File => F,
                                   Item => L (K));
               end loop;

               if L (I) = '0' then
                  Zero_Count := Zero_Count + 1;
               elsif L (I) = '1' then
                  One_Count := One_Count + 1;
               else
                  raise Constraint_Error;
               end if;
            end loop;
            Close (File => F);

            if One_Count > Zero_Count then
               Gamma_Input   (I) := '1';
               Epsilon_Input (I) := '0';
            elsif One_Count < Zero_Count then
               Gamma_Input   (I) := '0';
               Epsilon_Input (I) := '1';
            else
               raise Constraint_Error;
            end if;
         end loop;

         Gamma   := Nat32'Value ("2#" & Gamma_Input   & "#");
         Epsilon := Nat32'Value ("2#" & Epsilon_Input & "#");

         Put_Line (Gamma_Input);
         Put_Line (Epsilon_Input);
         Put_Line (Gamma);
         Put_Line (Epsilon);
         Put ("Answer: ");
         Put (Gamma * Epsilon);
         New_Line;
         --  Answer: 2743844
      end Run;

      procedure Run is
      begin
         Run ("2021_03r.txt");
      end Run;

      package Result_Should_Be_198_Test is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Result_Should_Be_198_Test;

      package body Result_Should_Be_198_Test is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Day 02, part 1. Result should be 198");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            Run ("2021_03t.txt");
         end Run;

         procedure Verify (Test : Unit_Test) is
         begin
            Test_Suite.Find_In_Standard_Output
              (Searched_For => "Answer: 198",
               Location     => (0707478102, -0008758331));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Result_Should_Be_198_Test;

   end Day_3_Part_One;

   package body Day_3_Part_Two is

      type String_Vector_Index is range 1 .. 1_024;

      type Counted_Type is record
         Zero_Count : Natural;
         One_Count  : Natural;
      end record;

      procedure Run (File_Name : String) is
         Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool;

         --  This function returns an integer of Positive type since
         --  the index type of String types is Positive.
         function Calculate_Line_Length return Positive is
            Text_File : File_IO.Text_File_With_Finalization;
            F : Ada.Text_IO.File_Type renames Text_File.Handle;

            C : Character;
            L : Natural := 0;
         begin
            Open (File => F,
                  Mode => In_File,
                  Name => File_Name);
            while not End_Of_Line (File => F) loop
               Ada.Text_IO.Get (File => F,
                                Item => C);
               L := L + 1;
            end loop;
            Close (File => F);
            return L;
         end Calculate_Line_Length;

         Line_Length : constant Positive := Calculate_Line_Length;

         subtype Column_Number is Positive range 1 .. Line_Length;
         --  This type is a subtype of Positive in order to be compatible
         --  with the Positive index type used in Ada's standard String.

         subtype Input_String is String (Column_Number'Range);

         package V is

            subtype Extended_Index is String_Vector_Index'Base range
              0 .. String_Vector_Index'Last;

            procedure Append (New_Item : Input_String);
            procedure Replace_Element (Index       : String_Vector_Index;
                                       New_Element : Input_String);
            procedure Replace_Last_Element (New_Element : Input_String);
            procedure Delete_Last;
            procedure Delete (Item : Input_String);
            procedure Clear;
            function Contains (Searched_For : Input_String) return Boolean;
            function Last return Extended_Index;
            function Element (Index : String_Vector_Index) return Input_String;
            function Is_Empty return Boolean;
            function Is_Full return Boolean;
            function Last_Element return Input_String;
            function Length return Nat32;

         private

            type Elements_Array is array (String_Vector_Index) of Input_String;
            --  The component type in the array is not aliased on purpose.
            --  Not allowed to reference reference individual
            --  elements in the array through some access-to-object type.

            type Elements_Array_Ptr is access Elements_Array;
            for Elements_Array_Ptr'Storage_Pool use Pool;

            Items   : Elements_Array_Ptr := new Elements_Array;
            My_Last : Extended_Index := Extended_Index'First;

         end V;

         package body V is

            function Last return Extended_Index is
            begin
               return My_Last;
            end Last;

            function Is_Empty return Boolean is
            begin
               return My_Last = Extended_Index'First;
            end Is_Empty;

            function Is_Full return Boolean is
            begin
               return My_Last = String_Vector_Index'Last;
            end Is_Full;

            procedure Append (New_Item : Input_String) is
            begin
               My_Last := My_Last + 1;
               Items (String_Vector_Index (My_Last)) := New_Item;
            end Append;

            function Contains (Searched_For : Input_String) return Boolean
            is
               Result : Boolean := False;
            begin
               for I in Extended_Index range 1 .. My_Last loop
                  if Items (I) = Searched_For then
                     Result := True;
                     exit;
                  end if;
               end loop;
               return Result;
            end Contains;

            function Element (Index : String_Vector_Index)
                              return Input_String is
            begin
               return Items (Index);
            end Element;

            function Last_Element return Input_String is
            begin
               return Items (String_Vector_Index (My_Last));
            end Last_Element;

            procedure Delete_Last is
            begin
               My_Last := My_Last - 1;
            end Delete_Last;

            --  Deletes all instances of the item from the vector.
            procedure Delete (Item : Input_String) is
            begin
               for I in String_Vector_Index range 1 .. My_Last loop
                  if Items (I) = Item then
                     Items.all (I .. My_Last - 1) :=
                       Items.all (I + 1 .. My_Last);
                     My_Last := My_Last - 1;
                  end if;
               end loop;
            end Delete;

            procedure Clear is
            begin
               My_Last := Extended_Index'First;
            end Clear;

            procedure Replace_Element (Index       : String_Vector_Index;
                                       New_Element : Input_String) is
            begin
               Items (Index) := New_Element;
            end Replace_Element;

            procedure Replace_Last_Element (New_Element : Input_String) is
            begin
               Items (Last) := New_Element;
            end Replace_Last_Element;

            function Length return Nat32 is
            begin
               return Nat32 (My_Last);
            end Length;

         begin
            if String_Vector_Index'First /= 1 then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity, "");
            end if;
         end V;

         package To_Delete is

            subtype Extended_Index is String_Vector_Index'Base range
              0 .. String_Vector_Index'Last;

            procedure Append (New_Item : Input_String);
            procedure Replace_Element (Index       : String_Vector_Index;
                                       New_Element : Input_String);
            procedure Replace_Last_Element (New_Element : Input_String);
            procedure Delete_Last;
            procedure Delete (Item : Input_String);
            procedure Clear;
            function Contains (Searched_For : Input_String) return Boolean;
            function Last return Extended_Index;
            function Element (Index : String_Vector_Index) return Input_String;
            function Is_Empty return Boolean;
            function Is_Full return Boolean;
            function Last_Element return Input_String;
            function Length return Nat32;

         private

            type Elements_Array is array (String_Vector_Index) of Input_String;
            --  The component type in the array is not aliased on purpose.
            --  Not allowed to reference reference individual
            --  elements in the array through some access-to-object type.

            type Elements_Array_Ptr is access Elements_Array;
            for Elements_Array_Ptr'Storage_Pool use Pool;

            Items   : Elements_Array_Ptr := new Elements_Array;
            My_Last : Extended_Index := Extended_Index'First;

         end To_Delete;

         package body To_Delete is

            function Last return Extended_Index is
            begin
               return My_Last;
            end Last;

            function Is_Empty return Boolean is
            begin
               return My_Last = Extended_Index'First;
            end Is_Empty;

            function Is_Full return Boolean is
            begin
               return My_Last = String_Vector_Index'Last;
            end Is_Full;

            procedure Append (New_Item : Input_String) is
            begin
               My_Last := My_Last + 1;
               Items (String_Vector_Index (My_Last)) := New_Item;
            end Append;

            function Contains (Searched_For : Input_String) return Boolean
            is
               Result : Boolean := False;
            begin
               for I in Extended_Index range 1 .. My_Last loop
                  if Items (I) = Searched_For then
                     Result := True;
                     exit;
                  end if;
               end loop;
               return Result;
            end Contains;

            function Element (Index : String_Vector_Index)
                              return Input_String is
            begin
               return Items (Index);
            end Element;

            function Last_Element return Input_String is
            begin
               return Items (String_Vector_Index (My_Last));
            end Last_Element;

            procedure Delete_Last is
            begin
               My_Last := My_Last - 1;
            end Delete_Last;

            --  Deletes all instances of the item from the vector.
            procedure Delete (Item : Input_String) is
            begin
               for I in String_Vector_Index range 1 .. My_Last loop
                  if Items (I) = Item then
                     Items.all (I .. My_Last - 1) :=
                       Items.all (I + 1 .. My_Last);
                     My_Last := My_Last - 1;
                  end if;
               end loop;
            end Delete;

            procedure Clear is
            begin
               My_Last := Extended_Index'First;
            end Clear;

            procedure Replace_Element (Index       : String_Vector_Index;
                                       New_Element : Input_String) is
            begin
               Items (Index) := New_Element;
            end Replace_Element;

            procedure Replace_Last_Element (New_Element : Input_String) is
            begin
               Items (Last) := New_Element;
            end Replace_Last_Element;

            function Length return Nat32 is
            begin
               return Nat32 (My_Last);
            end Length;

         begin
            if String_Vector_Index'First /= 1 then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity, "");
            end if;
         end To_Delete;

         function Count (C : Column_Number) return Counted_Type is
            S : Input_String;

            Result : Counted_Type := (0, 0);
         begin
            for I in String_Vector_Index range 1 .. V.Last loop
               S := V.Element (I);
               if S (S'First + C - 1) = '0' then
                  Result.Zero_Count := Result.Zero_Count + 1;
               elsif S (S'First + C - 1) = '1' then
                  Result.One_Count := Result.One_Count + 1;
               else
                  raise Constraint_Error;
               end if;
            end loop;

            return Result;
         end Count;

         type Filter_Action is (Keep_Item,
                                Remove_Item);

         procedure Filter_Out (Column    : Column_Number;
                               Marker    : Character)
         is
            Item   : Input_String;
            Action : Filter_Action;
         begin
            To_Delete.Clear;
            for I in String_Vector_Index range 1 .. V.Last loop
               Item := V.Element (I);
               if Item (Column) = Marker then
                  Action := Remove_Item;
               else
                  Action := Keep_Item;
               end if;
               case Action is
                  when Keep_Item   => null;
                  when Remove_Item => To_Delete.Append (Item);
               end case;
            end loop;

            for I in String_Vector_Index range 1 .. To_Delete.Last loop
               Item := To_Delete.Element (I);
               V.Delete (Item);
            end loop;
         end Filter_Out;

         Text_File : File_IO.Text_File_With_Finalization;
         F : Ada.Text_IO.File_Type renames Text_File.Handle;

         L : String (1 .. Line_Length);

         Oxygen : String (Column_Number'Range);
         Co2    : String (Column_Number'Range);
         Counted : Counted_Type;

         G : Nat32;
         E : Nat32;
      begin
         Open (File => F,
               Mode => In_File,
               Name => File_Name);
         while not End_Of_File (File => F) loop
            for K in L'Range loop
               Ada.Text_IO.Get (File => F,
                                Item => L (K));
            end loop;
            V.Append (L);
         end loop;
         Close (File => F);

         --  Oxygen
         for I in Column_Number'Range loop
            Counted := Count (I);
            if Counted.Zero_Count > Counted.One_Count then
               Filter_Out (Column    => I,
                           Marker    => '1');
            elsif Counted.Zero_Count <= Counted.One_Count then
               Filter_Out (Column    => I,
                           Marker    => '0');
            end if;

            if V.Length = 1 then
               Put_Line ("Success! " & V.Element (1));
               Oxygen := V.Element (1);
               exit;
            end if;
         end loop;

         V.Clear;
         Open (File => F,
               Mode => In_File,
               Name => File_Name);
         while not End_Of_File (File => F) loop
            for K in L'Range loop
               Ada.Text_IO.Get (File => F,
                                Item => L (K));
            end loop;
            V.Append (L);
         end loop;
         Close (File => F);

         --  CO2
         for I in Column_Number'Range loop
            Counted := Count (I);
            if Counted.Zero_Count > Counted.One_Count then
               Filter_Out (Column    => I,
                           Marker    => '0');
            elsif Counted.Zero_Count <= Counted.One_Count then
               Filter_Out (Column    => I,
                           Marker    => '1');
            end if;

            if V.Length = 1 then
               Put_Line ("Success! " & V.Element (1));
               Co2 := V.Element (1);
               exit;
            end if;
         end loop;

         G := Nat32'Value ("2#" & Oxygen & "#");
         E := Nat32'Value ("2#" & Co2    & "#");
         Put_Line (G);
         Put_Line (E);
         Put ("Answer:");
         Put (G * E);
         New_Line;
         --  Answer: 6677951
      end Run;

      procedure Run is
      begin
         Run ("2021_03r.txt");
      end Run;

   end Day_3_Part_Two;

   package body Day_4_Part_One is

      type Number_Index is range 1 .. 1024;

      type Board_Cell is record
         Is_Marked : Boolean;
         Value     : Nat32;
      end record;

      subtype Row_Type    is Positive range 1 .. 5;
      subtype Column_Type is Positive range 1 .. 5;

      type Board_Type is array (Row_Type, Column_Type) of Board_Cell;

      function Is_Winner (Board : Board_Type) return Boolean is
         Count : Natural;
      begin
         for Row in Row_Type loop
            Count := 0;
            for Column in Column_Type loop
               if Board (Row, Column).Is_Marked then
                  Count := Count + 1;
               end if;
            end loop;
            if Count = 5 then
               return True;
            end if;
         end loop;

         for Column in Column_Type loop
            Count := 0;
            for Row in Row_Type loop
               if Board (Row, Column).Is_Marked then
                  Count := Count + 1;
               end if;
            end loop;
            if Count = 5 then
               return True;
            end if;
         end loop;

         return False;
      end Is_Winner;

      procedure Mark (Board        : in out Board_Type;
                      Searched_For : Nat32) is
      begin
         for R in Row_Type loop
            for C in Column_Type loop
               if Board (R, C).Value = Searched_For then
                  Board (R, C).Is_Marked := True;
               end if;
            end loop;
         end loop;
      end Mark;

      function Sum_Of_All_Unmarked_Numbers (Board : Board_Type)
                                            return Nat32 is
         Sum : Nat32 := 0;
      begin
         for R in Row_Type loop
            for C in Column_Type loop
               if not Board (R, C).Is_Marked then
                  Sum := Sum + Board (R, C).Value;
               end if;
            end loop;
         end loop;
         return Sum;
      end Sum_Of_All_Unmarked_Numbers;

      procedure Put_Line (Board : Board_Type) is
      begin
         for R in Row_Type loop
            for C in Column_Type loop
               if Board (R, C).Is_Marked then
                  Put ("*");
                  Nat32_IO.Put (Board (R, C).Value, 3);
               else
                  Nat32_IO.Put (Board (R, C).Value, 4);
               end if;
            end loop;
            New_Line;
         end loop;
      end Put_Line;

      type Board_Index is range 1 .. 1024;

      procedure Run (File_Name : String) is
         Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool;

         package Numbers is

            subtype Extended_Index is Number_Index'Base range
              0 .. Number_Index'Last;

            procedure Append (New_Item : Nat32);
            procedure Replace_Element (Index       : Number_Index;
                                       New_Element : Nat32);
            procedure Replace_Last_Element (New_Element : Nat32);
            procedure Delete_Last;
            procedure Delete (Item : Nat32);
            procedure Clear;
            function Contains (Searched_For : Nat32) return Boolean;
            function Last return Extended_Index;
            function Element (Index : Number_Index) return Nat32;
            function Is_Empty return Boolean;
            function Is_Full return Boolean;
            function Last_Element return Nat32;
            function Length return Nat32;

         private

            type Elements_Array is array (Number_Index) of Nat32;
            --  The component type in the array is not aliased on purpose.
            --  Not allowed to reference reference individual
            --  elements in the array through some access-to-object type.

            type Elements_Array_Ptr is access Elements_Array;
            for Elements_Array_Ptr'Storage_Pool use Pool;

            Items   : Elements_Array_Ptr := new Elements_Array;
            My_Last : Extended_Index := Extended_Index'First;

         end Numbers;

         package body Numbers is

            function Last return Extended_Index is
            begin
               return My_Last;
            end Last;

            function Is_Empty return Boolean is
            begin
               return My_Last = Extended_Index'First;
            end Is_Empty;

            function Is_Full return Boolean is
            begin
               return My_Last = Number_Index'Last;
            end Is_Full;

            procedure Append (New_Item : Nat32) is
            begin
               My_Last := My_Last + 1;
               Items (Number_Index (My_Last)) := New_Item;
            end Append;

            function Contains (Searched_For : Nat32) return Boolean
            is
               Result : Boolean := False;
            begin
               for I in Extended_Index range Number_Index'First .. My_Last loop
                  if Items (I) = Searched_For then
                     Result := True;
                     exit;
                  end if;
               end loop;
               return Result;
            end Contains;

            function Element (Index : Number_Index) return Nat32 is
            begin
               return Items (Index);
            end Element;

            function Last_Element return Nat32 is
            begin
               return Items (Number_Index (My_Last));
            end Last_Element;

            procedure Delete_Last is
            begin
               My_Last := My_Last - 1;
            end Delete_Last;

            --  Deletes all instances of the item from the vector.
            procedure Delete (Item : Nat32) is
            begin
               for I in Number_Index range Number_Index'First .. My_Last loop
                  if Items (I) = Item then
                     Items.all (I .. My_Last - 1) :=
                       Items.all (I + 1 .. My_Last);
                     My_Last := My_Last - 1;
                  end if;
               end loop;
            end Delete;

            procedure Clear is
            begin
               My_Last := Extended_Index'First;
            end Clear;

            procedure Replace_Element (Index       : Number_Index;
                                       New_Element : Nat32) is
            begin
               Items (Index) := New_Element;
            end Replace_Element;

            procedure Replace_Last_Element (New_Element : Nat32) is
            begin
               Items (Last) := New_Element;
            end Replace_Last_Element;

            function Length return Nat32 is
            begin
               return Nat32 (My_Last);
            end Length;

         begin
            if Number_Index'First /= 1 then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity, "");
            end if;
         end Numbers;

         package Boards is

            subtype Extended_Index is Board_Index'Base range
              0 .. Board_Index'Last;

            procedure Append (New_Item : Board_Type);
            procedure Replace_Element (Index       : Board_Index;
                                       New_Element : Board_Type);
            procedure Replace_Last_Element (New_Element : Board_Type);
            procedure Delete_Last;
            procedure Delete (Item : Board_Type);
            procedure Clear;
            function Contains (Searched_For : Board_Type) return Boolean;
            function Last return Extended_Index;
            function Element (Index : Board_Index) return Board_Type;
            function Is_Empty return Boolean;
            function Is_Full return Boolean;
            function Last_Element return Board_Type;
            function Length return Nat32;

         private

            type Elements_Array is array (Board_Index) of Board_Type;
            --  The component type in the array is not aliased on purpose.
            --  Not allowed to reference reference individual
            --  elements in the array through some access-to-object type.

            type Elements_Array_Ptr is access Elements_Array;
            for Elements_Array_Ptr'Storage_Pool use Pool;

            Items   : Elements_Array_Ptr := new Elements_Array;
            My_Last : Extended_Index := Extended_Index'First;

         end Boards;

         package body Boards is

            function Last return Extended_Index is
            begin
               return My_Last;
            end Last;

            function Is_Empty return Boolean is
            begin
               return My_Last = Extended_Index'First;
            end Is_Empty;

            function Is_Full return Boolean is
            begin
               return My_Last = Board_Index'Last;
            end Is_Full;

            procedure Append (New_Item : Board_Type) is
            begin
               My_Last := My_Last + 1;
               Items (Board_Index (My_Last)) := New_Item;
            end Append;

            function Contains (Searched_For : Board_Type) return Boolean
            is
               Result : Boolean := False;
            begin
               for I in Extended_Index range Board_Index'First .. My_Last loop
                  if Items (I) = Searched_For then
                     Result := True;
                     exit;
                  end if;
               end loop;
               return Result;
            end Contains;

            function Element (Index : Board_Index) return Board_Type is
            begin
               return Items (Index);
            end Element;

            function Last_Element return Board_Type is
            begin
               return Items (Board_Index (My_Last));
            end Last_Element;

            procedure Delete_Last is
            begin
               My_Last := My_Last - 1;
            end Delete_Last;

            --  Deletes all instances of the item from the vector.
            procedure Delete (Item : Board_Type) is
            begin
               for I in Board_Index range Board_Index'First .. My_Last loop
                  if Items (I) = Item then
                     Items.all (I .. My_Last - 1) :=
                       Items.all (I + 1 .. My_Last);
                     My_Last := My_Last - 1;
                  end if;
               end loop;
            end Delete;

            procedure Clear is
            begin
               My_Last := Extended_Index'First;
            end Clear;

            procedure Replace_Element (Index       : Board_Index;
                                       New_Element : Board_Type) is
            begin
               Items (Index) := New_Element;
            end Replace_Element;

            procedure Replace_Last_Element (New_Element : Board_Type) is
            begin
               Items (Last) := New_Element;
            end Replace_Last_Element;

            function Length return Nat32 is
            begin
               return Nat32 (My_Last);
            end Length;

         begin
            if Board_Index'First /= 1 then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity, "");
            end if;
         end Boards;

         Text_File : File_IO.Text_File_With_Finalization;
         F : Ada.Text_IO.File_Type renames Text_File.Handle;

         Is_Winner_Found : Boolean := False;
         Wb              : Board_Type;
         Wn              : Nat32;

         B     : Board_Type;
         N     : Nat32;
         C     : Character;
      begin
         Put_Line ("Day 04, part one");
         Put_Line ("----------------");

         Open (File => F,
               Mode => In_File,
               Name => File_Name);
         Nat32_IO.Get (File => F,
                       Item => N);
         Numbers.Append (New_Item => N);
         while not End_Of_Line (File => F) loop
            Ada.Text_IO.Get (File => F,
                             Item => C);
            Nat32_IO.Get (File => F,
                          Item => N);
            if C /= ',' then
               Raise_Exception (Constraint_Error'Identity);
            end if;
            Numbers.Append (New_Item => N);
         end loop;
         while not End_Of_File (File => F) loop
            for Row in Row_Type'Range loop
               for Column in Column_Type'Range loop
                  Nat32_IO.Get (File => F,
                                Item => N);
                  B (Row, Column) := (Is_Marked => False,
                                      Value     => N);
               end loop;
            end loop;
            Boards.Append (New_Item => B);
         end loop;
         Close (File => F);

         Outer_Loop : for I in Number_Index range 1 .. Numbers.Last loop
            for J in Board_Index range 1 .. Boards.Last loop
               B := Boards.Element (J);
               Mark (Board        => B,
                     Searched_For => Numbers.Element (I));
               Boards.Replace_Element (J, B);
            end loop;

            for J in Board_Index range 1 .. Boards.Last loop
               B := Boards.Element (J);
               if Is_Winner (Board => B) then
                  Put_Line ("Winner found!");
                  Is_Winner_Found := True;
                  Wb := B;
                  Wn := Numbers.Element (I);
                  exit Outer_Loop;
               end if;
            end loop;
         end loop Outer_Loop;

         if Is_Winner_Found then
            Put ("Wn");
            Put (Wn);
            New_Line;
            Put ("unmarked ");
            Put (Sum_Of_All_Unmarked_Numbers (Wb));
            New_Line;
            Put ("Answer: ");
            Put (Sum_Of_All_Unmarked_Numbers (Wb) * Wn);
            New_Line;
            --  Answer: 45031
         else
            Put ("Winner not found.");
            New_Line;
         end if;
      end Run;

      procedure Run is
      begin
         Run ("2021_04r.txt");
      end Run;

      package Result_Should_Be_4512_Test is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Result_Should_Be_4512_Test;

      package body Result_Should_Be_4512_Test is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Day 04, part 1. Result should be 4512");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            Run ("2021_04t.txt");
         end Run;

         procedure Verify (Test : Unit_Test) is
         begin
            Test_Suite.Find_In_Standard_Output
              (Searched_For => "Answer: 4512",
               Location     => (1983643366, -1549476615));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Result_Should_Be_4512_Test;

   end Day_4_Part_One;

end Ac21.Aa;
