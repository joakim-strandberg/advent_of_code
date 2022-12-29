with Std;
with Ada.Text_IO;

with Ac21_Tst;
with Ada.Command_Line;
with Ada.Exceptions;
with Stdb;

package body Ac21.Ab is

   use Std.Types;

   package Basic_Bounded_Dynamic_Pools renames Aa.Basic_Bounded_Dynamic_Pools;

   package Nat32_IO renames Ac21.Aa.Nat32_IO;

   package Test_Suite renames Ac21_Tst.Test_Suite;

   package String_Split renames Std.String_Split;

   procedure Put (Item : String) renames Ada.Text_IO.Put;

   procedure Put_Line (Item : String) renames Ada.Text_IO.Put_Line;

   procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1) renames
     Ada.Text_IO.New_Line;

   procedure Put (Number : Nat32) is
   begin
      Nat32_IO.Put (Item  => Number,
                    Width => 0);
   end Put;

   package body Day_4_Part_Two is

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
         --  ((for some Row in Row_Type =>
         --        (for all I in Column_Type => Board (Row, I).Is_Marked))
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

         --  (for some Column in Column_Type =>
         --     (for all I in Row_Type => Board (I, Column).Is_Marked))
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

      procedure Parse_Board_Row (Board : in out Board_Type;
                                 Line  : String;
                                 Row   : Positive) is
         Slices : String_Split.Variable_Length_Interval_Array :=
           String_Split.Get_Slices (Line, ' ');
      begin
         for I in String_Split.Interval_Array_Index range 1 .. 5 loop
            declare
               S : constant String :=
                 Line (Slices.Interval (I).First .. Slices.Interval (I).Last);
            begin
               --  Put_Line ("wl: " & T);
               Board (Row, Positive (I)) := (Is_Marked => False,
                                             Value     => Nat32'Value (S));
            end;
         end loop;
      end Parse_Board_Row;

      --        package Fb is new Containers.Bounded_Vector
      --          (Element_Type => Board_Type,
      --           Index_Type   => Board_Index);

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

         -- Boards to filter out
         package Fb is

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

         end Fb;

         package body Fb is

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
         end Fb;

         Controlled_File : Std.File_IO.Text_File;
         F : Ada.Text_IO.File_Type renames Controlled_File.File;

         Is_Winner_Found : Boolean := False;
         Wb              : Board_Type;  --  Winner board
         Wn              : Nat32;       --  Winner number

         B : Board_Type;
         N : Nat32;
         C : Character;
      begin
         Ada.Text_IO.Open (File => F,
                           Mode => Ada.Text_IO.In_File,
                           Name => File_Name);
         Nat32_IO.Get (File => F,
                       Item => N);
         Numbers.Append (New_Item => N);
         while not Ada.Text_IO.End_Of_Line (File => F) loop
            Ada.Text_IO.Get (File => F,
                             Item => C);
            Nat32_IO.Get (File => F,
                          Item => N);
            if C /= ',' then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity);
            end if;
            Numbers.Append (New_Item => N);
         end loop;
         while not Ada.Text_IO.End_Of_File (File => F) loop
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
         Ada.Text_IO.Close (File => F);

         Outer_Loop : for I in Number_Index range 1 .. Numbers.Last loop
            for J in Board_Index range 1 .. Boards.Last loop
               B := Boards.Element (J);
               Mark (Board        => B,
                     Searched_For => Numbers.Element (I));
               Boards.Replace_Element (J, B);
            end loop;

            Fb.Clear;
            for J in Board_Index range 1 .. Boards.Last loop
               if Is_Winner (Board => Boards.Element (J)) then
                  Fb.Append (New_Item  => Boards.Element (J));
                  --  Put_Line (Boards.Element (J));
                  --  New_Line;

                  if Boards.Length = 1 then
                     Put_Line ("Last Winner found!");

                     Is_Winner_Found := True;
                     Wb := Boards.Element (1);
                     Wn := Numbers.Element (I);
                     Put_Line (Wb);
                     exit Outer_Loop;
                  end if;
               end if;
            end loop;

            for J in Board_Index range 1 .. Fb.Last loop
               Boards.Delete (Fb.Element (J));
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
            --  Answer: 2568
         end if;
      end Run;

      procedure Run is
      begin
         Run ("2021_04r.txt");
      end Run;

   end Day_4_Part_Two;

   package body Day_5_Part_One is

      type Index_Interval is record
         First : Natural;
         Last  : Natural;
      end record;

      type Index_Interval_Array is array (Positive range <>) of Index_Interval;

      function Filter_Spaces (Line : String) return String is
         L : Natural := 0;
      begin
         for I in Line'Range loop
            if Line (I) = ' ' then
               if I = Line'First then
                  null;
               elsif I > Line'First and then Line (I - 1) = ' ' then
                  null;
               else
                  L := L + 1;
               end if;
            else
               L := L + 1;
            end if;
         end loop;

         --  Text_IO.Put_Line ("d" & Line);
         --  Text_IO.Put_Line (Line'Length'Img);
         --  Text_IO.Put_Line (L'Img);

         declare
            R : String (1 .. L);  --  Result
            C : Positive := 1;    --  Current
         begin
            for I in Line'Range loop
               if Line (I) = ' ' then
                  if I = Line'First then
                     null;
                  elsif I > Line'First and then Line (I - 1) = ' ' then
                     null;
                  else
                     R (C) := Line (I);
                     C := C + 1;
                  end if;
               else
                  R (C) := Line (I);
                  C := C + 1;
               end if;
            end loop;
            Put_Line (R);
            return R;
         end;
      end Filter_Spaces;

      function Get_Intervals (Line : String;
                              Sep  : Character)
                              return Index_Interval_Array is
         Comma_Count : Natural := 0;
      begin
         for I in Line'Range loop
            if Line (I) = Sep then
               Comma_Count := Comma_Count + 1;
            end if;
         end loop;

         declare
            Result     : Index_Interval_Array (1 .. Comma_Count + 1);
            Current    : Positive := 1;
            Prev_Index : Positive := Line'First;
         begin
            for I in Line'Range loop
               if Line (I) = Sep then
                  --  Text_IO.Put_Line (Line (Prev_Index .. I - 1));
                  Result (Current).First := Prev_Index;
                  Result (Current).Last  := I - 1;
                  Prev_Index := I + 1;
                  Current := Current + 1;
                  Result (Current).First := Prev_Index;
               end if;
            end loop;
            Result (Result'Last).Last := Line'Last;
            return Result;
         end;
      end Get_Intervals;

      type Board_Cell is record
         Count : Natural;
      end record;

      type X_Type is range 0 .. 999;
      type Y_Type is range 0 .. 999;

      package X_IO is new Ada.Text_IO.Integer_IO (X_Type);
      package Y_IO is new Ada.Text_IO.Integer_IO (Y_Type);

      type Board_Type is array (X_Type, Y_Type) of Board_Cell;

      procedure Run (File_Name  : String) is
         Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool;

         type Board_Ptr is access Board_Type;
         for  Board_Ptr'Storage_Pool use Pool;

         Allocated_Board : Board_Ptr := new Board_Type;
         Board : Board_Type renames Allocated_Board.all;

         Controlled_File : Std.File_IO.Text_File;
         F : Ada.Text_IO.File_Type renames Controlled_File.File;

         C : Character;

         Count : Nat32 := 0;
         X0 : X_Type;
         Y0 : Y_Type;
         X1 : X_Type;
         Y1 : Y_Type;
      begin
         Board := (others => (others => (Count => 0)));

         Ada.Text_IO.Open (File => F,
                           Mode => Ada.Text_IO.In_File,
                           Name => File_Name);
         while not Ada.Text_IO.End_Of_File (File => F) loop
            X_IO.Get (File => F,
                      Item => X0);
            Ada.Text_IO.Get (File => F,
                             Item => C);
            if C /= ',' then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity);
            end if;
            Y_IO.Get (File  => F,
                      Item  => Y0);
            Ada.Text_IO.Get (File => F,
                             Item => C);
            if C /= ' ' then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity);
            end if;
            Ada.Text_IO.Get (File => F,
                             Item => C);
            if C /= '-' then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity);
            end if;
            Ada.Text_IO.Get (File => F,
                             Item => C);
            if C /= '>' then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity);
            end if;
            Ada.Text_IO.Get (File => F,
                             Item => C);
            if C /= ' ' then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity);
            end if;
            X_IO.Get (File => F,
                      Item => X1);
            Ada.Text_IO.Get (File => F,
                             Item => C);
            if C /= ',' then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity);
            end if;
            Y_IO.Get (File  => F,
                      Item  => Y1);

            if X0 = X1 then
               if Y0 <= Y1 then
                  for I in Y_Type range Y0 .. Y1 loop
                     Board (X0, I).Count := Board (X0, I).Count + 1;
                  end loop;
               else
                  for I in Y_Type range Y1 .. Y0 loop
                     Board (X0, I).Count := Board (X0, I).Count + 1;
                  end loop;
               end if;
            elsif Y0 = Y1 then
               if X0 <= X1 then
                  for I in X_Type range X0 .. X1 loop
                     Board (I, Y0).Count := Board (I, Y0).Count + 1;
                  end loop;
               else
                  for I in X_Type range X1 .. X0 loop
                     Board (I, Y0).Count := Board (I, Y0).Count + 1;
                  end loop;
               end if;
            else
               --  Put_Line ("What to do: " & Line.Text);
               null;
               --  Should be ignored for now
            end if;
         end loop;
         Ada.Text_IO.Close (File => F);

         for X in X_Type loop
            for Y in Y_Type loop
               if Board (X, Y).Count > 1 then
                  Count := Count + 1;
               end if;
            end loop;
         end loop;
         Put ("Answer: ");
         Put (Count);
         New_Line;
      end Run;

      procedure Run is
      begin
         Run ("2021_05r.txt");
      end Run;

      package Result_Should_Be_5_Test is

         type Unit_Test is new Test_Suite.Unit_Test with null record;

         procedure Put_Name (Test : Unit_Test);
         procedure Run      (Test : Unit_Test);
         procedure Verify   (Test : Unit_Test);

      end Result_Should_Be_5_Test;

      package body Result_Should_Be_5_Test is

         procedure Put_Name (Test : Unit_Test) is
         begin
            Put ("Day 05, part 1. Result should be 5");
         end Put_Name;

         procedure Run (Test : Unit_Test) is
         begin
            Run ("2021_05t.txt");
         end Run;

         procedure Verify (Test : Unit_Test) is
         begin
            Test_Suite.Find_In_Standard_Output
              (Searched_For => "Answer: 5",
               Location     => (0873526814, -1535564459));
         end Verify;

         Test : Unit_Test;
      begin
         Test_Suite.Run_Test (Test);
      end Result_Should_Be_5_Test;

   end Day_5_Part_One;

   package body Day_5_Part_Two is

      type Index_Interval is record
         First : Natural;
         Last  : Natural;
      end record;

      type Index_Interval_Array is array (Positive range <>) of Index_Interval;

      function Filter_Spaces (Line : String) return String is
         L : Natural := 0;
      begin
         for I in Line'Range loop
            if Line (I) = ' ' then
               if I = Line'First then
                  null;
               elsif I > Line'First and then Line (I - 1) = ' ' then
                  null;
               else
                  L := L + 1;
               end if;
            else
               L := L + 1;
            end if;
         end loop;

         --  Text_IO.Put_Line ("d" & Line);
         --  Text_IO.Put_Line (Line'Length'Img);
         --  Text_IO.Put_Line (L'Img);

         declare
            R : String (1 .. L);  --  Result
            C : Positive := 1;    --  Current
         begin
            for I in Line'Range loop
               if Line (I) = ' ' then
                  if I = Line'First then
                     null;
                  elsif I > Line'First and then Line (I - 1) = ' ' then
                     null;
                  else
                     R (C) := Line (I);
                     C := C + 1;
                  end if;
               else
                  R (C) := Line (I);
                  C := C + 1;
               end if;
            end loop;
            Put_Line (R);
            return R;
         end;
      end Filter_Spaces;

      function Get_Intervals (Line : String;
                              Sep  : Character)
                              return Index_Interval_Array is
         Comma_Count : Natural := 0;
      begin
         for I in Line'Range loop
            if Line (I) = Sep then
               Comma_Count := Comma_Count + 1;
            end if;
         end loop;

         declare
            Result     : Index_Interval_Array (1 .. Comma_Count + 1);
            Current    : Positive := 1;
            Prev_Index : Positive := Line'First;
         begin
            for I in Line'Range loop
               if Line (I) = Sep then
                  --  Text_IO.Put_Line (Line (Prev_Index .. I - 1));
                  Result (Current).First := Prev_Index;
                  Result (Current).Last  := I - 1;
                  Prev_Index := I + 1;
                  Current := Current + 1;
                  Result (Current).First := Prev_Index;
               end if;
            end loop;
            Result (Result'Last).Last := Line'Last;
            return Result;
         end;
      end Get_Intervals;

      type Board_Cell is record
         Count : Natural;
      end record;

      type X_Type is range 0 .. 999;
      type Y_Type is range 0 .. 999;

      package X_IO is new Ada.Text_IO.Integer_IO (X_Type);
      package Y_IO is new Ada.Text_IO.Integer_IO (Y_Type);

      type Board_Type is array (X_Type, Y_Type) of Board_Cell;

      procedure Run (File_Name : String) is
         Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool;

         type Board_Ptr is access Board_Type;
         for  Board_Ptr'Storage_Pool use Pool;

         Allocated_Board : Board_Ptr := new Board_Type;
         Board : Board_Type renames Allocated_Board.all;

         Controlled_File : Std.File_IO.Text_File;
         F : Ada.Text_IO.File_Type renames Controlled_File.File;

         C : Character;

         Count : Nat32 := 0;
         X0 : X_Type;
         Y0 : Y_Type;
         X1 : X_Type;
         Y1 : Y_Type;
         Y : Y_Type;
      begin
         Board := (others => (others => (Count => 0)));

         Ada.Text_IO.Open (File => F,
                           Mode => Ada.Text_IO.In_File,
                           Name => File_Name);
         while not Ada.Text_IO.End_Of_File (File => F) loop
            X_IO.Get (File => F,
                      Item => X0);
            Ada.Text_IO.Get (File => F,
                             Item => C);
            if C /= ',' then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity);
            end if;
            Y_IO.Get (File  => F,
                      Item  => Y0);
            Ada.Text_IO.Get (File => F,
                             Item => C);
            if C /= ' ' then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity);
            end if;
            Ada.Text_IO.Get (File => F,
                             Item => C);
            if C /= '-' then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity);
            end if;
            Ada.Text_IO.Get (File => F,
                             Item => C);
            if C /= '>' then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity);
            end if;
            Ada.Text_IO.Get (File => F,
                             Item => C);
            if C /= ' ' then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity);
            end if;
            X_IO.Get (File => F,
                      Item => X1);
            Ada.Text_IO.Get (File => F,
                             Item => C);
            if C /= ',' then
               Ada.Exceptions.Raise_Exception (Constraint_Error'Identity);
            end if;
            Y_IO.Get (File  => F,
                      Item  => Y1);
            if X0 = X1 then
               if Y0 <= Y1 then
                  for I in Y_Type range Y0 .. Y1 loop
                     Board (X0, I).Count := Board (X0, I).Count + 1;
                  end loop;
               else
                  for I in Y_Type range Y1 .. Y0 loop
                     Board (X0, I).Count := Board (X0, I).Count + 1;
                  end loop;
               end if;
            elsif Y0 = Y1 then
               if X0 <= X1 then
                  for I in X_Type range X0 .. X1 loop
                     Board (I, Y0).Count := Board (I, Y0).Count + 1;
                  end loop;
               else
                  for I in X_Type range X1 .. X0 loop
                     Board (I, Y0).Count := Board (I, Y0).Count + 1;
                  end loop;
               end if;
            else
               if X0 <= X1 then
                  Y := Y0;
                  for I in X_Type range X0 .. X1 loop
                     Board (I, Y).Count := Board (I, Y).Count + 1;

                     if I = X1 and then not (Y = Y1) then
                        Put_Line ("0. End point not correct.");
                     end if;

                     if Y0 <= Y1 then
                        Y := Y + 1;
                     else
                        Y := Y - 1;
                     end if;
                  end loop;
               else
                  Y := Y1;
                  for I in X_Type range X1 .. X0 loop
                     Board (I, Y).Count := Board (I, Y).Count + 1;
                     if I = X0 and then not (Y = Y0) then
                        Put ("1. End point not correct.");
                        New_Line;
                     end if;

                     if Y1 <= Y0 then
                        Y := Y + 1;
                     else
                        Y := Y - 1;
                     end if;
                  end loop;
               end if;
            end if;
         end loop;
         Ada.Text_IO.Close (File => F);

         for I in X_Type loop
            for J in Y_Type loop
               if Board (I, J).Count > 1 then
                  Count := Count + 1;
               end if;
            end loop;
         end loop;
         Put ("Answer: ");
         Put (Count);
         New_Line;
      end Run;

      procedure Run is
      begin
         Run ("2021_05r.txt");
      end Run;

   end Day_5_Part_Two;

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
end Ac21.Ab;
