with Stdx;
pragma Elaborate_All (Stdx);

with Stdb;
with Stdh;

with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Finalization;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;
with Ada.Containers.Generic_Array_Sort;
with Ada.Task_Identification;

package body Advent_Of_Code_2021 is

   subtype Nat32 is Stda.Types.Nat32;
   subtype Pos32 is Stda.Types.Pos32;

   package Exceptions renames Ada.Exceptions;

   package Text_IO renames Ada.Text_IO;

   package Nat32_IO is new Text_IO.Integer_IO (Nat32);

   package Latin_1 renames Stda.Latin_1;

   package Text_Files renames Stdx.Text_Files;

   package Big_Integers renames Stdb.Big_Integers;

   subtype Limited_Controlled is Ada.Finalization.Limited_Controlled;

   use type Nat32;
   use type Pos32;
   use type Big_Integers.Multi_Int;

   function Trim (Text : String) return String is
   begin
      return Ada.Strings.Fixed.Trim (Text, Ada.Strings.Both);
   end Trim;

   Pool : Stdh.Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool (10_000_000);
   --  This pool is shared by all puzzles, but it is only one puzzle
   --  that is solved each time the application is executed.

   type String_Ptr is access constant String;
   for String_Ptr'Storage_Pool use Pool;

   type String_Array is array (Pos32 range <>) of String_Ptr;

   type String_Array_Ptr is access String_Array;
   for String_Array_Ptr'Storage_Pool use Pool;

   package Text_File_Parser is

      type String_Ptr is access constant String;
      for String_Ptr'Storage_Pool use Pool;

      type String_Array is array (Pos32 range <>) of String_Ptr;

      type String_Array_Ptr is access String_Array;
      for String_Array_Ptr'Storage_Pool use Pool;

      type T (File_Name : String_Ptr) is record
         Line    : String_Array_Ptr;
         Current : Pos32;
      end record;

      procedure Read_From_File (Parser : in out T);

      Empty_File_Error : exception;

   end Text_File_Parser;

   package body Text_File_Parser is

      procedure Read_Line (Parser : in out T;
                           Line   : String) is
      begin
         Parser.Line (Parser.Current) := new String'(Line);
         Parser.Current := Parser.Current + 1;
      end Read_Line;

      procedure Read_Values_From_File is new Text_Files.Read_Line_By_Line
        (Text_Parser => T,
         Handle_Line => Read_Line);

      procedure Read_From_File (Parser : in out T) is
         Line_Count : constant Nat32
           := Text_Files.Count_Lines_In_File (Parser.File_Name.all);
      begin
         if Line_Count = 0 then
            Text_IO.Put_Line ("Error, empty file " & Parser.File_Name.all);
            Ada.Exceptions.Raise_Exception
              (Empty_File_Error'Identity, Parser.File_Name.all);
         end if;
         Parser.Current := 1;
         Parser.Line    := new String_Array (1 .. Line_Count);
         Read_Values_From_File (Parser, Parser.File_Name.all);
      end Read_From_File;

   end Text_File_Parser;

   package body Day_1 is

      type Depth_Type is new Nat32 range 0 .. Nat32'Last;
      --  The exact unit of a depth value is unspecified.
      --  This means it is not known if it is expressed in meters or not.

      type Depth_Array is array (Pos32 range <>) of Depth_Type;

      type Depth_Array_Ptr is access Depth_Array;
      for  Depth_Array_Ptr'Storage_Pool use Pool;

      function Count_Increases (Depth : Depth_Array) return Nat32 is
         Result : Nat32 := 0;
      begin
         for I in Nat32 range Depth'First + 1 .. Depth'Last loop
            if Depth (I) > Depth (I - 1) then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Count_Increases;

      File_Name : aliased constant String := "day_01_input.txt";

      package body Part_One is

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);
            Depth : Depth_Array_Ptr;
         begin
            Run_Test_Suite;

            Text_File_Parser.Read_From_File (Parser);

            Depth := new Depth_Array (Parser.Line'Range);
            for Li in Parser.Line'Range loop
               Depth (Li) := Depth_Type'Value (Parser.Line (Li).all);
            end loop;
            Text_IO.Put_Line (Nat32'Image (Count_Increases (Depth.all)));
         end Run;

         package Result_Should_Be_7_Test is

            type Test_Type is new Stdx.Test_Defs2.Unit_Test with null record;

            function Name (Test : Test_Type) return String;

            procedure Run (Test : in out Test_Type);

         end Result_Should_Be_7_Test;

         package body Result_Should_Be_7_Test is

            function Name (Test : Test_Type) return String is
            begin
               return "Result should be 7";
            end Name;

            procedure Run (Test : in out Test_Type) is
               Depth : constant Depth_Array
                 := (199, 200, 208, 210, 200, 207, 240, 269, 260, 263);

               Increases_Count : constant Nat32
                 := Count_Increases (Depth);
            begin
               Stdx.Test_Defs2.Assert (Increases_Count = 7,
                                       Location => (2094655197, -0933542565));
            end Run;

         end Result_Should_Be_7_Test;

         Test_1 : aliased Result_Should_Be_7_Test.Test_Type;

         procedure Run_Test_Suite is
            Suite : Stdx.Test_Defs2.Test_Suite;
         begin
            Stdx.Test_Defs2.Add (Suite, Test_1'Access);
            Stdx.Test_Defs2.Run_Tests (Suite);

            declare
               Total_Test_Count  : constant String
                 := Stdx.Test_Statistics.Total_Test_Count;
               Passed_Test_Count : constant String
                 := Stdx.Test_Statistics.Passed_Test_Count;
               Failed_Test_Count : constant String
                 := Stdx.Test_Statistics.Failed_Test_Count;
            begin
               Text_IO.Put_Line ("Total number of tests:" & Total_Test_Count);
               Text_IO.Put_Line ("Tests failed:" & Failed_Test_Count);
               Text_IO.Put_Line ("Tests passed:" & Passed_Test_Count);
            end;
         end Run_Test_Suite;

      end Part_One;

      package body Part_Two is

         procedure Make_Sliding_Window
           (Window : in out Depth_Array;
            Depth  : in     Depth_Array) is
         begin
            for I in Nat32 range Depth'First .. Depth'Last - 2 loop
               Window (I) := Depth (I) + Depth (I + 1) + Depth (I + 2);
            end loop;
         end Make_Sliding_Window;

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);

            Depth  : Depth_Array_Ptr;
            Window : Depth_Array_Ptr;
         begin
            Run_Test_Suite;

            Text_File_Parser.Read_From_File (Parser);

            Depth  := new Depth_Array (1 .. Parser.Line'Last);
            Window := new Depth_Array (1 .. Parser.Line'Last - 2);
            for Li in Parser.Line'Range loop
               Depth (Li) := Depth_Type'Value (Parser.Line (Li).all);
            end loop;
            Make_Sliding_Window (Window.all, Depth.all);
            Text_IO.Put_Line (Nat32'Image
                              (Count_Increases (Window.all)));
         end Run;

         package Result_Should_Be_5_Test is

            type Test_Type is new Stdx.Test_Defs2.Unit_Test with null record;

            function Name (Test : Test_Type) return String;

            procedure Run (Test : in out Test_Type);

         end Result_Should_Be_5_Test;

         package body Result_Should_Be_5_Test is

            function Name (Test : Test_Type) return String is
            begin
               return "Result should be 5";
            end Name;

            procedure Run (Test : in out Test_Type) is
               Depth : constant Depth_Array
                 := (199, 200, 208, 210, 200, 207, 240, 269, 260, 263);

               Window : Depth_Array (1 .. Depth'Last - 2);

               Increases_Count : Nat32;
            begin
               Make_Sliding_Window (Window, Depth);
               Increases_Count := Count_Increases (Window);
               Stdx.Test_Defs2.Assert (Increases_Count = 5,
                                       Location => (2094655193, -0933142565));
            end Run;

         end Result_Should_Be_5_Test;

         Test_1 : aliased Result_Should_Be_5_Test.Test_Type;

         procedure Run_Test_Suite is
            Suite : Stdx.Test_Defs2.Test_Suite;
         begin
            Stdx.Test_Defs2.Add (Suite, Test_1'Access);
            Stdx.Test_Defs2.Run_Tests (Suite);

            declare
               Total_Test_Count  : constant String
                 := Stdx.Test_Statistics.Total_Test_Count;
               Passed_Test_Count : constant String
                 := Stdx.Test_Statistics.Passed_Test_Count;
               Failed_Test_Count : constant String
                 := Stdx.Test_Statistics.Failed_Test_Count;
            begin
               Text_IO.Put_Line ("Total number of tests:" & Total_Test_Count);
               Text_IO.Put_Line ("Tests failed:" & Failed_Test_Count);
               Text_IO.Put_Line ("Tests passed:" & Passed_Test_Count);
            end;
         end Run_Test_Suite;

      end Part_Two;

   end Day_1;

   package body Day_2 is

      type Movement_Type is (Forward,
                             Down,
                             Up);

      File_Name : aliased constant String := "day_02_input.txt";

      package body Part_One is

         procedure Run is
            X : Nat32 := 0;
            Y : Nat32 := 0;

            Parser : Text_File_Parser.T (File_Name'Access);
         begin
            Text_File_Parser.Read_From_File (Parser);
            for Li in Parser.Line'Range loop
               declare
                  Line : constant String := Parser.Line (Li).all;
                  Sep : constant Integer :=
                    Ada.Strings.Fixed.Index (Source  => Line,
                                             Pattern => " ");

                  Direction : constant Movement_Type
                    := Movement_Type'Value (Line (Line'First .. Sep - 1));
                  Value     : constant Nat32
                    := Nat32'Value (Line (Sep + 1 .. Line'Last));
               begin
                  --  Text_IO.Put_Line ("1:" & Line (1 .. Sep - 1));
                  --  Text_IO.Put_Line ("2:" & Line (Sep + 1 .. Line'Last));
                  case Direction is
                     when Forward => X := X + Value;
                     when Up      => Y := Y - Value;
                     when Down    => Y := Y + Value;
                  end case;
               end;
            end loop;

            Text_IO.Put_Line (Nat32'Image (X * Y));
         end Run;

      end Part_One;

      package body Part_Two is

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);
            X   : Nat32 := 0;
            Y   : Nat32 := 0;
            Aim : Nat32 := 0;
         begin
            Text_File_Parser.Read_From_File (Parser);
            for Li in Parser.Line'Range loop
               declare
                  Line : constant String := Parser.Line (Li).all;
                  Sep : constant Integer :=
                    Ada.Strings.Fixed.Index (Source  => Line,
                                             Pattern => " ");

                  Direction : Movement_Type
                    := Movement_Type'Value (Line (Line'First .. Sep - 1));
                  Value     : Nat32
                    := Nat32'Value (Line (Sep + 1 .. Line'Last));
               begin
                  case Direction is
                     when Forward => X := X + Value; Y := Y + Value * Aim;
                     when Up      => Aim := Aim - Value;
                     when Down    => Aim := Aim + Value;
                  end case;
               end;
            end loop;

            Text_IO.Put_Line (Nat32'Image (X * Y));
         end Run;

      end Part_Two;

   end Day_2;

   package body Day_3 is

      subtype Column_Number is Positive range 1 .. 12;
      --  This type is a subtype of Positive in order to be compatible
      --  with the Positive index type used in Ada's standard String.

      File_Name : aliased constant String := "day_03_input.txt";

      package body Part_One is

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);

            One_Count  : Nat32;
            Zero_Count : Nat32;

            Gamma_Input   : String (Column_Number'Range);
            Epsilon_Input : String (Column_Number'Range);
         begin
            Text_File_Parser.Read_From_File (Parser);

            for I in Column_Number'Range loop
               Zero_Count := 0;
               One_Count  := 0;
               for J in Parser.Line.all'Range loop
                  declare
                     Line : constant String := Parser.Line (J).all;
                  begin
                     if Line (Line'First + I - 1) = '0' then
                        Zero_Count := Zero_Count + 1;
                     elsif Line (Line'First + I - 1) = '1' then
                        One_Count := One_Count + 1;
                     else
                        raise Constraint_Error;
                     end if;
                  end;
               end loop;
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

            declare
               Gamma   : Nat32 := Nat32'Value ("2#" & Gamma_Input & "#");
               Epsilon : Nat32 := Nat32'Value ("2#" & Epsilon_Input & "#");
            begin
               Text_IO.Put_Line (Gamma_Input);
               Text_IO.Put_Line (Epsilon_Input);
               Text_IO.Put_Line (Nat32'Image (Gamma));
               Text_IO.Put_Line (Nat32'Image (Epsilon));
               Text_IO.Put_Line ("Answer:" & Nat32'Image (Gamma * Epsilon));
               --  Answer: 2743844
            end;
         end Run;

      end Part_One;

      package body Part_Two is

         subtype Input_String is String (Column_Number'Range);

         type String_Vector_Index is range 1 .. 1_024;

         package S_Vc is new Stdh.Unbounded_Bounded_Vectors
           (Index_Type   => String_Vector_Index,
            Element_Type => Input_String);

         type Counted_Type is record
            Zero_Count : Natural;
            One_Count  : Natural;
         end record;

         function Count (V : S_Vc.Vector;
                         C : Column_Number) return Counted_Type
         is
            First : constant String_Vector_Index := S_Vc.First_Index;
            Last  : constant S_Vc.Extended_Index := S_Vc.Last_Index (V);
            S     : Input_String;

            Result : Counted_Type := (0, 0);
         begin
            for I in String_Vector_Index range First .. Last loop
               S := S_Vc.Element (V, I);
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

         procedure Filter_Out (Container : in out S_Vc.Vector;
                               Column    : Column_Number;
                               Marker    : Character)
         is
            To_Delete : S_Vc.Vector;
         begin
            declare
               First  : constant String_Vector_Index := S_Vc.First_Index;
               Last   : constant S_Vc.Extended_Index
                 := S_Vc.Last_Index (Container);
               Item   : Input_String;
               Action : Filter_Action;
            begin
               for I in String_Vector_Index range First .. Last loop
                  Item := S_Vc.Element (Container, I);
                  if Item (Column) = Marker then
                     Action := Remove_Item;
                  else
                     Action := Keep_Item;
                  end if;
                  case Action is
                     when Keep_Item   => null;
                     when Remove_Item => S_Vc.Append (To_Delete, Item);
                  end case;
               end loop;
            end;

            declare
               First : constant String_Vector_Index := S_Vc.First_Index;
               Last  : constant S_Vc.Extended_Index
                 := S_Vc.Last_Index (To_Delete);
               Item  : Input_String;
            begin
               for I in String_Vector_Index range First .. Last loop
                  Item := S_Vc.Element (To_Delete, I);
                  S_Vc.Delete (Container, Item);
               end loop;
            end;
         end Filter_Out;

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);

            Oxygen : String (Column_Number'Range);
            Co2    : String (Column_Number'Range);

            S : S_Vc.Vector;
         begin
            Text_File_Parser.Read_From_File (Parser);

            for Li in Parser.Line'Range loop
               S_Vc.Append (S, Parser.Line (Li).all);
            end loop;

            --  Oxygen
            for I in Column_Number'Range loop
               declare
                  Counted : constant Counted_Type := Count (V => S,
                                                            C => I);
               begin
                  if Counted.Zero_Count > Counted.One_Count then
                     Filter_Out (Container => S,
                                 Column    => I,
                                 Marker    => '1');
                  elsif Counted.Zero_Count <= Counted.One_Count then
                     Filter_Out (Container => S,
                                 Column    => I,
                                 Marker    => '0');
                  end if;
               end;

               if S_Vc.Length (S) = 1 then
                  Text_IO.Put_Line ("Success! " & S_Vc.Element (S, 1));
                  Oxygen := S_Vc.Element (S, 1);
                  exit;
               end if;
            end loop;

            S_Vc.Clear (S);
            for Li in Parser.Line'Range loop
               S_Vc.Append (S, Parser.Line (Li).all);
            end loop;

            --  CO2
            for I in Column_Number'Range loop
               declare
                  Counted : constant Counted_Type := Count (V => S,
                                                            C => I);
               begin
                  if Counted.Zero_Count > Counted.One_Count then
                     Filter_Out (Container => S,
                                 Column    => I,
                                 Marker    => '0');
                  elsif Counted.Zero_Count <= Counted.One_Count then
                     Filter_Out (Container => S,
                                 Column    => I,
                                 Marker    => '1');
                  end if;
               end;

               if S_Vc.Length (S) = 1 then
                  Text_IO.Put_Line ("Success! " & S_Vc.Element (S, 1));
                  Co2 := S_Vc.Element (S, 1);
                  exit;
               end if;
            end loop;

            declare
               G : Nat32 := Nat32'Value ("2#" & Oxygen & "#");
               E : Nat32 := Nat32'Value ("2#" & Co2   & "#");
            begin
               Text_IO.Put_Line (G'Image);
               Text_IO.Put_Line (E'Image);
               Text_IO.Put_Line ("Answer:" & Nat32'Image (G * E));
               --  Answer: 6677951
            end;
         end Run;

      end Part_Two;

   end Day_3;

   package body Day_4 is

      type Parser_State is (Reading_First_Line,
                            Read_Board_1,
                            Read_Board_2,
                            Read_Board_3,
                            Read_Board_4,
                            Read_Board_5);

      type Number_Index is range 1 .. 1024;

      package Number_Vectors is new Stdh.Unbounded_Bounded_Vectors
        (Index_Type   => Number_Index,
         Element_Type => Nat32);

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
                  Text_IO.Put ("*");
                  Nat32_IO.Put (Board (R, C).Value, 3);
               else
                  Nat32_IO.Put (Board (R, C).Value, 4);
               end if;
            end loop;
            Text_IO.New_Line;
         end loop;
      end Put_Line;

      type Board_Index is range 1 .. 1024;

      package Board_Vs is new Stda.Pool_Bounded_Vectors
        (Index_Type   => Board_Index,
         Element_Type => Board_Type);

      type Board_Vector_Ptr is access Board_Vs.Vector;
      for Board_Vector_Ptr'Storage_Pool use Pool;

      type Parser_Type is record
         State   : Parser_State := Reading_First_Line;
         Numbers : Number_Vectors.Vector;
         Board   : Board_Type;
         Boards  : Board_Vector_Ptr := new Board_Vs.Vector;
      end record;

      --  type Parser_Ptr is access Parser_Type;
      --  for Parser_Ptr'Storage_Pool use Pool;

      type Index_Interval is record
         First : Natural;
         Last  : Natural;
      end record;

      type Index_Interval_Array is array
        (Positive range <>) of Index_Interval;

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
            Text_IO.Put_Line (R);
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

      procedure Parse_Board_Row (Board : in out Board_Type;
                                 Line   : String;
                                 Row    : Positive) is
         FL  : constant String := Filter_Spaces (Line);
         Int : constant Index_Interval_Array
           := Get_Intervals (FL, ' ');
      begin
         for I in Positive range 1 .. 5 loop
            declare
               T : constant String
                 := FL (Int (I).First .. Int (I).Last);
            begin
               --  Text_IO.Put_Line ("wl: " & T);
               Board (Row, I)
                 := (Is_Marked => False,
                     Value     => Nat32'Value (T));
            end;
         end loop;
      end Parse_Board_Row;

      File_Name : aliased constant String := "day_04_input.txt";

      package body Part_One is

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);

            Is_Winner_Found : Boolean := False;
            Wb              : Board_Type;
            Wn              : Nat32;

            State   : Parser_State := Reading_First_Line;
            Numbers : Number_Vectors.Vector;
            Board   : Board_Type;
            Boards  : Board_Vector_Ptr := new Board_Vs.Vector;
         begin
            Text_File_Parser.Read_From_File (Parser);

            for Li in Parser.Line'Range loop
               declare
                  Line : constant String := Parser.Line (Li).all;
               begin
                  case State is
                  when Reading_First_Line =>
                     declare
                        Int : constant Index_Interval_Array
                          := Get_Intervals (Line, ',');
                     begin
                        -- Text_IO.Put_Line
                        --   ("last: "  &
                        --      Line (Intervals (Intervals'First).First ..
                        --        Intervals (Intervals'First).Last));
                        for I in Int'Range loop
                           Number_Vectors.Append
                             (This     => Numbers,
                              New_Item =>
                                (Nat32'Value
                                     (Line (Int (I).First .. Int (I).Last))));
                        end loop;
                     end;
                     State := Read_Board_1;
                  when Read_Board_1 =>
                     if Line'Length > 0 then
                        Parse_Board_Row (Board, Line, 1);
                        State := Read_Board_2;
                     end if;
                  when Read_Board_2 =>
                     if Line'Length > 0 then
                        Parse_Board_Row (Board, Line, 2);
                        State := Read_Board_3;
                     end if;
                  when Read_Board_3 =>
                     if Line'Length > 0 then
                        Parse_Board_Row (Board, Line, 3);
                        State := Read_Board_4;
                     end if;
                  when Read_Board_4 =>
                     if Line'Length > 0 then
                        Parse_Board_Row (Board, Line, 4);
                        State := Read_Board_5;
                     end if;
                  when Read_Board_5 =>
                     if Line'Length > 0 then
                        Parse_Board_Row (Board, Line, 5);
                        Board_Vs.Append (Container => Boards,
                                         New_Item  => Board);
                        --  Text_IO.Put_Line ("Adding board");
                        State := Read_Board_1;
                     end if;
                  end case;

               end;
            end loop;

            declare
               First : constant Number_Index := Number_Index'First;
               Last  : constant Number_Vectors.Extended_Index
                 := Number_Vectors.Last_Index (Numbers);
               N     : Nat32;
            begin
               Outer_Loop : for I in Number_Index range First .. Last loop
                  N := Number_Vectors.Element (Numbers, I);

                  declare
                     B     : Board_Type;
                     Bord : constant Board_Vs.Read_Only_Element_Array_Ptr
                       := Board_Vs.Array_View (Container => Boards);
                  begin
                     for J in Bord'Range loop
                        if Bord (J).Exists then
                           B := Bord (J).Value;
                           Mark (Board        => B,
                                 Searched_For => N);
                           Board_Vs.Replace_Element (Boards.all, J, B);
                        else
                           exit;
                        end if;
                     end loop;
                  end;

                  declare
                     B     : Board_Type;
                     Bord : constant Board_Vs.Read_Only_Element_Array_Ptr
                       := Board_Vs.Array_View (Container => Boards);
                  begin
                     for J in Bord'Range loop
                        if Bord (J).Exists then
                           B := Bord (J).Value;
                           if Is_Winner (Board => B) then
                              Text_IO.Put_Line ("Winner found!");
                              Is_Winner_Found := True;
                              Wb := B;
                              Wn := N;
                              exit Outer_Loop;
                           end if;
                        else
                           exit;
                        end if;
                     end loop;
                  end;
               end loop Outer_Loop;
            end;

            if Is_Winner_Found then
               Text_IO.Put_Line ("Wn" & Nat32'Image (Wn));
               Text_IO.Put_Line
                 ("unmarked " & Nat32'Image
                    (Sum_Of_All_Unmarked_Numbers (Wb)));
               Text_IO.Put_Line
                 (Nat32'Image (Sum_Of_All_Unmarked_Numbers (Wb) * Wn));
               --  Anser: 45031
            end if;
         end Run;

      end Part_One;

      package body Part_Two is

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);

            Is_Winner_Found : Boolean := False;
            Wb              : Board_Type;
            Wn              : Nat32;

            State   : Parser_State := Reading_First_Line;
            Numbers : Number_Vectors.Vector;
            Board   : Board_Type;
            Boards  : Board_Vector_Ptr := new Board_Vs.Vector;
         begin
            Text_File_Parser.Read_From_File (Parser);

            for Li in Parser.Line'Range loop
               declare
                  Line : constant String := Parser.Line (Li).all;
               begin
                  case State is
                  when Reading_First_Line =>
                     declare
                        Int : constant Index_Interval_Array
                          := Get_Intervals (Line, ',');
                     begin
                        -- Text_IO.Put_Line
                        --   ("last: "  &
                        --      Line (Intervals (Intervals'First).First ..
                        --        Intervals (Intervals'First).Last));
                        for I in Int'Range loop
                           Number_Vectors.Append
                             (This     => Numbers,
                              New_Item =>
                                (Nat32'Value
                                     (Line (Int (I).First .. Int (I).Last))));
                        end loop;
                     end;
                     State := Read_Board_1;
                  when Read_Board_1 =>
                     if Line'Length > 0 then
                        Parse_Board_Row (Board, Line, 1);
                        State := Read_Board_2;
                     end if;
                  when Read_Board_2 =>
                     if Line'Length > 0 then
                        Parse_Board_Row (Board, Line, 2);
                        State := Read_Board_3;
                     end if;
                  when Read_Board_3 =>
                     if Line'Length > 0 then
                        Parse_Board_Row (Board, Line, 3);
                        State := Read_Board_4;
                     end if;
                  when Read_Board_4 =>
                     if Line'Length > 0 then
                        Parse_Board_Row (Board, Line, 4);
                        State := Read_Board_5;
                     end if;
                  when Read_Board_5 =>
                     if Line'Length > 0 then
                        Parse_Board_Row (Board, Line, 5);
                        Board_Vs.Append (Container => Boards,
                                         New_Item  => Board);
                        --  Text_IO.Put_Line ("Adding board");
                        State := Read_Board_1;
                     end if;
                  end case;

               end;
            end loop;

            declare
               First : constant Number_Index := Number_Index'First;
               Last  : constant Number_Vectors.Extended_Index
                 := Number_Vectors.Last_Index (Numbers);
               N     : Nat32;
            begin
               Outer_Loop : for I in Number_Index range First .. Last loop
                  N := Number_Vectors.Element (Numbers, I);
                  declare
                     B     : Board_Type;
                     Bord : constant Board_Vs.Read_Only_Element_Array_Ptr
                       := Board_Vs.Array_View (Container => Boards);
                  begin
                     for J in Bord'Range loop
                        if Bord (J).Exists then
                           B := Bord (J).Value;
                           Mark (Board        => B,
                                 Searched_For => N);
                           Board_Vs.Replace_Element (Boards.all, J, B);
                        else
                           exit;
                        end if;
                     end loop;
                  end;

                  declare
                     Fb : Board_Vs.Vector;  -- Boards to filter out
                  begin
                     declare
                        B     : Board_Type;
                        Bord : constant Board_Vs.Read_Only_Element_Array_Ptr
                          := Board_Vs.Array_View (Container => Boards);
                     begin
                        for J in Bord'Range loop
                           if Bord (J).Exists then
                              B := Bord (J).Value;
                              if Is_Winner (Board => B) then
                                 Board_Vs.Append
                                   (Container => Fb,
                                    New_Item  => B);
                                 Put_Line (B);
                                 Text_IO.New_Line;

                                 if Board_Vs.Length (Boards.all) = 1 then
                                    Text_IO.Put_Line ("Last Winner found!");

                                    Is_Winner_Found := True;
                                    Wb := Bord (1).Value;
                                    Wn := N;
                                    Put_Line (Wb);
                                    exit Outer_Loop;
                                 end if;
                              end if;
                           else
                              exit;
                           end if;
                        end loop;
                     end;

                     --   declare
                     --      First : constant Board_Index := Board_Index'First;
                     --      Last  : constant Board_Vs.Extended_Index
                     --        := Board_Vs.Last_Index (Fb);
                     --      D     : Board_Type;
                     --   begin
                     --      for J in Board_Index range First .. Last loop
                     --         D := Board_Vs.Element (Parser.Boards, J);
                     --         Board_Vs.Delete (Parser.Boards, D);
                     --      end loop;
                     --   end;
                  end;
               end loop Outer_Loop;
            end;

            if Is_Winner_Found then
               Text_IO.Put_Line ("Wn" & Nat32'Image (Wn));
               Text_IO.Put_Line
                 ("unmarked " & Nat32'Image
                    (Sum_Of_All_Unmarked_Numbers (Wb)));
               Text_IO.Put_Line
                 (Nat32'Image (Sum_Of_All_Unmarked_Numbers (Wb) * Wn));
               --  Answer: 2568
            end if;
         end Run;

      end Part_Two;

   end Day_4;

   package body Day_5 is

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
            Text_IO.Put_Line (R);
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

      type X_Type is new Natural range 0 .. 999;
      type Y_Type is new Natural range 0 .. 999;

      type Board_Type is array (X_Type, Y_Type) of Board_Cell;

      type Board_Ptr is access Board_Type;
      for  Board_Ptr'Storage_Pool use Pool;

      File_Name : aliased constant String := "day_05_input.txt";

      package body Part_One is

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);
            Board : Board_Ptr := new Board_Type'
              (others => (others => (Count => 0)));
            Count     : Natural := 0;
         begin
            Text_File_Parser.Read_From_File (Parser);

            for Li in Parser.Line'Range loop
               declare
                  Line : constant String := Parser.Line (Li).all;
                  Int : constant Index_Interval_Array
                    := Get_Intervals (Parser.Line (Li).all, ' ');
               begin
                  if not (Line (Int (2).First .. Int (2).Last) = "->") then
                     Text_IO.Put_Line
                       ("Unexpected: " & Line (Int (2).First .. Int (2).Last));
                  end if;

                  declare
                     Start_Point : constant String
                       := Line (Int (1).First .. Int (1).Last);
                     End_Point   : constant String
                       := Line (Int (3).First .. Int (3).Last);
                     Ls          : constant Index_Interval_Array  --  Line start
                       := Get_Intervals (Start_Point, ',');
                     Le          : constant Index_Interval_Array  --  Line end
                       := Get_Intervals (End_Point, ',');
                  begin
                     if Ls'Length /= 2 then
                        Text_IO.Put_Line ("Start point error:" & Line);
                     end if;
                     if Le'Length /= 2 then
                        Text_IO.Put_Line ("Start end error:" & Line);
                     end if;

                     declare
                        X0 : constant X_Type := X_Type'Value
                          (Start_Point (Ls (1).First .. Ls (1).Last));

                        Y0 : constant Y_Type := Y_Type'Value
                          (Start_Point (Ls (2).First .. Ls (2).Last));

                        X1 : constant X_Type := X_Type'Value
                          (End_Point (Le (1).First .. Le (1).Last));

                        Y1 : constant Y_Type := Y_Type'Value
                          (End_Point (Le (2).First .. Le (2).Last));
                     begin
                        if X0 = X1 then
                           if Y0 <= Y1 then
                              for I in Y_Type range Y0 .. Y1 loop
                                 Board (X0, I).Count
                                   := Board (X0, I).Count + 1;
                              end loop;
                           else
                              for I in Y_Type range Y1 .. Y0 loop
                                 Board (X0, I).Count
                                   := Board (X0, I).Count + 1;
                              end loop;
                           end if;
                        elsif Y0 = Y1 then
                           if X0 <= X1 then
                              for I in X_Type range X0 .. X1 loop
                                 Board (I, Y0).Count
                                   := Board (I, Y0).Count + 1;
                              end loop;
                           else
                              for I in X_Type range X1 .. X0 loop
                                 Board (I, Y0).Count
                                   := Board (I, Y0).Count + 1;
                              end loop;
                           end if;
                        else
                           Text_IO.Put_Line ("What to do: " & Line);
                           --  Shoud be ignored for now
                        end if;
                     end;
                  end;
               end;
            end loop;

            for X in X_Type loop
               for Y in Y_Type loop
                  if Board (X, Y).Count > 1 then
                     Count := Count + 1;
                  end if;
               end loop;
            end loop;
            Text_IO.Put_Line (Natural'Image (Count));
         end Run;

      end Part_One;

      package body Part_Two is

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);
            Board : Board_Ptr := new Board_Type'
              (others => (others => (Count => 0)));
            Count     : Natural := 0;
         begin
            Text_File_Parser.Read_From_File (Parser);
            for Li in Parser.Line'Range loop
               declare
                  Line : constant String := Parser.Line (Li).all;
                  Int : constant Index_Interval_Array
                    := Get_Intervals (Line, ' ');
               begin
                  if not (Line (Int (2).First .. Int (2).Last) = "->") then
                     Text_IO.Put_Line
                       ("Unexpected: " & Line (Int (2).First .. Int (2).Last));
                  end if;

                  declare
                     Start_Point : constant String
                       := Line (Int (1).First .. Int (1).Last);
                     End_Point   : constant String
                       := Line (Int (3).First .. Int (3).Last);
                     Ls          : constant Index_Interval_Array  --  Line start
                       := Get_Intervals (Start_Point, ',');
                     Le          : constant Index_Interval_Array  --  Line end
                       := Get_Intervals (End_Point, ',');
                  begin
                     if Ls'Length /= 2 then
                        Text_IO.Put_Line ("Start point error:" & Line);
                     end if;
                     if Le'Length /= 2 then
                        Text_IO.Put_Line ("Start end error:" & Line);
                     end if;

                     declare
                        X0 : constant X_Type := X_Type'Value
                          (Start_Point (Ls (1).First .. Ls (1).Last));
                        Y0 : constant Y_Type := Y_Type'Value
                          (Start_Point (Ls (2).First .. Ls (2).Last));
                        X1 : constant X_Type := X_Type'Value
                          (End_Point (Le (1).First .. Le (1).Last));
                        Y1 : constant Y_Type := Y_Type'Value
                          (End_Point (Le (2).First .. Le (2).Last));
                     begin
                        if X0 = X1 then
                           if Y0 <= Y1 then
                              for I in Y_Type range Y0 .. Y1 loop
                                 Board (X0, I).Count
                                   := Board (X0, I).Count + 1;
                              end loop;
                           else
                              for I in Y_Type range Y1 .. Y0 loop
                                 Board (X0, I).Count
                                   := Board (X0, I).Count + 1;
                              end loop;
                           end if;
                        elsif Y0 = Y1 then
                           if X0 <= X1 then
                              for I in X_Type range X0 .. X1 loop
                                 Board (I, Y0).Count
                                   := Board (I, Y0).Count + 1;
                              end loop;
                           else
                              for I in X_Type range X1 .. X0 loop
                                 Board (I, Y0).Count
                                   := Board (I, Y0).Count + 1;
                              end loop;
                           end if;
                        else
                           if X0 <= X1 then
                              declare
                                 Y : Y_Type := Y0;
                              begin
                                 for I in X_Type range X0 .. X1 loop
                                    Board (I, Y).Count
                                      := Board (I, Y).Count + 1;

                                    if I = X1 and then not (Y = Y1) then
                                       Text_IO.Put_Line
                                         ("0. End point not correct:" & Line);
                                    end if;

                                    --  Text_IO.Put_Line ("X" & I'Img & ",Y" & Y'Img);
                                    if Y0 <= Y1 then
                                       Y := Y + 1;
                                    else
                                       Y := Y - 1;
                                    end if;
                                 end loop;
                              end;
                           else
                              declare
                                 Y : Y_Type := Y1;
                              begin
                                 for I in X_Type range X1 .. X0 loop
                                    Board (I, Y).Count
                                      := Board (I, Y).Count + 1;
                                    if I = X0 and then not (Y = Y0) then
                                       Text_IO.Put_Line
                                         ("1. End point not correct:" & Line);
                                       --  Text_IO.Put_Line (I'Img & "," & Y'Img);
                                    end if;

                                    if Y1 <= Y0 then
                                       Y := Y + 1;
                                    else
                                       Y := Y - 1;
                                    end if;
                                 end loop;
                              end;
                           end if;
                        end if;
                     end;
                  end;
               end;
            end loop;

            for X in X_Type loop
               for Y in Y_Type loop
                  if Board (X, Y).Count > 1 then
                     Count := Count + 1;
                  end if;
               end loop;
            end loop;
            Text_IO.Put_Line (Natural'Image (Count));
         end Run;

      end Part_Two;

   end Day_5;

   type Substring_Array_Ptr is access Latin_1.Substring_Array;
   for Substring_Array_Ptr'Storage_Pool use Pool;

   package body Day_6 is

      File_Name : aliased constant String := "day_06_input.txt";

      package body Part_One is

         type Fish_Timer_Value is range 0 .. 8;

         type Fish_Count is new Nat32 range 0 .. Nat32'Last;
         --  Specifies a number of cray-fish.

         type Fish_Count_Array is array (Fish_Timer_Value) of Fish_Count;
         --  This type stores the number of cray-fish with a given
         --  timer count down value,

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);

            Sum : Fish_Count := 0;

            Fish : Fish_Count_Array := (others => 0);
         begin
            Text_File_Parser.Read_From_File (Parser);
            declare
               Int : Latin_1.Substring_Array
                 := Latin_1.Split (Parser.Line (1).all, ',');
            begin
               for I in Int'Range loop
                  declare
                     Timer : constant Fish_Timer_Value
                       := Fish_Timer_Value'Value
                         (Parser.Line (1).all (Int (I).First .. Int (I).Last));
                  begin
                     Fish (Timer) := Fish (Timer) + 1;
                  end;
               end loop;
            end;

            for I in 1 .. 80 loop
               declare
                  Temp : Fish_Count;
               begin
                  Temp := Fish (0);
                  for I in Fish_Timer_Value range 1 .. 8 loop
                     Fish (I - 1) := Fish (I);
                  end loop;
                  Fish (8) := Temp;
                  Fish (6) := Fish (6) + Temp;
               end;
            end loop;

            for I in Fish'Range loop
               Sum := Sum + Fish (I);
            end loop;
            Text_IO.Put_Line ("Answer: " & Fish_Count'Image (Sum));
         end Run;

      end Part_One;

      package body Part_Two is

         package Big_Int_Holder is

            type Holder_Type is limited private;

            procedure Set_Int (Holder : in out Holder_Type;
                               Value  : Big_Integers.Multi_Int);

            procedure Set_Int (Holder : in out Holder_Type;
                               Value  : Holder_Type);

            function Int (Holder : Holder_Type)
                          return Big_Integers.Multi_Int;

         private

            type Item_Ptr is access Big_Integers.Multi_Int;

            type Holder_Type is new Limited_Controlled with record
               Item : Item_Ptr;
            end record;

            procedure Finalize (Holder : in out Holder_Type);

         end Big_Int_Holder;

         package body Big_Int_Holder is

            procedure Free is new Ada.Unchecked_Deallocation
              (Object => Big_Integers.Multi_Int,
               Name   => Item_Ptr);

            procedure Set_Int (Holder : in out Holder_Type;
                               Value  : Big_Integers.Multi_Int) is
            begin
               Free (Holder.Item);
               Holder.Item := new Big_Integers.Multi_Int'(Value);
            end Set_Int;

            procedure Set_Int (Holder : in out Holder_Type;
                               Value  : Holder_Type) is
            begin
               if Holder.Item /= null then
                  Free (Holder.Item);
               end if;
               Holder.Item := new Big_Integers.Multi_Int'(Int (Value));
            end Set_Int;

            function Int (Holder : Holder_Type)
                          return Big_Integers.Multi_Int is
            begin
               return Holder.Item.all;
            end Int;

            procedure Finalize (Holder : in out Holder_Type) is
            begin
               Free (Holder.Item);
            end Finalize;

         end Big_Int_Holder;

         type Fish_Timer_Value is range 0 .. 8;

         type Fish_Count_Array is array
           (Fish_Timer_Value) of Big_Int_Holder.Holder_Type;
         --  This type stores the number of cray-fish with a given
         --  timer count down value,

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);

            Sum : Big_Int_Holder.Holder_Type;

            Fish : Fish_Count_Array; --  := (others => Big_Integers.IO.Val ("0"));
         begin
            Text_File_Parser.Read_From_File (Parser);
            Big_Int_Holder.Set_Int (Holder => Sum,
                                    Value  => Big_Integers.IO.Val ("0"));
            for I in Fish'Range loop
               Big_Int_Holder.Set_Int (Holder => Fish (I),
                                       Value  => Big_Integers.IO.Val ("0"));
            end loop;

            declare
               Int : Latin_1.Substring_Array
                 := Latin_1.Split (Parser.Line (1).all, ',');
            begin
               for I in Int'Range loop
                  declare
                     Timer : constant Fish_Timer_Value
                       := Fish_Timer_Value'Value
                         (Parser.Line (1).all (Int (I).First .. Int (I).Last));
                  begin
                     Big_Int_Holder.Set_Int
                       (Holder => Fish (Timer),
                        Value  => Big_Int_Holder.Int (Fish (Timer)) + 1);
                  end;
               end loop;
            end;

            for I in 1 .. 256 loop
               declare
                  Temp : Big_Int_Holder.Holder_Type;
               begin
                  Big_Int_Holder.Set_Int (Holder => Temp,
                                          Value  => Fish (0));
                  for I in Fish_Timer_Value range 1 .. 8 loop
                     Big_Int_Holder.Set_Int (Holder => Fish (I - 1),
                                             Value  => Fish (I));
                  end loop;
                  Big_Int_Holder.Set_Int (Holder => Fish (8),
                                          Value  => Temp);
                  Big_Int_Holder.Set_Int
                    (Holder => Fish (6),
                     Value  =>
                       Big_Int_Holder.Int (Fish (6)) +
                         Big_Int_Holder.Int (Temp));
               end;
            end loop;

            for I in Fish'Range loop
               Big_Int_Holder.Set_Int
                 (Holder => Sum,
                  Value  =>
                    Big_Int_Holder.Int (Sum) + Big_Int_Holder.Int (Fish (I)));
            end loop;
            Text_IO.Put_Line
              ("Answer: " & Big_Integers.IO.Str (Big_Int_Holder.Int (Sum)));
         end Run;

      end Part_Two;

   end Day_6;

   package body Day_7 is

      File_Name : aliased constant String := "day_07_input.txt";

      type Crab_Position is new Nat32;

      type Crab_Position_Array is array (Pos32 range <>) of Crab_Position;

      type Crab_Position_Array_Ptr is access Crab_Position_Array;
      for  Crab_Position_Array_Ptr'Storage_Pool use Pool;

      type Cost_Array is array (Crab_Position range <>) of Nat32;
      --  The index in this array represents the horizontal position that all
      --  crabs are to be moved to and the component value is the cost
      --  of moving the crabs to the position. This array is used to find
      --  the index/crab position with the minimum cost.

      type Cost_Array_Ptr is access Cost_Array;
      for  Cost_Array_Ptr'Storage_Pool use Pool;

      function Minimum_Value_In_Array (Cp : Crab_Position_Array)
                                       return Crab_Position is
         Result : Crab_Position := Cp (Cp'First);
      begin
         for I in Cp'Range loop
            if Cp (I) < Result then
               Result := Cp (I);
            end if;
         end loop;
         return Result;
      end Minimum_Value_In_Array;

      function Minimum_Value_In_Array (C : Cost_Array) return Nat32 is
         Result : Nat32 := C (C'First);
      begin
         for I in C'Range loop
            if C (I) < Result then
               Result := C (I);
            end if;
         end loop;
         return Result;
      end Minimum_Value_In_Array;

      function Maximum_Value_In_Array (Cp : Crab_Position_Array)
                                       return Crab_Position is
         Result : Crab_Position := Cp (Cp'First);
      begin
         for I in Cp'Range loop
            if Cp (I) > Result then
               Result := Cp (I);
            end if;
         end loop;
         return Result;
      end Maximum_Value_In_Array;

      package body Part_One is

         function Cost (Cp  : Crab_Position_Array;
                        Pos : Crab_Position) return Nat32 is
            Result : Nat32 := 0;
         begin
            for I in Cp'Range loop
               Result := Result + Nat32 (abs (Cp (I) - Pos));
            end loop;
            return Result;
         end Cost;

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);

            Ss : Substring_Array_Ptr;
            Cp : Crab_Position_Array_Ptr;
            C  : Cost_Array_Ptr;

            Min : Crab_Position;
            Max : Crab_Position;
         begin
            Text_File_Parser.Read_From_File (Parser);

            Ss := new Latin_1.Substring_Array'
              (Latin_1.Split (Parser.Line (1).all, ','));
            Cp := new Crab_Position_Array (1 .. Ss'Last);

            for I in Ss'Range loop
               Cp (I) := Crab_Position'Value
                 (Parser.Line (1) (Ss (I).First .. Ss (I).Last));
            end loop;

            Min := Minimum_Value_In_Array (Cp.all);
            Max := Maximum_Value_In_Array (Cp.all);
            C := new Cost_Array (Min .. Max);

            for I in C'Range loop
               C (I) := Cost (Cp.all, I);
            end loop;

            Text_IO.Put ("Answer: ");
            Nat32_IO.Put (Item => Minimum_Value_In_Array (C.all), Width => 0);
            Text_IO.New_Line;
         end Run;

      end Part_One;

      package body Part_Two is

         function Cost (Cp  : Crab_Position_Array;
                        Pos : Crab_Position) return Nat32 is
            Result : Nat32 := 0;
            D : Nat32;  --  D = Distance
         begin
            for I in Cp'Range loop
               D := Nat32 (abs (Cp (I) - Pos));
               Result := Result + (D * (D + 1) / 2);
            end loop;
            return Result;
         end Cost;

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);
            Ss     : Substring_Array_Ptr;
            Cp     : Crab_Position_Array_Ptr;
            C      : Cost_Array_Ptr;

            Min : Crab_Position;
            Max : Crab_Position;
         begin
            Text_File_Parser.Read_From_File (Parser);

            Ss := new Latin_1.Substring_Array'
              (Latin_1.Split (Parser.Line (1).all, ','));
            Cp := new Crab_Position_Array (1 .. Ss'Last);

            for I in Ss'Range loop
               Cp (I) := Crab_Position'Value
                 (Parser.Line (1) (Ss (I).First .. Ss (I).Last));
            end loop;

            Min := Minimum_Value_In_Array (Cp.all);
            Max := Maximum_Value_In_Array (Cp.all);
            C := new Cost_Array (Min .. Max);

            for Pos in C'Range loop
               C (Pos) := Cost (Cp.all, Pos);
            end loop;

            Text_IO.Put ("Answer: ");
            Nat32_IO.Put (Item  => Minimum_Value_In_Array (C.all),
                          Width => 0);
            Text_IO.New_Line;
         end Run;

      end Part_Two;

   end Day_7;

   package body Day_8 is

      File_Name : aliased constant String := "day_08_input.txt";

      type Segment_Type is (A, B, C, D, E, F, G);

      type Segment_Array is array (Positive range <>) of Segment_Type;

      type Segment_Array_Ptr is access Segment_Array;
      for Segment_Array_Ptr'Storage_Pool use Pool;

      type Number_Type is range 0 .. 9;

      type Number_Array is array (Number_Type) of Number_Type;

      type Num_Map_Array is array (Number_Type) of Segment_Array_Ptr;

      function "=" (L, R : Segment_Array) return Boolean is
      begin
         if L'Length = R'Length then
            for I in L'Range loop
               declare
                  Exists : Boolean := False;
               begin
                  for J in R'Range loop
                     if R (J) = L (I) then
                        Exists := True;
                        exit;
                     end if;
                  end loop;
                  if not Exists then
                     return False;
                  end if;
               end;
            end loop;

            for I in R'Range loop
               declare
                  Exists : Boolean := False;
               begin
                  for J in L'Range loop
                     if L (J) = R (I) then
                        Exists := True;
                        exit;
                     end if;
                  end loop;
                  if not Exists then
                     return False;
                  end if;
               end;
            end loop;

            return True;
         else
            return False;
         end if;
      end "=";

      function "+" (Segment : Segment_Array) return String is
         Last : Natural := 0;
      begin
         for I in Segment'Range loop
            declare
               Text :  constant String := Segment_Type'Image (Segment (I));
            begin
               Last := Last + Text'Length;
            end;
         end loop;

         declare
            Result : String (1 .. Last);
         begin
            Last := 0;
            for I in Segment'Range loop
               declare
                  Text :  constant String := Segment_Type'Image (Segment (I));
               begin
                  Result (Last + 1 .. Last + Text'Length) := Text;
                  Last := Last + Text'Length;
               end;
            end loop;
            return Result;
         end;
      end "+";

      function Number_Image (Num_Map : Num_Map_Array;
                             Segment : Segment_Array) return String is
      begin
         for I in Num_Map'Range loop
            if Num_Map (I).all = Segment then
               return Number_Type'Image (I);
            end if;
         end loop;
         raise Constraint_Error with +Segment;
      end Number_Image;

      function Number_Of (Num_Map : Num_Map_Array;
                          Segment : Segment_Array) return Number_Type is
      begin
         for I in Num_Map'Range loop
            if Num_Map (I).all = Segment then
               return I;
            end if;
         end loop;
         raise Constraint_Error with +Segment;
      end Number_Of;

      package body Part_One is

         --  Example of line to parse:
         --  cfb gcfbea bacgfde ebadcf gdfce | gbfdae gaefcb cebdf
         function Line_Value (Line : String) return Nat32 is
            Value : Nat32 := 0;
            Vl : Latin_1.Substring_Array := Latin_1.Split (Line, '|');
            --  Vl = Verticla Line

            First_Part : constant String
              := Trim (Line (Vl (1).First .. Vl (1).Last));
            Second_Part : constant String
              := Trim (Line (Vl (2).First .. Vl (2).Last));
            --  FInt   : Latin_1.Index_Interval_Array
            --    := Latin_1.Split (First_Part, ' ');
            SInt : Latin_1.Substring_Array
              := Latin_1.Split (Second_Part, ' ');
         begin
            Text_IO.Put_Line ("F " & First_Part);
            Text_IO.Put_Line ("S " & Second_Part);
            for J in SInt'Range loop
               case Second_Part (Sint (J).First .. Sint (J).Last)'Length is
                  when 2 | 4 | 3 | 7 => Value := Value + 1;
                  when others        => null;
               end case;
            end loop;
            return Value;
         end Line_Value;

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);

            Num_Map : constant Num_Map_Array
              := (0 => new Segment_Array'(A, B, C, D, E, F, G),
                  1 => new Segment_Array'(C, F),
                  2 => new Segment_Array'(A, C, D, E, G),
                  3 => new Segment_Array'(A, C, D, F, G),
                  4 => new Segment_Array'(B, C, D, F),
                  5 => new Segment_Array'(A, B, D, F, G),
                  6 => new Segment_Array'(A, B, D, E, F, G),
                  7 => new Segment_Array'(A, C, F),
                  8 => new Segment_Array'(A, B, C, D, E, F, G),
                  9 => new Segment_Array'(A, B, C, D, F, G));

            Count : Nat32 := 0;
         begin
            Text_File_Parser.Read_From_File (Parser);

            declare
               A1 : Segment_Array := (A, B);
               A2 : Segment_Array := (B, A);
            begin
               if A1 /= A2 then
                  Text_IO.Put_Line ("Error: Not equal" & (+A1));
                  --  This should not be seen on standard out.
               end if;
               Text_IO.Put_Line (+A1); --  Checking String representation
               --  of segments are correct.
            end;

            for I in Parser.Line'Range loop
               Count := Count + Line_Value (Parser.Line (I).all);
            end loop;
            Text_IO.Put ("Answer:" & Nat32'Image (Count));
         end Run;

      end Part_One;

      package body Part_Two is

         type My_Vector is record
            My_I : aliased Integer := 6;
         end record;

         type I_Ref_Type is access constant Integer;

         function I (V : access My_Vector) return I_Ref_Type is
         begin
            return V.My_I'Access;
         end I;

         type My_Vector_Ptr is access My_Vector;
         for My_Vector_Ptr'Storage_Pool use Pool;
         --
         --           type Wanted_Array is array (Positive range <>) of Segment_Array_Ptr;
         --
         --           package Start_Vs is new Ada.Containers.Vectors
         --             (Index_Type   => Positive,
         --              Element_Type => Segment_Type);
         --
         --           type Solution_Map is array (Segment_Type) of Segment_Type;
         --
         --           Num_Map : constant Num_Map_Array
         --             := (0 => new Segment_Array'(A, B, C, E, F, G),
         --                 1 => new Segment_Array'(C, F),
         --                 2 => new Segment_Array'(A, C, D, E, G),
         --                 3 => new Segment_Array'(A, C, D, F, G),
         --                 4 => new Segment_Array'(B, C, D, F),
         --                 5 => new Segment_Array'(A, B, D, F, G),
         --                 6 => new Segment_Array'(A, B, D, E, F, G),
         --                 7 => new Segment_Array'(A, C, F),
         --                 8 => new Segment_Array'(A, B, C, D, E, F, G),
         --                 9 => new Segment_Array'(A, B, C, D, F, G));
         --
         --           function Is_Number (Segment : Segment_Array) return Boolean is
         --           begin
         --              for I in Num_Map'Range loop
         --                 if Segment = Num_Map (I).all then
         --                    return True;
         --                 end if;
         --              end loop;
         --              return False;
         --           end Is_Number;

         procedure Run is
            --              Parser : Text_File_Parser (File_Name'Access);
            --
            --              Count : Natural := 0;

            V     : aliased My_Vector_Ptr := new My_Vector;
            --              Sum   : Natural := 0;
         begin
            null;
            Text_IO.Put_Line ("I : " & I (V).all'Image);
            --  I (V'Access).all := 3;
            --
            --              Initialize (Parser);
            --
            --              declare
            --                 A1 : Segment_Array := (A, B);
            --                 A2 : Segment_Array := (B, A);
            --              begin
            --                 if A1 /= A2 then
            --                    Text_IO.Put_Line ("Not equal" & (+A1));
            --                 else
            --                    Text_IO.Put_Line ("Equal" & (+A1));
            --                 end if;
            --              end;
            --
            --              for I in Pos32 range Parser.Line'First .. Parser.Line'Last loop
            --                 declare
            --                    VL_Int : Latin_1.Index_Interval_Array
            --                      := Latin_1.Split (Parser.Line (I).all, '|');
            --                    First_Part : constant String
            --                      := Trim
            --                        (Parser.Line (I) (VL_Int (1).First .. VL_Int (1).Last));
            --                    Second_Part : constant String
            --                      := Trim
            --                        (Parser.Line (I) (VL_Int (2).First .. VL_Int (2).Last));
            --                    FInt   : Latin_1.Index_Interval_Array
            --                      := Latin_1.Split (First_Part, ' ');
            --                    SInt   : Latin_1.Index_Interval_Array
            --                      := Latin_1.Split (Second_Part, ' ');
            --
            --                    Input  : Wanted_Array (FInt'Range);
            --                    Wanted : Wanted_Array (SInt'Range);
            --                 begin
            --                    Text_IO.Put_Line ("Fint length: " & Fint'Length'Img);
            --
            --                    --  Text_IO.Put_Line ("F " & First_Part);
            --                    --  Text_IO.Put_Line ("S " & Second_Part);
            --                    for J in SInt'Range loop
            --                       declare
            --                          T               : constant String
            --                            := Second_Part (Sint (J).First .. Sint (J).Last);
            --                          Encoded_Segment : Segment_Array_Ptr
            --                            := new Segment_Array (T'Range);
            --                       begin
            --                          for K in T'Range loop
            --                             Encoded_Segment (K)
            --                               := Segment_Type'Value (String'(1 => T (K)));
            --                          end loop;
            --
            --                          case T'Length is
            --                             when 2 | 4 | 3 | 7 =>
            --                                Count := Count + 1;
            --                             when others => null;
            --                          end case;
            --                          Wanted (J) := Encoded_Segment;
            --                       end;
            --                    end loop;
            --
            --                    for J in FInt'Range loop
            --                       declare
            --                          T               : constant String
            --                            := First_Part (Fint (J).First .. Fint (J).Last);
            --                          Encoded_Segment : Segment_Array_Ptr
            --                            := new Segment_Array (T'Range);
            --                       begin
            --                          for K in T'Range loop
            --                             Encoded_Segment (K)
            --                               := Segment_Type'Value (String'(1 => T (K)));
            --                          end loop;
            --
            --                          Input (J) := Encoded_Segment;
            --                       end;
            --                    end loop;
            --
            --                    for I in Input'Range loop
            --                       Text_IO.Put_Line (+Input (I).all);
            --                    end loop;
            --
            --                    Outer_Loop : for I in 1 .. 7 loop
            --                       for J in 1 .. 6 loop
            --                          for K in 1 .. 5 loop
            --                             for L in 1 .. 4 loop
            --                                for M in 1 .. 3 loop
            --                                   for N in 1 .. 2 loop
            --                                      declare
            --                                         Start          : Start_Vs.Vector;
            --                                         Solution       : Solution_Map;
            --                                         Is_All_Numbers : Boolean := True;
            --                                      begin
            --                                         Start.Append (A);
            --                                         Start.Append (B);
            --                                         Start.Append (C);
            --                                         Start.Append (D);
            --                                         Start.Append (E);
            --                                         Start.Append (F);
            --                                         Start.Append (G);
            --
            --                                         Solution (A) := Start (I);
            --                                         Start.Delete (I);
            --                                         Solution (B) := Start (J);
            --                                         Start.Delete (J);
            --                                         Solution (C) := Start (K);
            --                                         Start.Delete (K);
            --                                         Solution (D) := Start (L);
            --                                         Start.Delete (L);
            --                                         Solution (E) := Start (M);
            --                                         Start.Delete (M);
            --                                         Solution (F) := Start (N);
            --                                         Start.Delete (N);
            --                                         Solution (G) := Start (1);
            --                                         Start.Delete (1);
            --                                         if not Start.Is_Empty then
            --                                            Text_IO.Put_Line ("not empty");
            --                                            --  This is an error
            --                                         end if;
            --
            --                                         for Si in Input'Range loop
            --                                            declare
            --                                               A : Segment_Array (Input (Si).all'Range);
            --                                            begin
            --                                               --  Text_IO.Put_Line ("Start " & Si'Img & " " & (+Input (Si).all) & Input'Length'Img);
            --                                               for Item in A'Range loop
            --                                                  A (Item) := Solution (Input (Si) (Item));
            --                                                  --  Text_IO.Put (Input (Si) (Item)'Img & " -> ");
            --                                                  --  Text_IO.Put_Line (A (Item)'Img);
            --                                               end loop;
            --                                               --  Text_IO.Put_Line ("End " & Is_Number (A)'Img);
            --                                               if not Is_Number (A) then
            --                                                  Is_All_Numbers := False;
            --                                                  exit;
            --                                               end if;
            --                                            end;
            --                                         end loop;
            --
            --                                         if Is_All_Numbers then
            --                                            Text_IO.Put_Line
            --                                              ("Solution found!");
            --
            --                                            declare
            --                                               Factor : Natural := 1;
            --                                            begin
            --                                               for Si in reverse Wanted'Range loop
            --                                                  declare
            --                                                     A : Segment_Array (Wanted (Si).all'Range);
            --                                                  begin
            --                                                     --  Text_IO.Put_Line ("Start " & Si'Img & " " & (+Input (Si).all) & Input'Length'Img);
            --                                                     for Item in A'Range loop
            --                                                        A (Item) := Solution (Wanted (Si) (Item));
            --                                                        --  Text_IO.Put (Input (Si) (Item)'Img & " -> ");
            --                                                        --  Text_IO.Put_Line (A (Item)'Img);
            --                                                     end loop;
            --                                                     --  Text_IO.Put_Line ("End " & Is_Number (A)'Img);
            --                                                     if Is_Number (A) then
            --                                                        declare
            --                                                           Nn : Natural := Natural (Number_Of (Num_Map, A)) * Factor;
            --                                                        begin
            --                                                           Text_IO.Put_Line ("Adding" & Nn'Image);
            --                                                           Sum := Sum + Nn;
            --                                                        end;
            --                                                     else
            --                                                        Text_IO.Put_Line ("not a number " & (+A));
            --                                                     end if;
            --                                                  end;
            --                                                  Factor := Factor * 10;
            --                                               end loop;
            --                                            end;
            --
            --                                            exit Outer_Loop;
            --                                         end if;
            --                                      end;
            --                                   end loop;
            --                                end loop;
            --                             end loop;
            --                          end loop;
            --                       end loop;
            --                    end loop Outer_Loop;
            --                 end;
            --              end loop;
            --              Text_IO.Put_Line ("Answer:" & Sum'Img);
         end Run;

      end Part_Two;

   end Day_8;

   package body Day_9 is

      File_Name : aliased constant String := "day_09_input.txt";

      type Height_Type is range 0 .. 9;

      type X_Type is range 1 .. 100;
      type Y_Type is range 1 .. 100;

      type Height_Array is array (X_Type, Y_Type) of Height_Type;

      function Initialize_Height (Parser : Text_File_Parser.T)
                                  return Height_Array is
         Result : Height_Array;
      begin
         for X in X_Type'Range loop
            for Y in Y_Type'Range loop
               declare
                  C : constant Character
                    := Parser.Line (Pos32 (X)).all (Positive (Y));
               begin
                  Result (X, Y) := Height_Type'Value (String'(1 => C));
               end;
            end loop;
         end loop;
         return Result;
      end Initialize_Height;

      package body Part_One is

         procedure Add_To_Sum (Sum : in out Nat32; Height : Height_Type) is
         begin
            Sum := Sum + Nat32 (Height) + 1;
         end Add_To_Sum;

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);
            Height : Height_Array;

            Sum : Nat32 := 0;
         begin
            Text_File_Parser.Read_From_File (Parser);

            Text_IO.Put_Line ("" & Parser.Line (1).all'Length'Img);
            Text_IO.Put_Line ("Rows" & Parser.Line'Length'Img);

            Height := Initialize_Height (Parser);

            for X in X_Type'Range loop
               for Y in Y_Type'Range loop
                  Text_IO.Put_Line (Height (X, Y)'Img);
                  if X > X_Type'First then
                     if X < X_Type'Last then
                        if Y > Y_Type'First then
                           if Y < Y_Type'Last then
                              if
                                Height (X, Y) < Height (X - 1, Y) and then
                                Height (X, Y) < Height (X, Y + 1) and then
                                Height (X, Y) < Height (X + 1, Y) and then
                                Height (X, Y) < Height (X, Y - 1)
                              then
                                 Add_To_Sum (Sum, Height (X, Y));
                                 Text_IO.Put_Line
                                   ("Found, X" & X'Img & " Y" & Y'Img &
                                      " " & Height (X, Y)'Img);
                              end if;
                           else
                              if
                                Height (X, Y) < Height (X - 1, Y) and then
                                Height (X, Y) < Height (X + 1, Y) and then
                                Height (X, Y) < Height (X, Y - 1)
                              then
                                 Add_To_Sum (Sum, Height (X, Y));
                                 Text_IO.Put_Line
                                   ("Found, X" & X'Img & " Y" & Y'Img &
                                      " " & Height (X, Y)'Img);
                              end if;
                           end if;
                        else
                           if
                             Height (X, Y) < Height (X - 1, Y) and then
                             Height (X, Y) < Height (X, Y + 1) and then
                             Height (X, Y) < Height (X + 1, Y)
                           then
                              Add_To_Sum (Sum, Height (X, Y));
                              Text_IO.Put_Line
                                ("Found, X" & X'Img & " Y" & Y'Img &
                                   " " & Height (X, Y)'Img);
                           end if;
                        end if;
                     else
                        --  X = X'Last
                        if Y > Y_Type'First then
                           if Y < Y_Type'Last then
                              if
                                Height (X, Y) < Height (X - 1, Y) and then
                                Height (X, Y) < Height (X, Y + 1) and then
                                Height (X, Y) < Height (X, Y - 1)
                              then
                                 Add_To_Sum (Sum, Height (X, Y));
                                 Text_IO.Put_Line
                                   ("Found, X" & X'Img & " Y" & Y'Img &
                                      " " & Height (X, Y)'Img);
                              end if;
                           else
                              --  Y = Y'Last
                              if
                                Height (X, Y) < Height (X - 1, Y) and then
                                Height (X, Y) < Height (X, Y - 1)
                              then
                                 Add_To_Sum (Sum, Height (X, Y));
                                 Text_IO.Put_Line
                                   ("Found, X" & X'Img & " Y" & Y'Img &
                                      " " & Height (X, Y)'Img);
                              end if;
                           end if;
                        else
                           --  Y = Y'First
                           if
                             Height (X, Y) < Height (X - 1, Y) and then
                             Height (X, Y) < Height (X, Y + 1)
                           then
                              Add_To_Sum (Sum, Height (X, Y));
                              Text_IO.Put_Line
                                ("Found, X" & X'Img & " Y" & Y'Img &
                                   " " & Height (X, Y)'Img);
                           end if;
                        end if;
                     end if;
                  else
                     --  X = X'First
                     if Y > Y_Type'First then
                        if Y < Y_Type'Last then

                           Text_IO.Put_Line ("H (X, Y)" & Height (X, Y)'Img);
                           Text_IO.Put_Line ("H (X, Y + 1)" & Height (X, Y + 1)'Img);
                           Text_IO.Put_Line ("H (X + 1, Y)" & Height (X + 1, Y)'Img);
                           Text_IO.Put_Line ("H (X, Y - 1)" & Height (X, Y - 1)'Img);
                           if
                             Height (X, Y) < Height (X, Y + 1) and then
                             Height (X, Y) < Height (X + 1, Y) and then
                             Height (X, Y) < Height (X, Y - 1)
                           then
                              Add_To_Sum (Sum, Height (X, Y));
                              Text_IO.Put_Line
                                ("Found, X" & X'Img & " Y" & Y'Img &
                                   " " & Height (X, Y)'Img);
                           end if;
                        else
                           if
                             Height (X, Y) < Height (X + 1, Y) and then
                             Height (X, Y) < Height (X, Y - 1)
                           then
                              Add_To_Sum (Sum, Height (X, Y));
                              Text_IO.Put_Line
                                ("Found, X" & X'Img & " Y" & Y'Img &
                                   " " & Height (X, Y)'Img);
                           end if;
                        end if;
                     else
                        --  Y = Y'First
                        if
                          Height (X, Y) < Height (X, Y + 1) and then
                          Height (X, Y) < Height (X + 1, Y)
                        then
                           Add_To_Sum (Sum, Height (X, Y));
                           Text_IO.Put_Line
                             ("Found, X" & X'Img & " Y" & Y'Img &
                                " " & Height (X, Y)'Img);
                        end if;
                     end if;
                  end if;
               end loop;
            end loop;
            Text_IO.Put_Line ("Answer: " & Sum'Img);
         end Run;

      end Part_One;

      package body Part_Two is

         type Basin_Type is record
            X    : X_Type;
            Y    : Y_Type;
            Size : Pos32;
         end record;

         function "<" (L, R : Basin_Type) return Boolean is
         begin
            return L.Size < R.Size;
         end "<";

         type Basin_Array is array (Positive range <>) of Basin_Type;

         type Position_Type is record
            X : X_Type;
            Y : Y_Type;
         end record;

         type Position_Index is new Pos32 range
           1 ..  Pos32 (X_Type'Last * X_Type'Last);

         package Position_Vs is new Stdh.Unbounded_Bounded_Vectors
           (Index_Type   => Position_Index,
            Element_Type => Position_Type);

         procedure Sort is new Ada.Containers.Generic_Array_Sort
           (Index_Type   => Positive,
            Element_Type => Basin_Type,
            Array_Type   => Basin_Array,
            "<"          => "<");

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);
            Height : Height_Array;

            Low_Point_Count : Natural := 0;

            Result : Pos32 := 1;
         begin
            Text_File_Parser.Read_From_File (Parser);

            Text_IO.Put_Line (Parser.Line (1).all'Length'Img);
            Text_IO.Put_Line ("Rows" & Parser.Line'Length'Img);

            Height := Initialize_Height (Parser);

            for X in X_Type'Range loop
               for Y in Y_Type'Range loop
                  --  Text_IO.Put_Line (Height (X, Y)'Img);
                  if X > X_Type'First then
                     if X < X_Type'Last then
                        if Y > Y_Type'First then
                           if Y < Y_Type'Last then
                              if
                                Height (X, Y) < Height (X - 1, Y) and then
                                Height (X, Y) < Height (X, Y + 1) and then
                                Height (X, Y) < Height (X + 1, Y) and then
                                Height (X, Y) < Height (X, Y - 1)
                              then
                                 Low_Point_Count := Low_Point_Count + 1;
                              end if;
                           else
                              if
                                Height (X, Y) < Height (X - 1, Y) and then
                                Height (X, Y) < Height (X + 1, Y) and then
                                Height (X, Y) < Height (X, Y - 1)
                              then
                                 Low_Point_Count := Low_Point_Count + 1;
                              end if;
                           end if;
                        else
                           if
                             Height (X, Y) < Height (X - 1, Y) and then
                             Height (X, Y) < Height (X, Y + 1) and then
                             Height (X, Y) < Height (X + 1, Y)
                           then
                              Low_Point_Count := Low_Point_Count + 1;
                           end if;
                        end if;
                     else
                        --  X = X'Last
                        if Y > Y_Type'First then
                           if Y < Y_Type'Last then
                              if
                                Height (X, Y) < Height (X - 1, Y) and then
                                Height (X, Y) < Height (X, Y + 1) and then
                                Height (X, Y) < Height (X, Y - 1)
                              then
                                 Low_Point_Count := Low_Point_Count + 1;
                              end if;
                           else
                              --  Y = Y'Last
                              if
                                Height (X, Y) < Height (X - 1, Y) and then
                                Height (X, Y) < Height (X, Y - 1)
                              then
                                 Low_Point_Count := Low_Point_Count + 1;
                              end if;
                           end if;
                        else
                           --  Y = Y'First
                           if
                             Height (X, Y) < Height (X - 1, Y) and then
                             Height (X, Y) < Height (X, Y + 1)
                           then
                              Low_Point_Count := Low_Point_Count + 1;
                           end if;
                        end if;
                     end if;
                  else
                     --  X = X'First
                     if Y > Y_Type'First then
                        if Y < Y_Type'Last then
                           if
                             Height (X, Y) < Height (X, Y + 1) and then
                             Height (X, Y) < Height (X + 1, Y) and then
                             Height (X, Y) < Height (X, Y - 1)
                           then
                              Low_Point_Count := Low_Point_Count + 1;
                           end if;
                        else
                           if
                             Height (X, Y) < Height (X + 1, Y) and then
                             Height (X, Y) < Height (X, Y - 1)
                           then
                              Low_Point_Count := Low_Point_Count + 1;
                           end if;
                        end if;
                     else
                        --  Y = Y'First
                        if
                          Height (X, Y) < Height (X, Y + 1) and then
                          Height (X, Y) < Height (X + 1, Y)
                        then
                           Low_Point_Count := Low_Point_Count + 1;
                        end if;
                     end if;
                  end if;
               end loop;
            end loop;

            declare
               Basin : Basin_Array (1 .. Positive (Low_Point_Count));
            begin
               Low_Point_Count := 0;
               for X in X_Type'Range loop
                  for Y in Y_Type'Range loop
                     --  Text_IO.Put_Line (Height (X, Y)'Img);
                     if X > X_Type'First then
                        if X < X_Type'Last then
                           if Y > Y_Type'First then
                              if Y < Y_Type'Last then
                                 if
                                   Height (X, Y) < Height (X - 1, Y) and then
                                   Height (X, Y) < Height (X, Y + 1) and then
                                   Height (X, Y) < Height (X + 1, Y) and then
                                   Height (X, Y) < Height (X, Y - 1)
                                 then
                                    Low_Point_Count := Low_Point_Count + 1;
                                    Basin (Low_Point_Count) := (X    => X,
                                                                Y    => Y,
                                                                Size => 1);
                                 end if;
                              else
                                 if
                                   Height (X, Y) < Height (X - 1, Y) and then
                                   Height (X, Y) < Height (X + 1, Y) and then
                                   Height (X, Y) < Height (X, Y - 1)
                                 then
                                    Low_Point_Count := Low_Point_Count + 1;
                                    Basin (Low_Point_Count) := (X    => X,
                                                                Y    => Y,
                                                                Size => 1);
                                 end if;
                              end if;
                           else
                              if
                                Height (X, Y) < Height (X - 1, Y) and then
                                Height (X, Y) < Height (X, Y + 1) and then
                                Height (X, Y) < Height (X + 1, Y)
                              then
                                 Low_Point_Count := Low_Point_Count + 1;
                                 Basin (Low_Point_Count) := (X    => X,
                                                             Y    => Y,
                                                             Size => 1);
                              end if;
                           end if;
                        else
                           --  X = X'Last
                           if Y > Y_Type'First then
                              if Y < Y_Type'Last then
                                 if
                                   Height (X, Y) < Height (X - 1, Y) and then
                                   Height (X, Y) < Height (X, Y + 1) and then
                                   Height (X, Y) < Height (X, Y - 1)
                                 then
                                    Low_Point_Count := Low_Point_Count + 1;
                                    Basin (Low_Point_Count) := (X    => X,
                                                                Y    => Y,
                                                                Size => 1);
                                 end if;
                              else
                                 --  Y = Y'Last
                                 if
                                   Height (X, Y) < Height (X - 1, Y) and then
                                   Height (X, Y) < Height (X, Y - 1)
                                 then
                                    Low_Point_Count := Low_Point_Count + 1;
                                    Basin (Low_Point_Count) := (X    => X,
                                                                Y    => Y,
                                                                Size => 1);
                                 end if;
                              end if;
                           else
                              --  Y = Y'First
                              if
                                Height (X, Y) < Height (X - 1, Y) and then
                                Height (X, Y) < Height (X, Y + 1)
                              then
                                 Low_Point_Count := Low_Point_Count + 1;
                                 Basin (Low_Point_Count) := (X    => X,
                                                             Y    => Y,
                                                             Size => 1);
                              end if;
                           end if;
                        end if;
                     else
                        --  X = X'First
                        if Y > Y_Type'First then
                           if Y < Y_Type'Last then
                              if
                                Height (X, Y) < Height (X, Y + 1) and then
                                Height (X, Y) < Height (X + 1, Y) and then
                                Height (X, Y) < Height (X, Y - 1)
                              then
                                 Low_Point_Count := Low_Point_Count + 1;
                                 Basin (Low_Point_Count) := (X    => X,
                                                             Y    => Y,
                                                             Size => 1);
                              end if;
                           else
                              if
                                Height (X, Y) < Height (X + 1, Y) and then
                                Height (X, Y) < Height (X, Y - 1)
                              then
                                 Low_Point_Count := Low_Point_Count + 1;
                                 Basin (Low_Point_Count) := (X    => X,
                                                             Y    => Y,
                                                             Size => 1);
                              end if;
                           end if;
                        else
                           --  Y = Y'First
                           if
                             Height (X, Y) < Height (X, Y + 1) and then
                             Height (X, Y) < Height (X + 1, Y)
                           then
                              Low_Point_Count := Low_Point_Count + 1;
                              Basin (Low_Point_Count) := (X    => X,
                                                          Y    => Y,
                                                          Size => 1);
                           end if;
                        end if;
                     end if;
                  end loop;
               end loop;
               for I in Basin'Range loop
                  declare
                     Position : Position_Vs.Vector;
                     New_P : Position_Vs.Vector;
                     NX : X_Type;
                     NY : Y_Type;
                  begin
                     Position.Append ((X => Basin (I).X, Y => Basin (I).Y));

                     loop
                        New_P.Clear;
                        declare
                           First : constant Position_Index
                             := Position_Index'First;
                           Last  : constant Position_Vs.Extended_Index
                             := Position_Vs.Last_Index (Position);
                           P : Position_Type;
                        begin
                           for Pi in Position_Index range First .. Last loop
                              P := Position_Vs.Element (Position, Pi);
                              Text_IO.Put_Line
                                ("Iterate over " & P.X'Img & " " & P.Y'Img);
                              if P.X > X_Type'First then
                                 if P.X < X_Type'Last then
                                    if P.Y > Y_Type'First then
                                       if P.Y < Y_Type'Last then
                                          NX := P.X - 1;
                                          NY := P.Y;
                                          if Height (NX, NY) > Height (P.X, P.Y) then
                                             if Height (NX, NY) < 9 then
                                                New_P.Append ((X => NX,
                                                               Y => NY));
                                             end if;
                                          end if;

                                          NX := P.X;
                                          NY := P.Y + 1;
                                          if Height (NX, NY) > Height (P.X, P.Y) then
                                             if Height (NX, NY) < 9 then
                                                New_P.Append ((X => NX,
                                                               Y => NY));
                                             end if;
                                          end if;

                                          NX := P.X + 1;
                                          NY := P.Y;
                                          if Height (NX, NY) > Height (P.X, P.Y) then
                                             if Height (NX, NY) < 9 then
                                                New_P.Append ((X => NX,
                                                               Y => NY));
                                             end if;
                                          end if;

                                          NX := P.X;
                                          NY := P.Y - 1;
                                          if Height (NX, NY) > Height (P.X, P.Y) then
                                             if Height (NX, NY) < 9 then
                                                New_P.Append ((X => NX,
                                                               Y => NY));
                                             end if;
                                          end if;
                                       else
                                          NX := P.X - 1;
                                          NY := P.Y;
                                          if Height (NX, NY) > Height (P.X, P.Y) then
                                             if Height (NX, NY) < 9 then
                                                New_P.Append ((X => NX,
                                                               Y => NY));
                                             end if;
                                          end if;

                                          NX := P.X + 1;
                                          NY := P.Y;
                                          if Height (NX, NY) > Height (P.X, P.Y) then
                                             if Height (NX, NY) < 9 then
                                                New_P.Append ((X => NX,
                                                               Y => NY));
                                             end if;
                                          end if;

                                          NX := P.X;
                                          NY := P.Y - 1;
                                          if Height (NX, NY) > Height (P.X, P.Y) then
                                             if Height (NX, NY) < 9 then
                                                New_P.Append ((X => NX,
                                                               Y => NY));
                                             end if;
                                          end if;
                                       end if;
                                    else
                                       NX := P.X - 1;
                                       NY := P.Y;
                                       if Height (NX, NY) > Height (P.X, P.Y) then
                                          if Height (NX, NY) < 9 then
                                             New_P.Append ((X => NX,
                                                            Y => NY));
                                          end if;
                                       end if;

                                       NX := P.X;
                                       NY := P.Y + 1;
                                       if Height (NX, NY) > Height (P.X, P.Y) then
                                          if Height (NX, NY) < 9 then
                                             New_P.Append ((X => NX,
                                                            Y => NY));
                                          end if;
                                       end if;

                                       NX := P.X + 1;
                                       NY := P.Y;
                                       if Height (NX, NY) > Height (P.X, P.Y) then
                                          if Height (NX, NY) < 9 then
                                             New_P.Append ((X => NX,
                                                            Y => NY));
                                          end if;
                                       end if;
                                    end if;
                                 else
                                    --  X = X'Last
                                    if P.Y > Y_Type'First then
                                       if P.Y < Y_Type'Last then
                                          NX := P.X - 1;
                                          NY := P.Y;
                                          if Height (NX, NY) > Height (P.X, P.Y) then
                                             if Height (NX, NY) < 9 then
                                                New_P.Append ((X => NX,
                                                               Y => NY));
                                             end if;
                                          end if;

                                          NX := P.X;
                                          NY := P.Y + 1;
                                          if Height (NX, NY) > Height (P.X, P.Y) then
                                             if Height (NX, NY) < 9 then
                                                New_P.Append ((X => NX,
                                                               Y => NY));
                                             end if;
                                          end if;

                                          NX := P.X;
                                          NY := P.Y - 1;
                                          if Height (NX, NY) > Height (P.X, P.Y) then
                                             if Height (NX, NY) < 9 then
                                                New_P.Append ((X => NX,
                                                               Y => NY));
                                             end if;
                                          end if;
                                       else
                                          --  Y = Y'Last
                                          NX := P.X - 1;
                                          NY := P.Y;
                                          if Height (NX, NY) > Height (P.X, P.Y) then
                                             if Height (NX, NY) < 9 then
                                                New_P.Append ((X => NX,
                                                               Y => NY));
                                             end if;
                                          end if;

                                          NX := P.X;
                                          NY := P.Y - 1;
                                          if Height (NX, NY) > Height (P.X, P.Y) then
                                             if Height (NX, NY) < 9 then
                                                New_P.Append ((X => NX,
                                                               Y => NY));
                                             end if;
                                          end if;
                                       end if;
                                    else
                                       --  Y = Y'First
                                       NX := P.X - 1;
                                       NY := P.Y;
                                       if Height (NX, NY) > Height (P.X, P.Y) then
                                          if Height (NX, NY) < 9 then
                                             New_P.Append ((X => NX,
                                                            Y => NY));
                                          end if;
                                       end if;

                                       NX := P.X;
                                       NY := P.Y + 1;
                                       if Height (NX, NY) > Height (P.X, P.Y) then
                                          if Height (NX, NY) < 9 then
                                             New_P.Append ((X => NX,
                                                            Y => NY));
                                          end if;
                                       end if;
                                    end if;
                                 end if;
                              else
                                 --  X = X'First
                                 if P.Y > Y_Type'First then
                                    if P.Y < Y_Type'Last then
                                       NX := P.X;
                                       NY := P.Y + 1;
                                       if Height (NX, NY) > Height (P.X, P.Y) then
                                          if Height (NX, NY) < 9 then
                                             New_P.Append ((X => NX,
                                                            Y => NY));
                                          end if;
                                       end if;

                                       NX := P.X + 1;
                                       NY := P.Y;
                                       if Height (NX, NY) > Height (P.X, P.Y) then
                                          if Height (NX, NY) < 9 then
                                             New_P.Append ((X => NX,
                                                            Y => NY));
                                          end if;
                                       end if;

                                       NX := P.X;
                                       NY := P.Y - 1;
                                       if Height (NX, NY) > Height (P.X, P.Y) then
                                          if Height (NX, NY) < 9 then
                                             New_P.Append ((X => NX,
                                                            Y => NY));
                                          end if;
                                       end if;
                                    else
                                       NX := P.X + 1;
                                       NY := P.Y;
                                       if Height (NX, NY) > Height (P.X, P.Y) then
                                          if Height (NX, NY) < 9 then
                                             New_P.Append ((X => NX,
                                                            Y => NY));
                                          end if;
                                       end if;

                                       NX := P.X;
                                       NY := P.Y - 1;
                                       if Height (NX, NY) > Height (P.X, P.Y) then
                                          if Height (NX, NY) < 9 then
                                             New_P.Append ((X => NX,
                                                            Y => NY));
                                          end if;
                                       end if;
                                    end if;
                                 else
                                    --  Y = Y'First
                                    NX := P.X;
                                    NY := P.Y + 1;
                                    if Height (NX, NY) > Height (P.X, P.Y) then
                                       if Height (NX, NY) < 9 then
                                          New_P.Append ((X => NX,
                                                         Y => NY));
                                       end if;
                                    end if;

                                    NX := P.X + 1;
                                    NY := P.Y;
                                    if Height (NX, NY) > Height (P.X, P.Y) then
                                       if Height (NX, NY) < 9 then
                                          New_P.Append ((X => NX,
                                                         Y => NY));
                                       end if;
                                    end if;
                                 end if;
                              end if;
                           end loop;
                        end;

                        Text_IO.Put_Line ("New length "  & New_P.Length'Img);

                        declare
                           Exists_New_Positions : Boolean := False;
                        begin
                           declare
                              First : constant Position_Index
                                := Position_Index'First;
                              Last  : constant Position_Vs.Extended_Index
                                := Position_Vs.Last_Index (New_P);
                              New_Item : Position_Type;
                           begin
                              for Pi in Position_Index range First .. Last loop
                                 New_Item := Position_Vs.Element (New_P, Pi);
                                 --  for New_Item of New_P loop
                                 Text_IO.Put_Line
                                   ("Will add " & New_Item.X'Img &
                                      " " & New_Item.Y'Img);
                                 if not Position_Vs.Contains (Position, New_Item) then
                                    Position_Vs.Append (Position, New_Item);
                                    Exists_New_Positions := True;
                                 end if;
                              end loop;
                           end;
                           Basin (I).Size := Pos32 (Position.Length);
                           exit when not Exists_New_Positions;
                        end;

                        declare
                           First : constant Position_Index
                             := Position_Index'First;
                              Last  : constant Position_Vs.Extended_Index
                             := Position_Vs.Last_Index (New_P);
                           New_Item : Position_Type;
                        begin
                           for Pi in Position_Index range First .. Last loop
                              New_Item := Position_Vs.Element (New_P, Pi);
                              --  Text_IO.Put_Line
                              --    ("Will add " & New_Item.X'Img & " " & New_Item.Y'Img);
                              if not Position_Vs.Contains (Position, New_Item) then
                                 Position_Vs.Append (Position, New_Item);
                              end if;
                           end loop;
                        end;
                     end loop;
                  end;
               end loop;

               for I in Basin'Range loop
                  Text_IO.Put_Line
                    (Basin (I).X'Img & " " & Basin (I).Y'Img &
                       " has size " & Basin (I).Size'Img);
               end loop;
               Sort (Basin);
               Text_IO.Put_Line ("Sorted");
               for I in Basin'Last - 2 .. Basin'Last loop
                  Text_IO.Put_Line
                    (Basin (I).X'Img & " " & Basin (I).Y'Img &
                       " has size " & Basin (I).Size'Img);
                  Result := Result * Basin (I).Size;
               end loop;
            end;
            Text_IO.Put_Line ("Answer: " & Result'Img);
         end Run;

      end Part_Two;

   end Day_9;

   package body Day_10 is

      File_Name : aliased constant String := "day_10_input.txt";

      type Line_Kind is (Corrupt, Incomplete, Complete);

      package body Part_One is

         package Local_State is

            type T is limited private;

            function Kind (State : in out T;
                           Line  : in     String) return Line_Kind;

            function Calculate_Sum (State : in out T;
                                    Line  : in     String) return Nat32;

         private

            type Expected_Symbol_Index is range 1 .. 128;

            --  Expected Symbol Vectors
            package Exp_Sym_Vecs is new Stda.Pool_Bounded_Vectors
              (Element_Type => Character,
               Index_Type   => Expected_Symbol_Index);

            type Expected_Symbol_Vector_Ptr is access Exp_Sym_Vecs.Vector;
            for Expected_Symbol_Vector_Ptr'Storage_Pool use Pool;

            type T is limited record
               V : Expected_Symbol_Vector_Ptr := new Exp_Sym_Vecs.Vector;
            end record;

         end Local_State;

         package body Local_State is

            function Amount (C : Character) return Nat32 is
            begin
               case C is
                  when ')' => return 3;
                  when ']' => return 57;
                  when '}' => return 1197;
                  when '>' => return 25137;
                  when others =>
                     raise Constraint_Error with "Unexpected character "  & C;
               end case;
            end Amount;

            --  When first illegal character found, the value is the
            --  index of the first illegal character.
            type Optional_Illegal (Is_Found : Boolean := False) is record
               case Is_Found is
                  when True  => Index : Pos32;
                  when False => null;
               end case;
            end record;

            function Kind (State : in out T;
                           Line  : in     String)
                           return Line_Kind
            is
               function Handle_Illegal_Found (I : in Positive)
                                              return Boolean is
               begin
                  Text_IO.Put_Line (":" & Line & "  illegal: " & Line (I) &
                                      " expected " &
                                      Exp_Sym_Vecs.Last_Element (State.V));
                  return True;
               end Handle_Illegal_Found;

               Illegal : Boolean := False;
            begin
               Exp_Sym_Vecs.Clear (State.V);
               --  Does not depend on any previous values

               for I in Line'Range loop
                  case Line (I) is
                     when '(' | '[' | '{' | '<' =>
                        Exp_Sym_Vecs.Append (State.V, Line (I));
                     when ')' =>
                        if Exp_Sym_Vecs.Last_Element (State.V) = '(' then
                           Exp_Sym_Vecs.Delete_Last (State.V);
                        else
                           Illegal := Handle_Illegal_Found (I);
                           exit;
                        end if;
                     when ']' =>
                        if Exp_Sym_Vecs.Last_Element (State.V) = '[' then
                           Exp_Sym_Vecs.Delete_Last (State.V);
                        else
                           Illegal := Handle_Illegal_Found (I);
                           exit;
                        end if;
                     when '}' =>
                        if Exp_Sym_Vecs.Last_Element (State.V) = '{' then
                           Exp_Sym_Vecs.Delete_Last (State.V);
                        else
                           Illegal := Handle_Illegal_Found (I);
                           exit;
                        end if;
                     when '>' =>
                        if Exp_Sym_Vecs.Last_Element (State.V) = '<' then
                           Exp_Sym_Vecs.Delete_Last (State.V);
                        else
                           Illegal := Handle_Illegal_Found (I);
                           exit;
                        end if;
                     when others =>
                        raise Constraint_Error with "Unexpected " & Line (I);
                  end case;
               end loop;
               if Illegal then
                  return Corrupt;
               else
                  if Exp_Sym_Vecs.Length (State.V) > 0 then
                     return Incomplete;
                  else
                     return Complete;
                  end if;
               end if;
            end Kind;

            function Calculate_Sum
              (State : in out T;
               Line  : in     String) return Nat32
            is
               procedure Handle_Illegal_Found (I : Positive) is
               begin
                  Text_IO.Put_Line
                    (":" & Line & "  illegal: " & Line (I) &
                       " expected " & Exp_Sym_Vecs.Last_Element (State.V));
               end Handle_Illegal_Found;

               Result : Nat32 := 0;
               C : Character;
            begin
               Exp_Sym_Vecs.Clear (State.V);
               --  Does not depend on any previous values

               for I in Line'Range loop
                  C := Line (I);
                  case C is
                     when '(' | '[' | '{' | '<' =>
                        Exp_Sym_Vecs.Append (State.V, C);
                     when ')' =>
                        if Exp_Sym_Vecs.Last_Element (State.V) = '(' then
                           Exp_Sym_Vecs.Delete_Last (State.V);
                        else
                           --  first illegal found!
                           Text_IO.Put_Line
                             (":" & Line & "  illegal: " & C &
                                " expected " &
                                Exp_Sym_Vecs.Last_Element (State.V));
                           Result := Amount (C);
                           exit;
                        end if;
                     when ']' =>
                        if Exp_Sym_Vecs.Last_Element (State.V) = '[' then
                           Exp_Sym_Vecs.Delete_Last (State.V);
                        else
                           --  first illegal found!
                           Text_IO.Put_Line
                             (":" & Line & "  illegal: " & C &
                                " expected " &
                                Exp_Sym_Vecs.Last_Element (State.V));
                           Result := Amount (C);
                           exit;
                        end if;
                     when '}' =>
                        if Exp_Sym_Vecs.Last_Element (State.V) = '{' then
                           Exp_Sym_Vecs.Delete_Last (State.V);
                        else
                           --  first illegal found!
                           Text_IO.Put_Line
                             (":" & Line & "  illegal: " & C & " expected " &
                                Exp_Sym_Vecs.Last_Element (State.V));
                           Result := Amount (C);
                           exit;
                        end if;
                     when '>' =>
                        if Exp_Sym_Vecs.Last_Element (State.V) = '<' then
                           Exp_Sym_Vecs.Delete_Last (State.V);
                        else
                           --  first illegal found!
                           Text_IO.Put_Line
                             (":" & Line & "  illegal: " & C & " expected " &
                                Exp_Sym_Vecs.Last_Element (State.V));
                           Result := Amount (C);
                           exit;
                        end if;
                     when others =>
                        raise Constraint_Error with "Unexpected "  & C;
                  end case;
               end loop;
               if Exp_Sym_Vecs.Length (State.V.all) > 0 then
                  Text_IO.Put_Line ("Incomplete " & Line);
                  return Result;
               end if;
               raise Constraint_Error;
            end Calculate_Sum;

         end Local_State;

         type Corrupt_Line_Array is array
           (Pos32 range <>) of Text_File_Parser.String_Ptr;

         type Corrupt_Line_Array_Ptr is access Corrupt_Line_Array;
         for  Corrupt_Line_Array_Ptr'Storage_Pool use Pool;

         procedure Run is
            Parser : Text_File_Parser.T (File_Name'Access);

            Corrupt_Line : Corrupt_Line_Array_Ptr;

            State : Local_State.T;

            Sum : Nat32 := 0;

            Corrupt_Line_Count : Nat32 := 0;

            Index : Pos32;
         begin
            Text_File_Parser.Read_From_File (Parser);
            for L in Parser.Line'Range loop
               if Local_State.Kind (State, Parser.Line (L).all) = Corrupt then
                  Corrupt_Line_Count := Corrupt_Line_Count + 1;
               end if;
            end loop;

            Corrupt_Line := new Corrupt_Line_Array (1 .. Corrupt_Line_Count);

            Index := 1;
            for L in Parser.Line'Range loop
               if Local_State.Kind (State, Parser.Line (L).all) = Corrupt then
                  Corrupt_Line (Index) := Parser.Line (L);
                  Index := Index + 1;
               end if;
            end loop;

            for I in Corrupt_Line'Range loop
               Sum := Sum + Local_State.Calculate_Sum (State,
                                                       Corrupt_Line (I).all);
            end loop;
            Text_IO.Put_Line ("Answer:" & Nat32'Image (Sum));
         end Run;

      end Part_One;

      package body Part_Two is

         procedure Run is
         begin
            null;
         end Run;

      end Part_Two;

   end Day_10;

   package Command_Line renames Ada.Command_Line;

   type Day_Type is new Integer range 1 .. 24;

   procedure Run_Part_One (Day : Day_Type) is
   begin
      case Day is
         when  1 => Advent_Of_Code_2021.Day_1.Part_One.Run;
         when  2 => Advent_Of_Code_2021.Day_2.Part_One.Run;
         when  3 => Advent_Of_Code_2021.Day_3.Part_One.Run;
         when  4 => Advent_Of_Code_2021.Day_4.Part_One.Run;
         when  5 => Advent_Of_Code_2021.Day_5.Part_One.Run;
         when  6 => Advent_Of_Code_2021.Day_6.Part_One.Run;
         when  7 => Advent_Of_Code_2021.Day_7.Part_One.Run;
         when  8 => Advent_Of_Code_2021.Day_8.Part_One.Run;
         when  9 => Advent_Of_Code_2021.Day_9.Part_One.Run;
         when 10 => Advent_Of_Code_2021.Day_10.Part_One.Run;
         when 11 .. 24 =>
            Text_IO.Put_Line ("No puzzle implemented.");
      end case;
   end Run_Part_One;

   procedure Run_Part_Two (Day : Day_Type) is
   begin
      case Day is
         when  1 => Advent_Of_Code_2021.Day_1.Part_Two.Run;
         when  2 => Advent_Of_Code_2021.Day_2.Part_Two.Run;
         when  3 => Advent_Of_Code_2021.Day_3.Part_Two.Run;
         when  4 => Advent_Of_Code_2021.Day_4.Part_Two.Run;
         when  5 => Advent_Of_Code_2021.Day_5.Part_Two.Run;
         when  6 => Advent_Of_Code_2021.Day_6.Part_Two.Run;
         when  7 => Advent_Of_Code_2021.Day_7.Part_Two.Run;
         when  8 => Advent_Of_Code_2021.Day_8.Part_Two.Run;
         when  9 => Advent_Of_Code_2021.Day_9.Part_Two.Run;
         when 10 => Advent_Of_Code_2021.Day_10.Part_Two.Run;
         when 11 .. 24 =>
            Text_IO.Put_Line ("No puzzle implemented.");
      end case;
   end Run_Part_Two;

   procedure Solve_Puzzle is

      procedure Print_Help is
      begin
         Text_IO.Put_Line ("Welcome to the Advent of Code application (aoc)!");
         Text_IO.Put_Line ("It requires at least three arguments.");
         Text_IO.New_Line;
         Text_IO.Put_Line ("1. The year (possible value is only 2021)");
         Text_IO.Put_Line ("2. The day (valid values are 1 to 24)");
         Text_IO.Put_Line ("3. Challenge (valid values are 1 or 2)");
         Text_IO.New_Line;
         Text_IO.Put_Line ("For example, to run the application for challenge");
         Text_IO.Put_Line ("two on the third of December 2021 execute:");
         Text_IO.Put_Line ("aoc 2021 3 2");
      end Print_Help;

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
            if Arg3 = "1" then
               Run_Part_One (Day);
            elsif Arg3 = "2" then
               Run_Part_Two (Day);
            else
               Text_IO.Put_Line ("The challenge has invalid value " & Arg3);
            end if;
         else
            Text_IO.Put_Line ("The year had invalid value " & Arg1);
            Print_Help;
            return;
         end if;
      end;
   end Solve_Puzzle;

   procedure Empty_Pool is new Stdh.Basic_Bounded_Dynamic_Pools.Reset;

   task body Puzzle_Runner is
   begin
      loop
         begin
            select
               accept Run do
                  if
                    Command_Line.Argument_Count = 1 and then
                    Command_Line.Argument (1) = "all"
                  then
                     for Day in Day_Type range 1 .. 10 loop
                        Empty_Pool (Pool);
                        Run_Part_One (Day);
                        Empty_Pool (Pool);
                        Run_Part_Two (Day);
                     end loop;
                  else
                     Empty_Pool (Pool);
                     Solve_Puzzle;
                  end if;
               end Run;
            or
               accept Take_Ownership;
               Stdh.Basic_Bounded_Dynamic_Pools.Set_Owner (Pool);
            or
               terminate;
            end select;
         exception
            when Error : others =>
               Text_IO.Put_Line (Exceptions.Exception_Information (Error));
         end;
      end loop;
   end Puzzle_Runner;

begin
   --  It is the environment task that initializes the Pool and thus
   --  becomes its owner. Here the environment task renounces ownership
   --  in order for the Puzzle_Runner task to take ownership.
   Stdh.Basic_Bounded_Dynamic_Pools.Set_Owner
     (Pool, Ada.Task_Identification.Null_Task_Id);

   --  The environment task gives the go ahead to the Puzzle_Runner task
   --  to take ownership of the Storage pool used in this package.
   Puzzle_Runner.Take_Ownership;
end Advent_Of_Code_2021;
