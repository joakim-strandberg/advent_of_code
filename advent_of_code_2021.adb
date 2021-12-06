with Stdx;
pragma Elaborate_All (Stdx);

with Stdb;
with Stdh;

with Ada.Finalization;
with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;
--  with Ada.Containers.Vectors;
with Ada.Integer_Text_IO;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package body Advent_Of_Code_2021 is

   package Text_IO renames Ada.Text_IO;

   package Latin_1 renames Stda.Latin_1;

   package Text_Files renames Stdx.Text_Files;

   package Big_Integers renames Stdb.Big_Integers;

   subtype Limited_Controlled is Ada.Finalization.Limited_Controlled;

   use type Big_Integers.Multi_int;

   Pool : Stdh.Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool (1_000_000);
   --  This pool is shared by all puzzles, but it is only one puzzle
   --  that is solved each time the application is executed.

   type String_Ptr is access constant String;
   for String_Ptr'Storage_Pool use Pool;

   type String_Array is array (Pos32 range <>) of String_Ptr;

   type String_Array_Ptr is access String_Array;
   for String_Array_Ptr'Storage_Pool use Pool;

   package body Day_1 is

      type Depth_Type is new Nat32 range 0 .. Nat32'Last;
      --  The exact unit of a depth value is unspecified.
      --  This means it is not known if it is expressed in meters or not.

      type Depth_Array is array (Pos32 range <>) of Depth_Type;

      package Internal is

         procedure Count_Lines_In_File (Result    : in out Nat32;
                                        File_Name : String);

         procedure Read_Values_From_File (Depth     : in out Depth_Array;
                                          File_Name : String);

         function Count_Increases (Depth : Depth_Array) return Nat32;

      end Internal;

      package body Internal is

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

         procedure Count_Lines_In_File (Result    : in out Nat32;
                                        File_Name : String) is
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
         end Count_Lines_In_File;

         procedure Read_Values_From_File (Depth     : in out Depth_Array;
                                          File_Name : String) is
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
                  declare
                     Line : constant String := Text_IO.Get_Line (Input_File);
                  begin
                     Depth (I) := Depth_Type'Value (Line);
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
         end Read_Values_From_File;

      end Internal;

      package body Part_One is

         procedure Run is
            File_Name : constant String := "day_01_input.txt";

            Count : Nat32 := 0;
         begin
            Run_Test_Suite;
            Internal.Count_Lines_In_File (Count, File_Name);
            if Count = 0 then
               Text_IO.Put_Line ("File " & File_Name & " is empty!?");
               return;
            end if;

            declare
               Depth : Depth_Array (1 .. Count);
            begin
               Internal.Read_Values_From_File (Depth, File_Name);
               Text_IO.Put_Line (Nat32'Image
                                 (Internal.Count_Increases (Depth)));
            end;
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
                 := Internal.Count_Increases (Depth);
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
               Total_Test_Count : constant String
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
            File_Name : constant String := "day_01_input.txt";

            Count : Nat32 := 0;
         begin
            Run_Test_Suite;
            Internal.Count_Lines_In_File (Count, File_Name);
            if Count = 0 then
               Text_IO.Put_Line ("File " & File_Name & " is empty!?");
               return;
            end if;

            declare
               Depth  : Depth_Array (1 .. Count);
               Window : Depth_Array (1 .. Count - 2);
            begin
               Internal.Read_Values_From_File (Depth, File_Name);
               Make_Sliding_Window (Window, Depth);
               Text_IO.Put_Line (Nat32'Image
                                 (Internal.Count_Increases (Window)));
            end;
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
               Increases_Count := Internal.Count_Increases (Window);
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
               Total_Test_Count : constant String
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

      package body Part_One is

         type Position_Type is record
            X : Nat32;
            Y : Nat32;
         end record;

         procedure Read_Line (Position : in out Position_Type;
                              Line     : String)
         is
            Sep : constant Integer := Ada.Strings.Fixed.Index (Source  => Line,
                                                               Pattern => " ");

            Direction : constant Movement_Type
              := Movement_Type'Value (Line (Line'First .. Sep - 1));
            Value : constant Nat32
              := Nat32'Value (Line (Sep + 1 .. Line'Last));
         begin
            --  Text_IO.Put_Line ("1:" & Line (1 .. Sep - 1));
            --  Text_IO.Put_Line ("2:" & Line (Sep + 1 .. Line'Last));
            case Direction is
               when Forward => Position.X := Position.X + Value;
               when Up      => Position.Y := Position.Y - Value;
               when Down    => Position.Y := Position.Y + Value;
            end case;
         end Read_Line;

         procedure Read_Values_From_File is new Text_Files.Read_Line_By_Line
           (Text_Parser => Position_Type,
            Handle_Line => Read_Line);

         procedure Run is
            File_Name : constant String := "day_02_input.txt";
            Position : Position_Type := (X => 0, Y => 0);
         begin
            Read_Values_From_File (Position, File_Name);
            Text_IO.Put_Line (Nat32'Image
                              (Position.X * Position.Y));
         end Run;

      end Part_One;

      package body Part_Two is

         type Position_Type is record
            X   : Nat32;
            Y   : Nat32;
            Aim : Nat32;
         end record;

         procedure Read_Line (Position : in out Position_Type;
                              Line     : String)
         is
            Sep : constant Integer :=
              Ada.Strings.Fixed.Index (Source  => Line,
                                       Pattern => " ");

            Direction : Movement_Type
              := Movement_Type'Value (Line (Line'First .. Sep - 1));
            Value : Nat32
              := Nat32'Value (Line (Sep + 1 .. Line'Last));
         begin
            case Direction is
               when Forward =>
                  Position.X := Position.X + Value;
                  Position.Y := Position.Y + Value * Position.Aim;
               when Up      => Position.Aim := Position.Aim - Value;
               when Down    => Position.Aim := Position.Aim + Value;
            end case;
         end Read_Line;

         procedure Read_Values_From_File is new Text_Files.Read_Line_By_Line
           (Text_Parser => Position_Type,
            Handle_Line => Read_Line);

         procedure Run is
            File_Name : constant String := "day_02_input.txt";

            Position : Position_Type := (X => 0, Y => 0, Aim => 0);
         begin
            Read_Values_From_File (Position, File_Name);
            Text_IO.Put_Line (Nat32'Image
                              (Position.X * Position.Y));
         end Run;

      end Part_Two;

   end Day_2;

   package body Day_3 is

      subtype Column_Number is Positive range 1 .. 12;
      --  This type is a subtype of Positive in order to be compatible
      --  with the Positive index type used in Ada's standard String.

      package body Part_One is

         type Parser_Type is record
            Line    : String_Array_Ptr;
            Current : Pos32;
         end record;

         procedure Read_Line (Parser : in out Parser_Type;
                              Line   : String) is
         begin
            Parser.Line (Parser.Current) := new String'(Line);
            Parser.Current := Parser.Current + 1;
         end Read_Line;

         procedure Read_Values_From_File is new Text_Files.Read_Line_By_Line
           (Text_Parser => Parser_Type,
            Handle_Line => Read_Line);

         procedure Run is
            File_Name : constant String := "day_03_input.txt";
            Parser : Parser_Type;

            Line_Count : constant Nat32
              := Text_Files.Count_Lines_In_File (File_Name);

            One_Count  : Nat32;
            Zero_Count : Nat32;

            Gamma_Input  : String (Column_Number'Range);
            Epsilon_Input : String (Column_Number'Range);
         begin
            if Line_Count = 0 then
               Text_IO.Put_Line ("Error, empty file " & File_Name);
               return;
            end if;
            Parser := (Current => 1,
                       Line    => new String_Array (1 .. Line_Count));
            Read_Values_From_File (Parser, File_Name);

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

         type Position_Type is limited record
            S : S_Vc.Vector;
         end record;

         procedure Read_Line (Position : in out Position_Type;
                              Line     : String) is
         begin
            S_Vc.Append (Position.S, Line);
         end Read_Line;

         procedure Read_Values_From_File is new Text_Files.Read_Line_By_Line
           (Text_Parser => Position_Type,
            Handle_Line => Read_Line);

         type Counted_Type is record
            Zero_Count : Natural;
            One_Count  : Natural;
         end record;

         function Count (V : S_Vc.Vector;
                         C : Column_Number) return Counted_Type
         is
            First : constant String_Vector_Index := S_Vc.First_Index;
            Last  : constant S_Vc.Extended_Index := S_Vc.Last_Index (V);
            S : Input_String;

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
               First : constant String_Vector_Index := S_Vc.First_Index;
               Last  : constant S_Vc.Extended_Index
                 := S_Vc.Last_Index (Container);
               Item : Input_String;
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
               Item : Input_String;
            begin
               for I in String_Vector_Index range First .. Last loop
                  Item := S_Vc.Element (To_Delete, I);
                  S_Vc.Delete (Container, Item);
               end loop;
            end;
         end Filter_Out;

         procedure Run is
            File_Name : constant String := "day_03_input.txt";
            Position : Position_Type;

            Oxygen : String (Column_Number'Range);
            Co2    : String (Column_Number'Range);
         begin
            Read_Values_From_File (Position, File_Name);
            --  Oxygen
            for I in Column_Number'Range loop
               declare
                  Counted : constant Counted_Type := Count (V => Position.S,
                                                            C => I);
               begin
                  if Counted.Zero_Count > Counted.One_Count then
                     Filter_Out (Container => Position.S,
                                 Column    => I,
                                 Marker    => '1');
                  elsif Counted.Zero_Count <= Counted.One_Count then
                     Filter_Out (Container => Position.S,
                                 Column    => I,
                                 Marker    => '0');
                  end if;
               end;

               if S_Vc.Length (Position.S) = 1 then
                  Text_IO.Put_Line
                    ("Success! " & S_Vc.Element (Position.S, 1));
                  Oxygen := S_Vc.Element (Position.S, 1);
                  exit;
               end if;
            end loop;

            S_Vc.Clear (Position.S);
            Read_Values_From_File (Position, File_Name);
            --  CO2
            for I in Column_Number'Range loop
               declare
                  Counted : constant Counted_Type := Count (V => Position.S,
                                                            C => I);
               begin
                  if Counted.Zero_Count > Counted.One_Count then
                     Filter_Out (Container => Position.S,
                                 Column    => I,
                                 Marker    => '0');
                  elsif Counted.Zero_Count <= Counted.One_Count then
                     Filter_Out (Container => Position.S,
                                 Column    => I,
                                 Marker    => '1');
                  end if;
               end;

               if S_Vc.Length (Position.S) = 1 then
                  Text_IO.Put_Line
                    ("Success! " & S_Vc.Element (Position.S, 1));
                  Co2 := S_Vc.Element (Position.S, 1);
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
         Element_Type => Natural);

      type Board_Cell is record
         Is_Marked : Boolean;
         Value     : Natural;
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

      procedure Mark (Board : in out Board_Type;
                      Searched_For : Natural) is
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
                                            return Natural is
         Sum : Natural := 0;
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
                  Ada.Integer_Text_IO.Put (Board (R, C).Value, 3);
               else
                  Ada.Integer_Text_IO.Put (Board (R, C).Value, 4);
               end if;
            end loop;
            Text_IO.New_Line;
         end loop;
      end Put_Line;

      type Board_Index is range 1 .. 1024;

      package Board_Vs is new Stdh.Unbounded_Bounded_Vectors
        (Index_Type   => Board_Index,
         Element_Type => Board_Type);

      type Parser_Type is record
         State : Parser_State := Reading_First_Line;
         Numbers : Number_Vectors.Vector;
         Board : Board_Type;
         Boards : Board_Vs.Vector;
      end record;

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
            Result : Index_Interval_Array (1 .. Comma_Count + 1);
            Current : Positive := 1;
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

      procedure Parse_Board_Row (Parser : in out Parser_Type;
                                 Line   : String;
                                 Row    : Positive) is
         FL : constant String := Filter_Spaces (Line);
         Int : constant Index_Interval_Array
           := Get_Intervals (FL, ' ');
      begin
         for I in Positive range 1 .. 5 loop
            declare
               T : constant String
                 := FL (Int (I).First .. Int (I).Last);
            begin
               --  Ada.Text_IO.Put_Line ("wl: " & T);
               Parser.Board (Row, I)
                 := (Is_Marked => False,
                     Value     => Natural'Value (T));
            end;
         end loop;
      end Parse_Board_Row;

      procedure Read_Line (Parser : in out Parser_Type;
                           Line   : String) is
      begin
         case Parser.State is
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
                       (This     => Parser.Numbers,
                        New_Item =>
                          (Natural'Value
                               (Line (Int (I).First .. Int (I).Last))));
                  end loop;
               end;
               Parser.State := Read_Board_1;
            when Read_Board_1 =>
               if Line'Length > 0 then
                  Parse_Board_Row (Parser, Line, 1);
                  Parser.State := Read_Board_2;
               end if;
            when Read_Board_2 =>
               if Line'Length > 0 then
                  Parse_Board_Row (Parser, Line, 2);
                  Parser.State := Read_Board_3;
               end if;
            when Read_Board_3 =>
               if Line'Length > 0 then
                  Parse_Board_Row (Parser, Line, 3);
                  Parser.State := Read_Board_4;
               end if;
            when Read_Board_4 =>
               if Line'Length > 0 then
                  Parse_Board_Row (Parser, Line, 4);
                  Parser.State := Read_Board_5;
               end if;
            when Read_Board_5 =>
               if Line'Length > 0 then
                  Parse_Board_Row (Parser, Line, 5);
                  Board_Vs.Append (This     => Parser.Boards,
                                   New_Item => Parser.Board);
                  --  Ada.Text_IO.Put_Line ("Adding board");
                  Parser.State := Read_Board_1;
               end if;
         end case;
      end Read_Line;

      procedure Read_Values_From_File is new Text_Files.Read_Line_By_Line
        (Text_Parser => Parser_Type,
         Handle_Line => Read_Line);

      package body Part_One is

         procedure Run is
            File_Name : constant String := "day_04_input.txt";
            Parser : Parser_Type;

            Is_Winner_Found : Boolean := False;
            Wb : Board_Type;
            Wn : Natural;
         begin
            Read_Values_From_File (Parser, File_Name);

            declare
               First : constant Number_Index := Number_Index'First;
               Last  : constant Number_Vectors.Extended_Index
                 := Number_Vectors.Last_Index (Parser.Numbers);
               N : Natural;
            begin
               Outer_Loop : for I in Number_Index range First .. Last loop
                  N := Number_Vectors.Element (Parser.Numbers, I);

                  declare
                     First : constant Board_Index := Board_Index'First;
                     Last  : constant Board_Vs.Extended_Index
                       := Board_Vs.Last_Index (Parser.Boards);
                     B : Board_Type;
                  begin
                     for J in Board_Index range First .. Last loop
                        B := Board_Vs.Element (Parser.Boards, J);
                        Mark (Board        => B,
                              Searched_For => N);
                        Board_Vs.Replace_Element (Parser.Boards, J, B);
                     end loop;
                  end;

                  declare
                     First : constant Board_Index := Board_Index'First;
                     Last  : constant Board_Vs.Extended_Index
                       := Board_Vs.Last_Index (Parser.Boards);
                     B : Board_Type;
                  begin
                     for J in Board_Index range First .. Last loop
                        B := Board_Vs.Element (Parser.Boards, J);
                        if Is_Winner (Board => B) then
                           Ada.Text_IO.Put_Line ("Winner found!");
                           Is_Winner_Found := True;
                           Wb := B;
                           Wn := N;
                           exit Outer_Loop;
                        end if;
                     end loop;
                  end;
               end loop Outer_Loop;
            end;

            if Is_Winner_Found then
               Text_IO.Put_Line ("Wn" & Natural'Image (Wn));
               Text_IO.Put_Line
                 ("unmarked " & Natural'Image
                    (Sum_Of_All_Unmarked_Numbers (Wb)));
               Text_IO.Put_Line
                 (Natural'Image (Sum_Of_All_Unmarked_Numbers (Wb) * Wn));
               --  Anser: 45031
            end if;
         end Run;

      end Part_One;

      package body Part_Two is

         procedure Run is
            File_Name : constant String := "day_04_input.txt";
            Parser : Parser_Type;

            Is_Winner_Found : Boolean := False;
            Wb : Board_Type;
            Wn : Natural;
         begin
            Read_Values_From_File (Parser, File_Name);
            declare
               First : constant Number_Index := Number_Index'First;
               Last  : constant Number_Vectors.Extended_Index
                 := Number_Vectors.Last_Index (Parser.Numbers);
               N : Natural;
            begin
               Outer_Loop : for I in Number_Index range First .. Last loop
                  N := Number_Vectors.Element (Parser.Numbers, I);
                  declare
                     First : constant Board_Index := Board_Index'First;
                     Last  : constant Board_Vs.Extended_Index
                       := Board_Vs.Last_Index (Parser.Boards);
                     B : Board_Type;
                  begin
                     for J in Board_Index range First .. Last loop
                        B := Board_Vs.Element (Parser.Boards, J);
                        Mark (Board        => B,
                              Searched_For => N);
                        Board_Vs.Replace_Element (Parser.Boards, J, B);
                     end loop;
                  end;

                  declare
                     Fb : Board_Vs.Vector;  -- Boards to filter out
                  begin
                     declare
                        First : constant Board_Index := Board_Index'First;
                        Last  : constant Board_Vs.Extended_Index
                          := Board_Vs.Last_Index (Parser.Boards);
                        B : Board_Type;
                     begin
                        for J in Board_Index range First .. Last loop
                           B := Board_Vs.Element (Parser.Boards, J);
                           if Is_Winner (Board => B) then
                              Board_Vs.Append
                                (This     => Fb,
                                 New_Item => B);
                              Put_Line (B);
                              Text_IO.New_Line;

                              if Board_Vs.Length (Parser.Boards) = 1 then
                                 Ada.Text_IO.Put_Line ("Last Winner found!");

                                 Is_Winner_Found := True;
                                 Wb := Board_Vs.Element (Parser.Boards, 1);
                                 Wn := N;
                                 Put_Line (Wb);
                                 exit Outer_Loop;
                              end if;
                           end if;
                        end loop;
                     end;

                     declare
                        First : constant Board_Index := Board_Index'First;
                        Last  : constant Board_Vs.Extended_Index
                          := Board_Vs.Last_Index (Fb);
                        D : Board_Type;
                     begin
                        for J in Board_Index range First .. Last loop
                           D := Board_Vs.Element (Parser.Boards, J);
                           Board_Vs.Delete (Parser.Boards, D);
                        end loop;
                     end;
                  end;
               end loop Outer_Loop;
            end;

            if Is_Winner_Found then
               Ada.Text_IO.Put_Line ("Wn" & Natural'Image (Wn));
               Ada.Text_IO.Put_Line
                 ("unmarked " & Natural'Image
                    (Sum_Of_All_Unmarked_Numbers (Wb)));
               Ada.Text_IO.Put_Line
                 (Natural'Image (Sum_Of_All_Unmarked_Numbers (Wb) * Wn));
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
            Result : Index_Interval_Array (1 .. Comma_Count + 1);
            Current : Positive := 1;
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

      package body Part_One is

         type Board_Cell is record
            Count : Natural;
         end record;

         type X_Type is new Natural range 0 .. 999;
         type Y_Type is new Natural range 0 .. 999;

         type Board_Type is array (X_Type, Y_Type) of Board_Cell;

         type Parser_Type is record
            Board : Board_Type;
         end record;

         procedure Read_Line (Parser : in out Parser_Type;
                              Line   : String)
         is
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
               End_Point : constant String
                 := Line (Int (3).First .. Int (3).Last);
               Ls : constant Index_Interval_Array  --  Line start
                 := Get_Intervals (Start_Point, ',');
               Le : constant Index_Interval_Array  --  Line end
                 := Get_Intervals (End_Point, ',');
            begin
               if Ls'Length /= 2 then
                  Text_IO.Put_Line ("Start point error:" & Line);
               end if;
               if Le'Length /= 2 then
                  Text_IO.Put_Line ("Start end error:" & Line);
               end if;

               declare
                  X0 : constant X_Type
                    := X_Type'Value (Start_Point (Ls (1).First .. Ls (1).Last));
                  Y0 : constant Y_Type
                    := Y_Type'Value (Start_Point (Ls (2).First .. Ls (2).Last));
                  X1 : constant X_Type
                    := X_Type'Value (End_Point (Le (1).First .. Le (1).Last));
                  Y1 : constant Y_Type
                    := Y_Type'Value (End_Point (Le (2).First .. Le (2).Last));
               begin
                  if X0 = X1 then
                     if Y0 <= Y1 then
                        for I in Y_Type range Y0 .. Y1 loop
                           Parser.Board (X0, I).Count
                             := Parser.Board (X0, I).Count + 1;
                        end loop;
                     else
                        for I in Y_Type range Y1 .. Y0 loop
                           Parser.Board (X0, I).Count
                             := Parser.Board (X0, I).Count + 1;
                        end loop;
                     end if;
                  elsif Y0 = Y1 then
                     if X0 <= X1 then
                        for I in X_Type range X0 .. X1 loop
                           Parser.Board (I, Y0).Count
                             := Parser.Board (I, Y0).Count + 1;
                        end loop;
                     else
                        for I in X_Type range X1 .. X0 loop
                           Parser.Board (I, Y0).Count
                             := Parser.Board (I, Y0).Count + 1;
                        end loop;
                     end if;
                  else
                     Text_IO.Put_Line ("What to do: " & Line);
                     --  Shoud be ignored for now
                  end if;
               end;
            end;
         end Read_Line;

         procedure Read_Values_From_File is new Text_Files.Read_Line_By_Line
           (Text_Parser => Parser_Type,
            Handle_Line => Read_Line);

         procedure Run is
            File_Name : constant String := "day_05_input.txt";
            Parser : Parser_Type
              := (Board => (others => (others => (Count => 0))));
            Count : Natural := 0;
         begin
            Read_Values_From_File (Parser, File_Name);
            for X in X_Type loop
               for Y in Y_Type loop
                  if Parser.Board (X, Y).Count > 1 then
                     Count := Count + 1;
                  end if;
               end loop;
            end loop;
            Text_IO.Put_Line (Natural'Image (Count));
         end Run;

      end Part_One;

      package body Part_Two is

         type Board_Cell is record
            Count : Natural;
         end record;

         type X_Type is new Natural range 0 .. 999;
         type Y_Type is new Natural range 0 .. 999;

         type Board_Type is array (X_Type, Y_Type) of Board_Cell;

         type Parser_Type is record
            Board : Board_Type;
         end record;

         procedure Read_Line (Parser : in out Parser_Type;
                              Line   : String)
         is
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
               End_Point : constant String
                 := Line (Int (3).First .. Int (3).Last);
               Ls : constant Index_Interval_Array  --  Line start
                 := Get_Intervals (Start_Point, ',');
               Le : constant Index_Interval_Array  --  Line end
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
                           Parser.Board (X0, I).Count
                             := Parser.Board (X0, I).Count + 1;
                        end loop;
                     else
                        for I in Y_Type range Y1 .. Y0 loop
                           Parser.Board (X0, I).Count
                             := Parser.Board (X0, I).Count + 1;
                        end loop;
                     end if;
                  elsif Y0 = Y1 then
                     if X0 <= X1 then
                        for I in X_Type range X0 .. X1 loop
                           Parser.Board (I, Y0).Count
                             := Parser.Board (I, Y0).Count + 1;
                        end loop;
                     else
                        for I in X_Type range X1 .. X0 loop
                           Parser.Board (I, Y0).Count
                             := Parser.Board (I, Y0).Count + 1;
                        end loop;
                     end if;
                  else
                     if X0 <= X1 then
                        declare
                           Y : Y_Type := Y0;
                        begin
                           for I in X_Type range X0 .. X1 loop
                              Parser.Board (I, Y).Count
                                := Parser.Board (I, Y).Count + 1;

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
                              Parser.Board (I, Y).Count
                                := Parser.Board (I, Y).Count + 1;
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
         end Read_Line;

         procedure Read_Values_From_File is new Text_Files.Read_Line_By_Line
           (Text_Parser => Parser_Type,
            Handle_Line => Read_Line);

         procedure Run is
            File_Name : constant String := "day_05_input.txt";
            Parser : Parser_Type
              := (Board => (others => (others => (Count => 0))));
            Count : Natural := 0;
         begin
            Read_Values_From_File (Parser, File_Name);
            for X in X_Type loop
               for Y in Y_Type loop
                  if Parser.Board (X, Y).Count > 1 then
                     Count := Count + 1;
                  end if;
               end loop;
            end loop;
            Text_IO.Put_Line (Natural'Image (Count));
         end Run;

      end Part_Two;

   end Day_5;

   package body Day_6 is

      --  No need to override Finalize for this type in order to deallocate
      --  the storage pool allocated Line array because the array is
      --  allocated inside a statically allocated memory pool.
      type Parser_Type
        (File_Name : String_Ptr)
      is record
         Line    : String_Array_Ptr;
         Current : Pos32;
      end record;

      procedure Initialize (Parser : in out Parser_Type);

      Empty_File_Error : exception;

      procedure Read_Line (Parser : in out Parser_Type;
                           Line   : String) is
      begin
         Parser.Line (Parser.Current) := new String'(Line);
         Parser.Current := Parser.Current + 1;
      end Read_Line;

      procedure Read_Values_From_File is new Text_Files.Read_Line_By_Line
        (Text_Parser => Parser_Type,
         Handle_Line => Read_Line);

      procedure Initialize (Parser : in out Parser_Type) is
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
      end Initialize;

      File_Name : aliased constant String := "day_06_input.txt";

      package body Part_One is

         type Fish_Timer_Value is range 0 .. 8;

         type Fish_Count is new Nat32 range 0 .. Nat32'Last;
         --  Specifies a number of cray-fish.

         type Fish_Count_Array is array (Fish_Timer_Value) of Fish_Count;
         --  This type stores the number of cray-fish with a given
         --  timer count down value,

         procedure Run is
            Parser : Parser_Type (File_Name'Access);

            Sum : Fish_Count := 0;

            Fish : Fish_Count_Array := (others => 0);
         begin
            Initialize (Parser);
            declare
               Int : Latin_1.Index_Interval_Array
                 := Latin_1.Get_Intervals (Parser.Line (1).all, ',');
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
                               Value  : Big_Integers.Multi_int);

            procedure Set_Int (Holder : in out Holder_Type;
                               Value  : Holder_Type);

            function Int (Holder : Holder_Type)
                          return Big_Integers.Multi_int;

         private

            type Item_Ptr is access Big_Integers.Multi_int;

            type Holder_Type is new Limited_Controlled with record
               Item : Item_Ptr;
            end record;

            procedure Finalize (Holder : in out Holder_Type);

         end Big_Int_Holder;

         package body Big_Int_Holder is

            procedure Free is new Ada.Unchecked_Deallocation
              (Object => Big_Integers.Multi_int,
               Name   => Item_Ptr);

            procedure Set_Int (Holder : in out Holder_Type;
                               Value  : Big_Integers.Multi_int) is
            begin
               Free (Holder.Item);
               Holder.Item := new Big_Integers.Multi_int'(Value);
            end Set_Int;

            procedure Set_Int (Holder : in out Holder_Type;
                               Value  : Holder_Type) is
            begin
               if Holder.Item /= null then
                  Free (Holder.Item);
               end if;
               Holder.Item := new Big_Integers.Multi_int'(Int (Value));
            end Set_Int;

            function Int (Holder : Holder_Type)
                          return Big_Integers.Multi_int is
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
            Parser : Parser_Type (File_Name'Access);

            Sum : Big_Int_Holder.Holder_Type;

            Fish : Fish_Count_Array; --  := (others => Big_Integers.IO.Val ("0"));
         begin
            Initialize (Parser);
            Big_Int_Holder.Set_Int (Holder => Sum,
                                    Value  => Big_Integers.IO.Val ("0"));
            for I in Fish'Range loop
               Big_Int_Holder.Set_Int (Holder => Fish (I),
                                       Value  => Big_Integers.IO.Val ("0"));
            end loop;

            declare
               Int : Latin_1.Index_Interval_Array
                 := Latin_1.Get_Intervals (Parser.Line (1).all, ',');
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

end Advent_Of_Code_2021;
