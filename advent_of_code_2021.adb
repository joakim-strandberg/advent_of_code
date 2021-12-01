with Stdx;
pragma Elaborate_All (Stdx);

with Ada.Text_IO;

package body Advent_Of_Code_2021 is

   package body Day_1_Part_One is

      package Text_IO renames Ada.Text_IO;

      Infinite_Loop_Max : constant := 10_000_000;
      --  Maximum number of iterations allowed in this application.

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
            for I in Nat32 range 1 .. Infinite_Loop_Max loop
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
            for I in Nat32 range 1 .. Infinite_Loop_Max loop
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

      procedure Run is
         File_Name : constant String := "day_01_input.txt";

         Count : Nat32 := 0;
      begin
         Count_Lines_In_File (Count, File_Name);
         if Count = 0 then
            Text_IO.Put_Line ("File " & File_Name & " is empty!?");
            return;
         end if;

         declare
            Depth : Depth_Array (1 .. Count);
         begin
            Read_Values_From_File (Depth, File_Name);
            Text_IO.Put_Line (Nat32'Image (Count_Increases (Depth)));
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

            Increases_Count : constant Nat32 := Count_Increases (Depth);
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

   end Day_1_Part_One;

   package body Day_1_Part_Two is

      use type Day_1_Part_One.Depth_Type;

      package Text_IO renames Ada.Text_IO;

      procedure Make_Sliding_Window
        (Window : in out Day_1_Part_One.Depth_Array;
         Depth  : in     Day_1_Part_One.Depth_Array) is
      begin
         for I in Nat32 range Depth'First .. Depth'Last - 2 loop
            Window (I) := Depth (I) + Depth (I + 1) + Depth (I + 2);
         end loop;
      end Make_Sliding_Window;

      procedure Run is
         File_Name : constant String := "day_01_input.txt";

         Count : Nat32 := 0;
      begin
         Day_1_Part_One.Count_Lines_In_File (Count, File_Name);
         if Count = 0 then
            Text_IO.Put_Line ("File " & File_Name & " is empty!?");
            return;
         end if;

         declare
            Depth : Day_1_Part_One.Depth_Array (1 .. Count);
            Window : Day_1_Part_One.Depth_Array (1 .. Count - 2);
         begin
            Day_1_Part_One.Read_Values_From_File (Depth, File_Name);
            Make_Sliding_Window (Window, Depth);
            Text_IO.Put_Line (Nat32'Image
                              (Day_1_Part_One.Count_Increases (Window)));
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
            Depth : constant Day_1_Part_One.Depth_Array
              := (199, 200, 208, 210, 200, 207, 240, 269, 260, 263);

            Window : Day_1_Part_One.Depth_Array (1 .. Depth'Last - 2);

            Increases_Count : Nat32;
         begin
            Make_Sliding_Window (Window, Depth);
            Increases_Count := Day_1_Part_One.Count_Increases (Window);
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

   end Day_1_Part_Two;

end Advent_Of_Code_2021;
