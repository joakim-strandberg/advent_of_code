with Ada.Text_IO;
with Std;

package Ac22_Tst is

   use Std.Types;

   package Nat32_IO is new Ada.Text_IO.Integer_IO (Nat32);

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

end Ac22_Tst;
