with Stda;
pragma Elaborate_All (Stda);

with Stdb;
pragma Elaborate_All (Stdb);

with Stdc;
pragma Elaborate_All (Stdc);

--  Std is short for Standard and a nod to the standard library in C++ :-)
package Std is
   pragma Elaborate_Body;

   package Types renames Stda.Types;
   --  Contains the fundamental type definitions Nat32, Pos32, Int32, etc.

   package Conversions renames Stda.Conversions;
   --  Contains conversion routines between standard types in the Ada standard
   --  and types defined in Types.

   package File_IO renames Stda.File_IO;

   package String_Split renames Stda.String_Split;

   package UTF8 renames Stda.UTF8;

end Std;
