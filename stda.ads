--  According to Rational guide for Ada from the 90's, it is good practise
--  to group packages to avoid naming collisions.
--
--  Std is short for Standard and a nod to the standard library in C++ :-)
with Interfaces;

package Stda is

   Infinite_Loop_Max : constant := (2**31 - 1);
   --  Maximum number of iterations allowed in this application.
   --  Instead of while loops, use the following construct in order
   --  to detect the existence of infinite loops.
   --
   --  declare
   --     Is_Infinite_Loop_Detected : Boolean := True;
   --  begin
   --     for I in Nat32 range 1 .. Infinite_Loop_Max loop
   --        if ... then
   --           Is_Infinite_Loop_Detected := False;
   --           exit;
   --        end if;
   --        ...
   --     end loop;
   --
   --     if Is_Infinite_Loop_Detected then
   --        raise Constraint_Error with "infinite loop detected";
   --     end if;
   --  end;

   package Types is

      type Int32 is range -2**31 + 1 .. (2**31 - 1);
      --  This type is defined to make the value -2*31 invalid
      --  in order to easily find usage of uninitialized variables
      --  through pragma Normalize_Scalars (..).

      subtype Pos16 is Int32 range 1 .. 2 ** 15 - 1;
      subtype Pos32 is Int32 range 1 .. Int32'Last;

      subtype Nat32 is Int32 range 0 .. Int32'Last;

      type Hash32 is mod 2**32;

      type Byte_Type is mod 2 ** 8;
      --  This byte definition is cross-compiler.
      --  It would be possible to use Ada.Streams.Stream_Element instead
      --  but may not exist in all Ada run-times.

      type Byte_Array is array (Pos32 range <>) of Byte_Type;

      type Mod_Int32 is mod 2 ** 32;

      type UTF8_String is new Byte_Array;
      --  Should be used when it is known that a byte array is in fact
      --  a UTF8-encoded unicode string. The subprograms for dealing
      --  with UTF8 encoded strings are to be found in the Stdb.UTF8 package.
      --  This subtype definition is defined here in order to define
      --  AScii encoded strings.

      subtype ASCII_String is Byte_Array;
      --  Should be used when it is known that a byte array is an ASCII-encoded
      --  text string. An ASCII string is always a UTF8 string but
      --  a UTF8 string may not be an ASCII string. The set of ASCII strings
      --  is a true subset of the set of all UTF8 strings.

      function "=" (Left, Right : Byte_Array) return Boolean;

      function "&" (Left, Right : Byte_Array) return Byte_Array;
      --  TODO: Needs review. This cannot always guarantuee result array
      --  always start index is 1.

      function "+" (Value : Byte_Type) return Int32;

      function "+" (Value : Character) return Byte_Type;
      --  Converts a character enumeration value into an integer according to
      --  Latin_1 character encoding.
      --
      --  Further down in this file is the nested package Latin_1 defined,
      --  and this operator is an example of a function that depends on
      --  the Latin_1 package.

      function "+" (Text : String) return Byte_Array;
      --  Converts an array of character enumeration values into corresponding
      --  integer values according to Latin_1 character encoding.
      --
      --  Further down in this file is the nested package Latin_1 defined,
      --  and this operator is an example of a function that depends on
      --  the Latin_1 package.

      function To_Char (This : Int32) return Character;
      --  with
      --  Global => null,
      --  Pre    => This >= 0 and This <= 9;

      function "+" (This : Int32) return String;
      --  The plus operator is often used to convert an object of
      --  "standard type" into another type. The types in the package
      --  Standard and types defined in this package is considered
      --  to be standard types.
      --
      --  Converts an Int32 value into a String representation.

      type Ada_Code_Location is record
         Code_1 : Int32;
         Code_2 : Int32;
      end record;
      --  Is intended to be used in situations where caller of a subprogram
      --  may want to act differently depending on what issue has arisen
      --  when calling a subprogram.

      function "+" (This : Ada_Code_Location) return String;

      type Subprogram_Call_Result (Has_Failed : Boolean := False) is record
         case Has_Failed is
            when True  => Codes : Ada_Code_Location;
            when False => null;
         end case;
      end record;

      function Message (This : Subprogram_Call_Result) return String;

      function "+" (This : Subprogram_Call_Result) return String;
      --  Raises exception if Has_Failed discriminant is False.

   end Types;

   subtype Pos32 is Types.Pos32;
   subtype Nat32 is Types.Nat32;

   generic
      Max_Length : Types.Pos16;
   package Bounded_Strings is

      type Bounded_String is private;

      function "+" (This : Bounded_String) return String;

      procedure Append (This : in out Bounded_String;
                        Text : String);

      function Is_Empty (This : Bounded_String) return Boolean;

      function Length (This : Bounded_String) return Natural;

      function Empty return Bounded_String;

   private

      type Temp_String is array (Integer range <>) of Character;
      --  Can hopefully be replaced by standard String in the future.

      type Bounded_String is record
         My_Text : Temp_String (1 .. Integer (Max_Length));
         --  The conversion of Max_Length to Integer can never fail no matter
         --  what Ada compiler is used to make an executable of this code.
         My_Last_Index : Natural := 0;
      end record;

   end Bounded_Strings;

   --  declare
   --     First : constant Vector_Index := Vector_Index'First;
   --     Last  : constant Vectors.Extended_Index
   --       := Vectors.Last_Index (V);
   --     Item : Item_Type;
   --  begin
   --     for I in Vector_Index range First .. Last loop
   --        Item := Vectors.Element (V, I);
   --        ...
   --     end loop;
   --  end;
   generic
      type Element_Type is private;
      type Index is range <>;
      --  Index is a type that is assumed to always start from one
      --  and upwards 1 .. n where n is some positive number.
   package Bounded_Vectors is

      subtype Extended_Index is Index'Base range 0 .. Index'Last;

      type Element_Ptr is access all Element_Type;
      for Element_Ptr'Storage_Size use 0;

      type Vector is private;

      function "=" (L, R : Vector) return Boolean;

      procedure Append (This     : in out Vector;
                        New_Item : Element_Type);
      --  Pre-condition: not Is_Full (This)

      function Contains (This         : Vector;
                         Searched_For : Element_Type) return Boolean;

      --  function First_Index (This : Vector) return Index;

      function Last_Index (This : Vector) return Extended_Index;

      function Element (This  : Vector;
                        Idx   : Index) return Element_Type;

      function Element_Reference (This  : access Vector;
                                  Idx   : Index) return Element_Ptr;
      --  It might be controversial with returning an access type value in
      --  this function, what if somebody would do Unchecked_Deallocation on
      --  the access type? One strategy is to minimize the number of packages
      --  that make use of Unchecked_Deallocation by using AdaControl that
      --  may forbid usage of Unchecked_Deallocation except for a number
      --  of specified exceptions (list of packages
      --  allowed to use unchecked deallocation). In addition,
      --  what if some global variable stores the access type value beyond
      --  the life-time of the element? Again, static code analysis by
      --  AdaControl can forbid global access type variables.
      --  Therefore, it is here attempted to allow
      --  the existence of this function. Maybe the test of time
      --  will prove this vector design decision wrong.

      procedure Replace_Element (This        : in out Vector;
                                 Idx         : Index;
                                 New_Element : Element_Type);

      procedure Replace_Last_Element (This        : in out Vector;
                                      New_Element : Element_Type);

      function Is_Empty (This : Vector) return Boolean;

      function Is_Full (This : Vector) return Boolean;

      function Last_Element (This : Vector) return Element_Type;

      function Last_Element_Reference (This : access Vector)
                                       return Element_Ptr;

      procedure Delete_Last (This : in out Vector);

      procedure Delete (This : in out Vector;
                        Item : Element_Type);
      --  Deletes all instances of the item from the vector.

      procedure Clear (This : in out Vector);

      function Length (This : Vector) return Types.Nat32;

   private

      type Elements_Array is array (Index) of aliased Element_Type;
      --  The array type is aliased to allow references to individual
      --  elements in the array.

      type Vector is record
         Items   : Elements_Array;
         My_Last : Extended_Index := Extended_Index'First;
      end record;

   end Bounded_Vectors;

   --  This tyoe of vector should be allocated inside pool.
   generic
      type Element_Type is private;
      type Index_Type is range <>;
      --  Index is a type that is assumed to always start from one
      --  and upwards 1 .. n where n is some positive number.
   package Pool_Bounded_Vectors is

      subtype Index is Index_Type;

      subtype Extended_Index is Index_Type'Base range 0 .. Index_Type'Last;

      type Optional_Element (Exists : Boolean := False) is record
         case Exists is
            when True  => Value : Element_Type;
            when False => null;
         end case;
      end record;

      type Element_Array is array (Index_Type) of Optional_Element;
      --  The array type is not aliased to disallow references to individual
      --  elements in the array. The idea is to use generics to change
      --  part of an element, not using an access-to-object type.

      type Read_Only_Element_Array_Ptr is access constant Element_Array;

      type Vector is private;

      function Array_View (Container : access Vector)
                           return Read_Only_Element_Array_Ptr;
      --  Read only view of the vector container.

      function "=" (L, R : Vector) return Boolean;

      procedure Append (Container : in out Vector;
                        New_Item  : Element_Type);
      --  Pre-condition: not Is_Full (This)

      procedure Append (Container : access Vector;
                        New_Item  : Element_Type);

      function Contains (This         : Vector;
                         Searched_For : Element_Type) return Boolean;

--        function First (This : Vector) return Index;
--
--        function Last (This : Vector) return Extended_Index;

      function Is_Empty (This : Vector) return Boolean;

      function Is_Full (This : Vector) return Boolean;

      function Element (This  : Vector;
                        Index : Index_Type) return Element_Type;

      function Last_Element (This : Vector) return Element_Type;

      function Last_Element (Container : access Vector) return Element_Type;

      procedure Delete_Last (This : in out Vector);

      procedure Delete_Last (Container : access Vector);

      procedure Delete (This : in out Vector;
                        Item : Element_Type);
      --  Deletes all instances of the item from the vector.

      procedure Replace_Element (This        : in out Vector;
                                 Idx         : Index_Type;
                                 New_Element : Element_Type);

      procedure Clear (This : in out Vector);

      procedure Clear (This : access Vector);

      function Length (This : Vector) return Types.Nat32;

      function Length (Container : access Vector) return Types.Nat32;

      package Exported_Identifiers is

         First : constant Index_Type := Index_Type'First;

         function Last (This : Vector) return Extended_Index;

         function Last (This : access Vector) return Extended_Index;

      end Exported_Identifiers;

   private

      type Vector is record
         Items   : aliased Element_Array;
         My_Last : Extended_Index := Extended_Index'First;
      end record;

   end Pool_Bounded_Vectors;

   --  This is a bounded vector implementation that is intended to be used
   --  in the private part of record types. And the instances of the record
   --  types are intended to be defined on package level, not inside
   --  declarative region of any subprogram.
   generic
      type Element_Type is private;
      type Index is range <>;
      --  Index is a type that is assumed to always start from one
      --  and upwards 1 .. n where n is some positive number.
   package Record_Bounded_Vectors is

      subtype Extended_Index is Index'Base range 0 .. Index'Last;

      type Element_Ptr is access all Element_Type;
      for Element_Ptr'Storage_Size use 0;

      type Elements_Array is private;

      type Elements_Ptr is access all Elements_Array;
      --  It's OK to allocate the element array in the default storage pool.
      --  On Linux, Windows, Mac OS X this means the heap.

      type Vector (Item : Elements_Ptr) is private;
      --  This type is not limited in order to be able to reset/emtpy
      --  the vector through aggregate.

      function "=" (L, R : Vector) return Boolean;
      --  This equals function returns True if both Vectors have the same
      --  number of elements and the same element at the same position
      --  in the vector, otherwise False.

      function Empty (This : Vector) return Vector;
      --  This function is intended to be used in aggregates to empty
      --  the contents of the vector. The effect is the same as calling
      --  the Clear subprogram on the vector.

      procedure Append (This     : in out Vector;
                        New_Item : Element_Type);
      --  Pre-condition: not Is_Full (This)

      function Contains (This         : Vector;
                         Searched_For : Element_Type) return Boolean;

      --  function First_Index (This : Vector) return Index;

      function Last_Index (This : Vector) return Extended_Index;

      function Element (This  : Vector;
                        Idx   : Index) return Element_Type;

      function Element_Reference (This  : access Vector;
                                  Idx   : Index) return Element_Ptr;
      --  It might be controversial with returning an access type value in
      --  this function, what if somebody would do Unchecked_Deallocation on
      --  the access type? One strategy is to minimize the number of packages
      --  that make use of Unchecked_Deallocation by using AdaControl that
      --  may forbid usage of Unchecked_Deallocation except for a number
      --  of specified exceptions (list of packages
      --  allowed to use unchecked deallocation). In addition,
      --  what if some global variable stores the access type value beyond
      --  the life-time of the element? Again, static code analysis by
      --  AdaControl can forbid global access type variables.
      --  Therefore, it is here attempted to allow
      --  the existence of this function. Maybe the test of time
      --  will prove this vector design decision wrong.

      procedure Replace_Element (This        : in out Vector;
                                 Idx         : Index;
                                 New_Element : Element_Type);

      procedure Replace_Last_Element (This        : in out Vector;
                                      New_Element : Element_Type);

      function Is_Empty (This : Vector) return Boolean;

      function Is_Full (This : Vector) return Boolean;

      function Last_Element (This : Vector) return Element_Type;

      function Last_Element_Reference (This : access Vector)
                                       return Element_Ptr;

      procedure Delete_Last (This : in out Vector);

      procedure Delete (This : in out Vector;
                        Item : Element_Type);
      --  Deletes all instances of the item from the vector.

      procedure Clear (This : in out Vector);

      function Length (This : Vector) return Types.Nat32;

   private

      type Elements_Array is array (Index) of aliased Element_Type;
      --  The array type is aliased to allow references to individual
      --  elements in the array.

      type Vector (Item : Elements_Ptr) is record
         My_Last : Extended_Index := Extended_Index'First;
      end record;

   end Record_Bounded_Vectors;

   generic
      type Element_Type is private;
      type Index_Type is range <>;
      --  Index is a type that is assumed to always start from one
      --  and upwards 1 .. n where n is some positive number.
   package Record_Bounded_Vectors2 is

      subtype Index is Index_Type;

      subtype Extended_Index is Index'Base range 0 .. Index'Last;

      type Element_Ptr is access all Element_Type;
      for Element_Ptr'Storage_Size use 0;

      type Elements_Array is private;

      type Elements_Ptr is access all Elements_Array;
      --  It's OK to allocate the element array in the default storage pool.
      --  On Linux, Windows, Mac OS X this means the heap.

      type Vector is private;
      --  This type is not limited in order to be able to reset/emtpy
      --  the vector through aggregate.
      --
      --   declare
      --      First : constant Vs.Index := Vs.Index'First;
      --      Last  : constant Vs.Extended_Index
      --        := Vs.Last_Index (V);
      --      Id : Element_Type;
      --   begin
      --      for I in Vector_Index range First .. Last loop
      --         Id := Vs.Element (V, I);
      --      end loop;
      --   end;

      procedure Init (V    : in out Vector;
                      Item : Elements_Ptr);

      function "=" (L, R : Vector) return Boolean;
      --  This equals function returns True if both Vectors have the same
      --  number of elements and the same element at the same position
      --  in the vector, otherwise False.

      function Empty (This : Vector) return Vector;
      --  This function is intended to be used in aggregates to empty
      --  the contents of the vector. The effect is the same as calling
      --  the Clear subprogram on the vector.

      procedure Append (Container : in out Vector;
                        New_Item  : Element_Type);
      --  Pre-condition: not Is_Full (This)

      function Contains (This         : Vector;
                         Searched_For : Element_Type) return Boolean;

      --  function First_Index (This : Vector) return Index;

      function Last_Index (This : Vector) return Extended_Index;

      function Element (This  : Vector;
                        Idx   : Index) return Element_Type;

      function Element_Reference (This  : access Vector;
                                  Idx   : Index) return Element_Ptr;
      --  It might be controversial with returning an access type value in
      --  this function, what if somebody would do Unchecked_Deallocation on
      --  the access type? One strategy is to minimize the number of packages
      --  that make use of Unchecked_Deallocation by using AdaControl that
      --  may forbid usage of Unchecked_Deallocation except for a number
      --  of specified exceptions (list of packages
      --  allowed to use unchecked deallocation). In addition,
      --  what if some global variable stores the access type value beyond
      --  the life-time of the element? Again, static code analysis by
      --  AdaControl can forbid global access type variables.
      --  Therefore, it is here attempted to allow
      --  the existence of this function. Maybe the test of time
      --  will prove this vector design decision wrong.

      procedure Replace_Element (This        : in out Vector;
                                 Idx         : Index;
                                 New_Element : Element_Type);

      procedure Replace_Last_Element (This        : in out Vector;
                                      New_Element : Element_Type);

      function Is_Empty (This : Vector) return Boolean;

      function Is_Full (This : Vector) return Boolean;

      function Last_Element (This : Vector) return Element_Type;

      function Last_Element_Reference (This : access Vector)
                                       return Element_Ptr;

      procedure Delete_Last (This : in out Vector);

      procedure Delete (This : in out Vector;
                        Item : Element_Type);
      --  Deletes all instances of the item from the vector.

      procedure Clear (This : in out Vector);

      function Length (This : Vector) return Types.Nat32;

   private

      type Elements_Array is array (Index) of aliased Element_Type;
      --  The array type is aliased to allow references to individual
      --  elements in the array.

      type Vector is record
         Item    : Elements_Ptr;
         My_Last : Extended_Index := Extended_Index'First;
      end record;

   end Record_Bounded_Vectors2;

   type Any_Task_Id is range 1 .. 1_000;
   --  It's good practice to be able to uniquely identify tasks
   --  when logging information (which task is reporting error messages).

   --  Originates from the Ada95 Reference Manual.
   --
   --  The reason this package exists is because Ada.Characters.Latin_1
   --  is not implemented in the Janus/Ada compiler, and in addition
   --  removes the need to use Character'Pos (..) to convert a character
   --  into an integer value which means it is possible to ban
   --  all usage of the 'Pos attribute using GNATCheck
   --  (rule POS_On_Enumeration_Types).
   --
   --  One may question why the Latin_1 package should be defined here,
   --  because if one doesn't use Latin 1, why should there be a dependency
   --  upon this package then? The reason is that the
   --  package should contain conversion routines between the standard
   --  types (i.e. Int32, ...) and uses subprograms in Latin_1 to do it.
   --  If one has an application that uses the package,
   --  one also has a dependency upon the Latin_1 package and why would
   --  it then not be sufficient to "with Ada_Extensions;" but also
   --  be forced to "with Latin_1;" to use the Latin_1 package?
   --  Taking these considerations into account, the Latin_1 package
   --  is therefore defined as a nested package.
   package Latin_1 is

      --  This record type contains the substring indices into an existing
      --  String. Consider the String "Hello World!" where the first
      --  String index is 1 and the last index is 12.
      --  First = 7 and Last = 11 denotes the substring "World".
      --  Natural is the index type used in standard Ada Strings.
      type Substring_Indices is record
         First : Natural;
         Last  : Natural;
      end record;

      type Substring_Array is array (Pos32 range <>) of Substring_Indices;

      function Split (Line : String;
                      Sep  : Character) return Substring_Array;
      --  Is used to split a String into substrings.

      subtype Character_As_Int32 is Types.Int32 range 0 .. 255;

      function To_Int32 (Value : Character) return Character_As_Int32;

      function Is_Graphic_Character (C : Character_As_Int32) return Boolean;

      --
      --  Control characters
      --

      NUL                  : constant Character := Character'Val (0);
      SOH                  : constant Character := Character'Val (1);
      STX                  : constant Character := Character'Val (2);
      ETX                  : constant Character := Character'Val (3);
      EOT                  : constant Character := Character'Val (4);
      ENQ                  : constant Character := Character'Val (5);
      ACK                  : constant Character := Character'Val (6);
      BEL                  : constant Character := Character'Val (7);
      BS                   : constant Character := Character'Val (8);
      HT                   : constant Character := Character'Val (9);
      LF                   : constant Character := Character'Val (10);
      --  Line feed

      VT                   : constant Character := Character'Val (11);
      FF                   : constant Character := Character'Val (12);
      CR                   : constant Character := Character'Val (13);
      --  Carriage return

      SO                   : constant Character := Character'Val (14);
      SI                   : constant Character := Character'Val (15);

      DLE                  : constant Character := Character'Val (16);
      DC1                  : constant Character := Character'Val (17);
      DC2                  : constant Character := Character'Val (18);
      DC3                  : constant Character := Character'Val (19);
      DC4                  : constant Character := Character'Val (20);
      NAK                  : constant Character := Character'Val (21);
      SYN                  : constant Character := Character'Val (22);
      ETB                  : constant Character := Character'Val (23);
      CAN                  : constant Character := Character'Val (24);
      EM                   : constant Character := Character'Val (25);
      SUB                  : constant Character := Character'Val (26);
      ESC                  : constant Character := Character'Val (27);
      FS                   : constant Character := Character'Val (28);
      GS                   : constant Character := Character'Val (29);
      RS                   : constant Character := Character'Val (30);
      US                   : constant Character := Character'Val (31);

      --
      --  ISO 646 graphic characters
      --

      Space                : constant Character := ' ';  -- Character'Val(32)
      Exclamation          : constant Character := '!';  -- Character'Val(33)
      Quotation            : constant Character := '"';  -- Character'Val(34)
      Number_Sign          : constant Character := '#';  -- Character'Val(35)
      Dollar_Sign          : constant Character := '$';  -- Character'Val(36)
      Percent_Sign         : constant Character := '%';  -- Character'Val(37)
      Ampersand            : constant Character := '&';  -- Character'Val(38)
      Apostrophe           : constant Character := ''';  -- Character'Val(39)
      Left_Parenthesis     : constant Character := '(';  -- Character'Val(40)
      Right_Parenthesis    : constant Character := ')';  -- Character'Val(41)
      Asterisk             : constant Character := '*';  -- Character'Val(42)
      Plus_Sign            : constant Character := '+';  -- Character'Val(43)
      Comma                : constant Character := ',';  -- Character'Val(44)
      Hyphen               : constant Character := '-';  -- Character'Val(45)
      Minus_Sign           : Character renames Hyphen;
      Full_Stop            : constant Character := '.';  -- Character'Val(46)
      Solidus              : constant Character := '/';  -- Character'Val(47)

      --
      --  Decimal digits '0' though '9' are at positions 48 through 57
      --

      Colon                : constant Character := ':';  -- Character'Val(58)
      Semicolon            : constant Character := ';';  -- Character'Val(59)
      Less_Than_Sign       : constant Character := '<';  -- Character'Val(60)
      Equals_Sign          : constant Character := '=';  -- Character'Val(61)
      Greater_Than_Sign    : constant Character := '>';  -- Character'Val(62)
      Question             : constant Character := '?';  -- Character'Val(63)
      Commercial_At        : constant Character := '@';  -- Character'Val(64)

      --
      --  Letters 'A' through 'Z' are at positions 65 through 90
      --

      Left_Square_Bracket  : constant Character := '[';  -- Character'Val(91)
      Reverse_Solidus      : constant Character := '\';  -- Character'Val(92)
      Right_Square_Bracket : constant Character := ']';  -- Character'Val(93)
      Circumflex           : constant Character := '^';  -- Character'Val(94)
      Low_Line             : constant Character := '_';  -- Character'Val(95)

      Grave                : constant Character := '`';  -- Character'Val(96)
      LC_A                 : constant Character := 'a';  -- Character'Val(97)
      LC_B                 : constant Character := 'b';  -- Character'Val(98)
      LC_C                 : constant Character := 'c';  -- Character'Val(99)
      LC_D                 : constant Character := 'd';  -- Character'Val(100)
      LC_E                 : constant Character := 'e';  -- Character'Val(101)
      LC_F                 : constant Character := 'f';  -- Character'Val(102)
      LC_G                 : constant Character := 'g';  -- Character'Val(103)
      LC_H                 : constant Character := 'h';  -- Character'Val(104)
      LC_I                 : constant Character := 'i';  -- Character'Val(105)
      LC_J                 : constant Character := 'j';  -- Character'Val(106)
      LC_K                 : constant Character := 'k';  -- Character'Val(107)
      LC_L                 : constant Character := 'l';  -- Character'Val(108)
      LC_M                 : constant Character := 'm';  -- Character'Val(109)
      LC_N                 : constant Character := 'n';  -- Character'Val(110)
      LC_O                 : constant Character := 'o';  -- Character'Val(111)
      LC_P                 : constant Character := 'p';  -- Character'Val(112)
      LC_Q                 : constant Character := 'q';  -- Character'Val(113)
      LC_R                 : constant Character := 'r';  -- Character'Val(114)
      LC_S                 : constant Character := 's';  -- Character'Val(115)
      LC_T                 : constant Character := 't';  -- Character'Val(116)
      LC_U                 : constant Character := 'u';  -- Character'Val(117)
      LC_V                 : constant Character := 'v';  -- Character'Val(118)
      LC_W                 : constant Character := 'w';  -- Character'Val(119)
      LC_X                 : constant Character := 'x';  -- Character'Val(120)
      LC_Y                 : constant Character := 'y';  -- Character'Val(121)
      LC_Z                 : constant Character := 'z';  -- Character'Val(122)
      Left_Curly_Bracket   : constant Character := '{';  -- Character'Val(123)
      Vertical_Line        : constant Character := '|';  -- Character'Val(124)
      Right_Curly_Bracket  : constant Character := '}';  -- Character'Val(125)
      Tilde                : constant Character := '~';  -- Character'Val(126)
      DEL                  : constant Character := Character'Val (127);

      --
      --  ISO 6429 control characters
      --

      IS4                  : Character renames FS;
      IS3                  : Character renames GS;
      IS2                  : Character renames RS;
      IS1                  : Character renames US;

      Reserved_128         : constant Character := Character'Val (128);
      Reserved_129         : constant Character := Character'Val (129);
      BPH                  : constant Character := Character'Val (130);
      NBH                  : constant Character := Character'Val (131);
      Reserved_132         : constant Character := Character'Val (132);
      NEL                  : constant Character := Character'Val (133);
      SSA                  : constant Character := Character'Val (134);
      ESA                  : constant Character := Character'Val (135);
      HTS                  : constant Character := Character'Val (136);
      HTJ                  : constant Character := Character'Val (137);
      VTS                  : constant Character := Character'Val (138);
      PLD                  : constant Character := Character'Val (139);
      PLU                  : constant Character := Character'Val (140);
      RI                   : constant Character := Character'Val (141);
      SS2                  : constant Character := Character'Val (142);
      SS3                  : constant Character := Character'Val (143);
      DCS                  : constant Character := Character'Val (144);
      PU1                  : constant Character := Character'Val (145);
      PU2                  : constant Character := Character'Val (146);
      STS                  : constant Character := Character'Val (147);
      CCH                  : constant Character := Character'Val (148);
      MW                   : constant Character := Character'Val (149);
      SPA                  : constant Character := Character'Val (150);
      EPA                  : constant Character := Character'Val (151);
      SOS                  : constant Character := Character'Val (152);
      Reserved_153         : constant Character := Character'Val (153);
      SCI                  : constant Character := Character'Val (154);
      CSI                  : constant Character := Character'Val (155);
      ST                   : constant Character := Character'Val (156);
      OSC                  : constant Character := Character'Val (157);
      PM                   : constant Character := Character'Val (158);
      APC                  : constant Character := Character'Val (159);

      --
      --  Other graphic characters:
      --

      --
      --  Character positions 160 (16#A0#) .. 175 (16#AF#):
      --

      No_Break_Space              : constant Character := Character'Val (160);
      NBSP                        : Character renames No_Break_Space;
      Inverted_Exclamation        : constant Character := Character'Val (161);
      Cent_Sign                   : constant Character := Character'Val (162);
      Pound_Sign                  : constant Character := Character'Val (163);
      Currency_Sign               : constant Character := Character'Val (164);
      Yen_Sign                    : constant Character := Character'Val (165);
      Broken_Bar                  : constant Character := Character'Val (166);
      Section_Sign                : constant Character := Character'Val (167);
      Diaeresis                   : constant Character := Character'Val (168);
      Copyright_Sign              : constant Character := Character'Val (169);
      Feminine_Ordinal_Indicator  : constant Character := Character'Val (170);
      Left_Angle_Quotation        : constant Character := Character'Val (171);
      Not_Sign                    : constant Character := Character'Val (172);
      Soft_Hyphen                 : constant Character := Character'Val (173);
      Registered_Trade_Mark_Sign  : constant Character := Character'Val (174);
      Macron                      : constant Character := Character'Val (175);

      --
      --  Character positions 176 (16#B0#) .. 191 (16#BF#):
      --

      Degree_Sign                 : constant Character := Character'Val (176);
      Ring_Above                  : Character renames Degree_Sign;
      Plus_Minus_Sign             : constant Character := Character'Val (177);
      Superscript_Two             : constant Character := Character'Val (178);
      Superscript_Three           : constant Character := Character'Val (179);
      Acute                       : constant Character := Character'Val (180);
      Micro_Sign                  : constant Character := Character'Val (181);
      Pilcrow_Sign                : constant Character := Character'Val (182);
      Paragraph_Sign              : Character renames Pilcrow_Sign;
      Middle_Dot                  : constant Character := Character'Val (183);
      Cedilla                     : constant Character := Character'Val (184);
      Superscript_One             : constant Character := Character'Val (185);
      Masculine_Ordinal_Indicator : constant Character := Character'Val (186);
      Right_Angle_Quotation       : constant Character := Character'Val (187);
      Fraction_One_Quarter        : constant Character := Character'Val (188);
      Fraction_One_Half           : constant Character := Character'Val (189);
      Fraction_Three_Quarters     : constant Character := Character'Val (190);
      Inverted_Question           : constant Character := Character'Val (191);

      --
      --  Character positions 192 (16#C0#) .. 207 (16#CF#):
      --

      UC_A_Grave                  : constant Character := Character'Val (192);
      UC_A_Acute                  : constant Character := Character'Val (193);
      UC_A_Circumflex             : constant Character := Character'Val (194);
      UC_A_Tilde                  : constant Character := Character'Val (195);
      UC_A_Diaeresis              : constant Character := Character'Val (196);
      UC_A_Ring                   : constant Character := Character'Val (197);
      UC_AE_Diphthong             : constant Character := Character'Val (198);
      UC_C_Cedilla                : constant Character := Character'Val (199);
      UC_E_Grave                  : constant Character := Character'Val (200);
      UC_E_Acute                  : constant Character := Character'Val (201);
      UC_E_Circumflex             : constant Character := Character'Val (202);
      UC_E_Diaeresis              : constant Character := Character'Val (203);
      UC_I_Grave                  : constant Character := Character'Val (204);
      UC_I_Acute                  : constant Character := Character'Val (205);
      UC_I_Circumflex             : constant Character := Character'Val (206);
      UC_I_Diaeresis              : constant Character := Character'Val (207);

      --
      --  Character positions 208 (16#D0#) .. 223 (16#DF#):
      --

      UC_Icelandic_Eth            : constant Character := Character'Val (208);
      UC_N_Tilde                  : constant Character := Character'Val (209);
      UC_O_Grave                  : constant Character := Character'Val (210);
      UC_O_Acute                  : constant Character := Character'Val (211);
      UC_O_Circumflex             : constant Character := Character'Val (212);
      UC_O_Tilde                  : constant Character := Character'Val (213);
      UC_O_Diaeresis              : constant Character := Character'Val (214);
      Multiplication_Sign         : constant Character := Character'Val (215);
      UC_O_Oblique_Stroke         : constant Character := Character'Val (216);
      UC_U_Grave                  : constant Character := Character'Val (217);
      UC_U_Acute                  : constant Character := Character'Val (218);
      UC_U_Circumflex             : constant Character := Character'Val (219);
      UC_U_Diaeresis              : constant Character := Character'Val (220);
      UC_Y_Acute                  : constant Character := Character'Val (221);
      UC_Icelandic_Thorn          : constant Character := Character'Val (222);
      LC_German_Sharp_S           : constant Character := Character'Val (223);

      --
      --  Character positions 224 (16#E0#) .. 239 (16#EF#):
      --

      LC_A_Grave                  : constant Character := Character'Val (224);
      LC_A_Acute                  : constant Character := Character'Val (225);
      LC_A_Circumflex             : constant Character := Character'Val (226);
      LC_A_Tilde                  : constant Character := Character'Val (227);
      LC_A_Diaeresis              : constant Character := Character'Val (228);
      LC_A_Ring                   : constant Character := Character'Val (229);
      LC_AE_Diphthong             : constant Character := Character'Val (230);
      LC_C_Cedilla                : constant Character := Character'Val (231);
      LC_E_Grave                  : constant Character := Character'Val (232);
      LC_E_Acute                  : constant Character := Character'Val (233);
      LC_E_Circumflex             : constant Character := Character'Val (234);
      LC_E_Diaeresis              : constant Character := Character'Val (235);
      LC_I_Grave                  : constant Character := Character'Val (236);
      LC_I_Acute                  : constant Character := Character'Val (237);
      LC_I_Circumflex             : constant Character := Character'Val (238);
      LC_I_Diaeresis              : constant Character := Character'Val (239);

      --
      --  Character positions 240 (16#F0#) .. 255 (16#FF#):
      --

      LC_Icelandic_Eth            : constant Character := Character'Val (240);
      LC_N_Tilde                  : constant Character := Character'Val (241);
      LC_O_Grave                  : constant Character := Character'Val (242);
      LC_O_Acute                  : constant Character := Character'Val (243);
      LC_O_Circumflex             : constant Character := Character'Val (244);
      LC_O_Tilde                  : constant Character := Character'Val (245);
      LC_O_Diaeresis              : constant Character := Character'Val (246);
      Division_Sign               : constant Character := Character'Val (247);
      LC_O_Oblique_Stroke         : constant Character := Character'Val (248);
      LC_U_Grave                  : constant Character := Character'Val (249);
      LC_U_Acute                  : constant Character := Character'Val (250);
      LC_U_Circumflex             : constant Character := Character'Val (251);
      LC_U_Diaeresis              : constant Character := Character'Val (252);
      LC_Y_Acute                  : constant Character := Character'Val (253);
      LC_Icelandic_Thorn          : constant Character := Character'Val (254);
      LC_Y_Diaeresis              : constant Character := Character'Val (255);

   end Latin_1;

   --       Signed     Signed One's   Signed Two's
   --      Magnitude    Complement     Complement
   --  +7    0111          0111          0111
   --  +6    0110          0110          0110
   --  +5    0101          0101          0101
   --  +4    0100          0100          0100
   --  +3    0011          0011          0011
   --  +2    0010          0010          0010
   --  +1    0001          0001          0001
   --  +0    0000          0000          0000
   --  -0    1000          1111          -
   --  -1    1001          1110          1111
   --  -2    1010          1101          1110
   --  -3    1011          1100          1101
   --  -4    1100          1011          1100
   --  -5    1101          1010          1011
   --  -6    1110          1001          1010
   --  -7    1111          1000          1001
   package Big_Endian is

      procedure To_Octets (Octets : in out Types.Byte_Array;
                           Value  : in     Types.Int32;
                           Index  : in out Types.Pos32);

      procedure To_Integer_32 (Octets : in     Types.Byte_Array;
                               Value  :    out Interfaces.Integer_32;
                               Index  : in out Types.Pos32);
      --  The code that calls this function can use the result of type
      --  Integer_32 to convert the value to Int32 if possible to do the
      --  type case without raising Constraint_Error exception.

   end Big_Endian;

   generic
      type Map_Key is range <>;
      Max_Octet_Count : Types.Pos32;
   package Bounded_Pos32_To_Octet_Array_Map is

      type Map is private;

      procedure Append (This        : in out Map;
                        Value       : Types.Byte_Array;
                        Key         : out Map_Key;
                        Call_Result : in out Types.Subprogram_Call_Result);

      function Value (This  : Map;
                      Index : Map_Key) return Types.Byte_Array;

      procedure Clear (M : out Map);

   private

      type Substring_T is record
         From : Types.Pos32 := 1;
         To   : Types.Nat32 := 0;
      end record;

      type Substring_Indexes is array (Map_Key) of Substring_T;

      type Map is record
         My_Huge_Text  : Types.Byte_Array (1 .. Max_Octet_Count);
         My_Next       : Types.Pos32 := 1;
         My_Next_Index : Map_Key := 1;
         My_Substrings : Substring_Indexes;
      end record;

   end Bounded_Pos32_To_Octet_Array_Map;

   generic
      type Map_Key is range <>;
      Max_Octet_Count : Types.Int32;
   package Bounded_Pos32_To_Octet_Array_Map2 is

      type Memory_Pool is limited private;

      type Memory_Pool_Ptr is access all Memory_Pool;
      for  Memory_Pool_Ptr'Storage_Size use 0;

      type Map is private;

      procedure Append (This        : in out Map;
                        Value       : Types.Byte_Array;
                        Key         : out Map_Key;
                        Call_Result : in out Types.Subprogram_Call_Result);

      function Value (This : Map; Index : Map_Key) return Types.Byte_Array;

      procedure Clear (M : out Map);

      function Empty_Map (Pool : Memory_Pool_Ptr) return Map;

   private

      type Substring_T is record
         From : Types.Pos32 := 1;
         To   : Types.Nat32 := 0;
      end record;

      type Substring_Indexes is array (Map_Key) of Substring_T;

      type Memory_Pool is limited record
         My_Huge_Text  : Types.Byte_Array (1 .. Types.Pos32 (Max_Octet_Count));
         My_Substrings : Substring_Indexes;
      end record;

      type Map is record
         Pool          : Memory_Pool_Ptr;
         My_Next       : Types.Pos32 := 1;
         My_Next_Index : Map_Key := 1;
      end record;

   end Bounded_Pos32_To_Octet_Array_Map2;

   generic
      type Key_Type is range <>;
      --  type Key_Type is new Pos32; --  is not supported by Janus/Ada

      type Value_Type is private;
      type Value_Const_Ptr is access constant Value_Type;
      type Values_Array is array
        (Types.Pos32 range <>) of Value_Const_Ptr;
      --  This array is made aliased to be compatible with arrays
      --  used in other containers.
      --  The components of this array type should be "not null" but is not
      --  supported by GNAT 3.14p.

      Default_Const_Ptr_Value : Value_Const_Ptr;
   package Bounded_Key_Array_Store is

      type Key_Array_Store
        (Initial_Keys_Count : Key_Type;
         --  Specifies the number of keys to preallocate memory for.
         --  This variable can be interpreted as the expected number of objects
         --  that has a collection of items.

         Initial_Values_Count : Types.Nat32
         --  Specifies the total number of values in all the collections.
        )
      is limited private;

      --     function Create_Key
      --       (This : access Key_Array_Store) return Key_Type;
      --  The signature of this function should have been
      --  function Create_Key (This : in out Key_Value_Store) ...
      --  but in-out function parameters were introduced in Ada 2012.
      --
      --  It can happen that this function cannot be used by the rules of the
      --  Ada language (the GNAT compiler complains "illegal attribute for
      --  discriminant-dependend component" and Janus/Ada complains
      --  "*ERROR* A field which depends on a discriminant may
      --  not be renamed (6.4.19) [RM 3.10.2(26)]". In this case use
      --  the procedure version of this function instead.

      procedure Create_Key
        (This : in out Key_Array_Store;
         Key  : out Key_Type);

      procedure Add_To_Array
        (This    : in out Key_Array_Store;
         Key     : Key_Type;
         Element : Value_Const_Ptr);

      function Get_Array
        (This : Key_Array_Store;
         Key  : Key_Type) return Values_Array;
      --  This function is named Get_Array instead of Array because array is
      --  a reserved word in Ada. The motivation for naming this function
      --  Array is that it would be analogous to the naming convention used
      --  in the key-value store Generic_Unbounded_Key_Value where the
      --  corresponding function is called Value.

   private

      type Linked_List_Node is record
         Element : Value_Const_Ptr;
         Next    : Types.Nat32;
         --  Specifies the index for the next element in the collection.
         --  The value zero means there are no more elements in the collection.
      end record;

      type Linked_List_Node_Array is
        array (Types.Pos32 range <>) of aliased Linked_List_Node;

      type Key_Item is record
         First_Index : Types.Nat32;
         Last_Index  : Types.Nat32;
      end record;

      type Key_Item_Array is array (Key_Type range <>) of aliased Key_Item;

      subtype Extended_Key_Type is Key_Type'Base range 0 .. Key_Type'Last;

      type Key_Array_Store
        (Initial_Keys_Count   : Key_Type;
         Initial_Values_Count : Types.Nat32)
      is limited record
         List : aliased Linked_List_Node_Array (1 .. Initial_Values_Count);
         Last_List_Index : Types.Nat32 := 0;
         Keys : aliased Key_Item_Array (1 .. Initial_Keys_Count)
           := (others => (First_Index => 0, Last_Index => 0));
         Last_Key_Index : Extended_Key_Type := Extended_Key_Type'First;
         Next_Available_List_Index : Types.Pos32 := 1;
      end record;

   end Bounded_Key_Array_Store;

   --  This package is not to be used directly but contains type definitions
   --  used for UTF8 support. The Make_Table function defined is a large piece
   --  of code and having all the UTF8 code in one single package became
   --  to much code for the Janus/Ada compiler to handle.
   package Pre_Utf8_Defs is

      type Code_Point_Base is mod 2**32;
      subtype Code_Point is Code_Point_Base range 0  .. 16#10FFFF#;
      --  Here a new numerical type is introduced and
      --  for the first 127 code points it corresponds to ASCII characters.
      --  One will want to have an easy way to compare code points with
      --  ASCII characters.

      type Categorization is record
         Code  : Code_Point;
         Upper : Code_Point;
         Lower : Code_Point;
      end record;
      type Categorization_Index is range 1 .. 3070;
      type Categorization_Array is array
        (Categorization_Index) of Categorization;

      function Make_Table return Categorization_Array;

   end Pre_Utf8_Defs;

end Stda;
