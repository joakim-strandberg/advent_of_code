with Ada.Text_IO;
with Ada.Finalization;

package Stda is

   package Types is

      Infinite_Loop_Detected : exception;

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
      --        Ada.Exceptions.Raise_Exception (Constraint_Error'Identity,
      --                                        "infinite loop detected");
      --     end if;
      --  end;

      type Int32 is range -2**31 + 1 .. (2**31 - 1);
      --  This type is defined to make the value -2*31 invalid
      --  in order to easily find usage of uninitialized variables
      --  through pragma Normalize_Scalars (..).

      subtype Pos16 is Int32 range 1 .. 2 ** 15 - 1;
      subtype Pos32 is Int32 range 1 .. Int32'Last;
      subtype Nat32 is Int32 range 0 .. Int32'Last;

      type Byte_Type is range 0 .. 255;
      --  This byte definition is cross-compiler.

      type Byte_Array is array (Pos32 range <>) of Byte_Type;

      type Ada_Code_Location is record
         Code_1 : Int32;
         Code_2 : Int32;
      end record;
      --  Is intended to be used in situations where caller of a subprogram
      --  may want to act differently depending on what issue has arisen
      --  when calling a subprogram.

   end Types;

   use Types;

   --  This package has been created to move subprograms out of the Types
   --  package. Contains conversion routines between the types defined
   --  in the Types package and types defined in the Ada standard.
   package Conversions is

      function To_String (This : Ada_Code_Location) return String;

      function To_Byte (Char : Character) return Byte_Type;

      function To_Byte_Array (Text : String) return Byte_Array;

   end Conversions;

   package File_IO is

      --  Instances of this type are expected to be used inside
      --  bodies of packages (usage of information hiding to increase
      --  confidence in that file handles are used correctly,
      --  no file handle resource leakage.
      type Text_File is new Ada.Finalization.Limited_Controlled with record
         File : Ada.Text_IO.File_Type;
      end record;

      procedure Finalize (File : in out Text_File);

   end File_IO;

   --  Originates from the Ada95 Reference Manual.
   --
   --  The reason this package exists is because Ada.Characters.Latin_1
   --  is not implemented in the Janus/Ada compiler.
   package Latin_1 is

      subtype Character_As_Int32 is Int32 range 0 .. 255;

      function To_Int32 (Value : Character) return Character_As_Int32;

      function Is_Graphic_Character (Value : Character_As_Int32) return Boolean;

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

   package String_Split is

      --  This record type contains the substring indices into an existing
      --  String. Consider the String "Hello World!" where the first
      --  String index is 1 and the last index is 12.
      --  First = 7 and Last = 11 denotes the substring "World".
      --  Natural is the index type used in standard Ada Strings.
      type Index_Interval is record
         First : Natural;
         Last  : Natural;
      end record;

      type Interval_Array_Index is range 1 .. 256;

      type Index_Interval_Array is array
        (Interval_Array_Index range <>) of Index_Interval;

      type Variable_Length_Interval_Array
        (Last : Interval_Array_Index)
      is record
         Interval : Index_Interval_Array (1 .. Last);
      end record;

      procedure Put (Interval : Variable_Length_Interval_Array);
      --  Prints to standard out.

      function Get_Slices
        (Line   : String;
         Sep    : Character) return Variable_Length_Interval_Array;

   end String_Split;


   package UTF8 is

      procedure Initialize;
      --  Must be called before usage of this package.
      --
      --  General_Category of a code point according to the  Unicode  character
      --  database. The names of the enumeration correspond to the names in the
      --  database.
      --
      type General_Category is
        (Lu, --  Letter, Uppercase
         Ll, --         Lowercase
         Lt, --         Titlecase
         Lm, --         Modifier
         Lo, --         Other

         Mn, -- Mark, Nonspacing
         Mc, --       Spacing Combining
         Me, --       Enclosing

         Nd, -- Number, Decimal Digit
         Nl, --         Letter
         No, --         Other

         Pc, -- Punctuation, Connector
         Pd, --              Dash
         Ps, --              Open
         Pe, --              Close
         Pi, --              Initial quote
         Pf, --              Final quote
         Po, --              Other

         Sm, -- Symbol, Math
         Sc, --         Currency
         Sk, --         Modifier
         So, --         Other

         Zs, -- Separator, Space
         Zl, --            Line
         Zp, --            Paragraph

         Cc, -- Other, Control
         Cf, --        Format
         Cs, --        Surrogate
         Co, --        Private Use
         Cn  --        Not Assigned
        );
      --
      --  Classes of categories
      --
      subtype Letter      is General_Category range Lu .. Lo;
      subtype Mark        is General_Category range Mn .. Me;
      subtype Mumber      is General_Category range Nd .. No;
      subtype Punctuation is General_Category range Pc .. Po;
      subtype Symbol      is General_Category range Sm .. So;
      subtype Separator   is General_Category range Zs .. Zp;
      subtype Other       is General_Category range Cc .. Cn;

      type Code_Point_Base is mod 2**32;
      subtype Code_Point is Code_Point_Base range 0  .. 16#10FFFF#;
      --  Here a new numerical type is introduced and
      --  for the first 127 code points it corresponds to ASCII characters.
      --  One will want to have an easy way to compare code points with
      --  ASCII characters.

      subtype Code_Point_Str32_Length is Pos32 range 1 .. 4;
      --  Length of a Str32 corresponding to a specific code point.

      --
      --  Image -- Of an UTF-8 code point
      --
      --    Value - The code point
      --
      --  Returns :
      --
      --    UTF-8 encoded equivalent
      --
      function Image (Value : Code_Point) return Byte_Array;

      function Image (Value : Code_Point) return String;

      --
      --  Has_Case -- Case test
      --
      --    Value - Code point
      --
      --  Returns :
      --
      --    True if Value has either an  upper  or  a  lower  case  equivalent
      --    different from Code.
      --
      function Has_Case (Value  : Code_Point) return Boolean;
      --
      --  Is_Lowercase -- Case test
      --
      --    Value - Code point
      --
      --  Returns :
      --
      --    True if Value is a lower case point
      --
      function Is_Lowercase (Value  : Code_Point) return Boolean;

      --
      --  Is_Uppercase -- Case test
      --
      --    Value - Code point
      --
      --  Returns :
      --
      --    True if Value is a lower case point
      --
      function Is_Uppercase (Value  : Code_Point) return Boolean;

      --
      --  To_Lowercase -- Convert to lower case
      --
      --    Value - Code point or UTF-8 encoded Str32
      --
      --  Returns :
      --
      --    The lower case eqivalent or else Value itself
      --
      function To_Lowercase (Value  : Code_Point) return Code_Point;
      --
      --  To_Uppercase -- Convert to upper case
      --
      --    Value - Code point or UTF-8 encoded Str32
      --
      --  Returns :
      --
      --    The upper case eqivalent or else Value itself
      --
      function To_Uppercase (Value  : Code_Point) return Code_Point;

      --
      --  Category -- Get category of a code point
      --
      --    Value - Code point
      --
      --  Returns :
      --
      --    The category of value
      --
      function Category (Value  : Code_Point) return General_Category;

      --
      --  Is_* -- Category tests
      --
      function Is_Alphanumeric (V  : Code_Point) return Boolean;
      function Is_Other_Format (V  : Code_Point) return Boolean;

      function Is_Digit   (Value : Code_Point) return Boolean;
      function Is_Control (Value : Code_Point) return Boolean;
      function Is_Letter  (Value : Code_Point) return Boolean;
      function Is_Lower   (Value : Code_Point) return Boolean;
      function Is_Space   (Value : Code_Point) return Boolean;
      function Is_Title   (Value : Code_Point) return Boolean;
      function Is_Upper   (Value : Code_Point) return Boolean;

      function Is_ISO_646 (Value : in Code_Point) return Boolean;
      --
      --  Special digits
      --
      function Is_Subscript_Digit (Value : in Code_Point) return Boolean;

      function Is_Superscript_Digit (Value : in Code_Point) return Boolean;

      --
      --  Ada 2005 identifier sets
      --
      --    identifier_start,  see ARM 2.3(3/2)
      --    identifier_extend, see ARM 2.3(3.1/2)
      --
      function Is_Identifier_Start  (Value : Code_Point) return Boolean;
      function Is_Identifier_Extend (Value : Code_Point) return Boolean;

      procedure Put (Destination : in out Byte_Array;
                     Pointer     : in out Pos32;
                     Value       : Code_Point);
      --
      --  Put -- Put one UTF-8 code point
      --
      --    Destination - The target Str32
      --    Pointer     - The position where to place the character
      --    Value       - The code point to put
      --
      --  This  procedure  puts  one  UTF-8  code  point into the Str32 Source
      --  starting from the position Source (Pointer). Pointer is then advanced
      --  to the first character following the output.
      --

      function Is_Valid_UTF8_Code_Point (Source  : Byte_Array;
                                         Pointer : Pos32) return Boolean;

      --
      --  Get -- Get one UTF-8 code point
      --
      --    Source  - The source Str32
      --    Pointer - The Str32 position to start at
      --    Value   - The result
      --
      --   This  procedure  decodes one UTF-8 code point from the Source.
      --   It starts at Source (Pointer). After successful completion
      --   Pointer is advanced to the first character following the input.
      --   The result is returned through the parameter Value.
      --
      procedure Get (Source  : Byte_Array;
                     Pointer : in out Pos32;
                     Value   : out Code_Point);

      function Is_Valid_UTF8 (Source : Byte_Array) return Boolean;

      --
      --  Length -- The length of an UTF-8 Str32
      --
      --    Source - The Str32 containing UTF-8 encoded code points
      --
      --  Returns :
      --
      --    The number of UTF-8 encoded code points in Source
      --
      function Length (Source : Byte_Array) return Nat32;

      function To_Lowercase (V : Byte_Array) return Byte_Array;
      function To_Uppercase (V : Byte_Array) return Byte_Array;

   private

      pragma Inline (Is_Alphanumeric);
      pragma Inline (Is_Control);
      pragma Inline (Is_Digit);
      pragma Inline (Is_ISO_646);
      pragma Inline (Is_Letter);
      pragma Inline (Is_Lower);
      pragma Inline (Is_Title);
      pragma Inline (Is_Upper);
      pragma Inline (Is_Subscript_Digit);
      pragma Inline (Is_Superscript_Digit);
      pragma Inline (Is_Identifier_Start);
      pragma Inline (Is_Identifier_Extend);

   end UTF8;

end Stda;
