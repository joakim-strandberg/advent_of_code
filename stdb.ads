with Stda;
pragma Elaborate_All (Stda);

package Stdb is

   package Types renames Stda.Types;

   use type Types.Int32;

   type Max_Hash_Map_Size_T is range 3 .. 2_147_483_647;  --  2**31 - 1

   generic
      type Key_T is private;
      type Element_T is private;

      with function Default_Key return Key_T;
      with function Default_Element return Element_T;

      with function Hash (Key : Key_T) return Types.Hash32;
      with function Equivalent_Keys (Left, Right : Key_T) return Boolean;

      Max_Hash_Map_Size : Max_Hash_Map_Size_T;

      Max_Collision_List_Size : Types.Int32 := 0;
   package Bounded_Hash_Map is

      type Map_Type is private;

      procedure Insert (This        : in out Map_Type;
                        Key         : Key_T;
                        New_Element : Element_T);

      function Element (This : Map_Type;
                        Key  : Key_T) return Element_T;

      function Exists (This : Map_Type;
                       Key  : Key_T) return Boolean;

      function Used_Capacity (This : Map_Type) return Natural;

      type Find_Element_Result_T (Exists : Boolean) is
         record
            case Exists is
               when True  => Element : Element_T;
               when False => null;
            end case;
         end record;

      function Find_Element (This : Map_Type;
                             Key  : Key_T) return Find_Element_Result_T;

      procedure Delete (This : in out Map_Type;
                        Key  : Key_T);

   private

      type Node_T is record
         Key     : Key_T;
         Element : Element_T;
      end record;

      type Nullable_Node_T (Exists : Boolean := False) is record
         case Exists is
            when True  => Value : Node_T;
            when False => null;
         end case;
      end record;

      subtype Bucket_Index_T is Types.Hash32 range
        Types.Hash32'(0) .. Types.Hash32 (Max_Hash_Map_Size - 1);

      type Bucket_Array_T is array (Bucket_Index_T) of Nullable_Node_T;

      function Default_Node return Node_T;

      subtype Collision_Index is Types.Pos32 range
        1 .. 1 + Max_Collision_List_Size;

      package Coll_Vecs is new Stda.Bounded_Vectors
        (Element_Type  => Node_T,
         Index         => Collision_Index);

      type Map_Type is record
         Buckets        : Bucket_Array_T := (others => (Exists => False));
         Collision_List : Coll_Vecs.Vector;
      end record;

      function Normalize_Index (H : Types.Hash32) return Bucket_Index_T;

   end Bounded_Hash_Map;

   package UTF8 is
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

      type UTF8_Tables is private;
      --  Contains two tables of data that is of private use
      --  of the UTF8 functionality.
      --  There should only be one instance of this type
      --  in the whole application that needs UTF8 functionality.

      procedure Initialize (This : out UTF8_Tables);
      --  Creates and initializes an instance of the type UTF8_Tables.

      subtype Code_Point_Base is Stda.Pre_Utf8_Defs.Code_Point_Base;
      subtype Code_Point      is Stda.Pre_Utf8_Defs.Code_Point;
      --  Here a new numerical type is introduced and
      --  for the first 127 code points it corresponds to ASCII characters.
      --  One will want to have an easy way to compare code points with
      --  ASCII characters.

      subtype Code_Point_Str32_Length is Stda.Types.Pos32 range 1 .. 4;
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
      function Image (Value : Code_Point) return Types.Byte_Array;

      function Image (Value : Code_Point) return String;

      function Image (Value : Types.Byte_Array) return String;

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
      function Has_Case (Tables : UTF8_Tables;
                         Value  : Code_Point) return Boolean;
      --
      --  Is_Lowercase -- Case test
      --
      --    Value - Code point
      --
      --  Returns :
      --
      --    True if Value is a lower case point
      --
      function Is_Lowercase (Tables : UTF8_Tables;
                             Value  : Code_Point) return Boolean;

      --
      --  Is_Uppercase -- Case test
      --
      --    Value - Code point
      --
      --  Returns :
      --
      --    True if Value is a lower case point
      --
      function Is_Uppercase (Tables : UTF8_Tables;
                             Value  : Code_Point) return Boolean;

      --
      --  To_Lowercase -- Convert to lower case
      --
      --    Value - Code point or UTF-8 encoded Str32
      --
      --  Returns :
      --
      --    The lower case eqivalent or else Value itself
      --
      function To_Lowercase (Tables : UTF8_Tables;
                             Value  : Code_Point) return Code_Point;
      --
      --  To_Uppercase -- Convert to upper case
      --
      --    Value - Code point or UTF-8 encoded Str32
      --
      --  Returns :
      --
      --    The upper case eqivalent or else Value itself
      --
      function To_Uppercase (Tables : UTF8_Tables;
                             Value  : Code_Point) return Code_Point;

      --
      --  Category -- Get category of a code point
      --
      --    Value - Code point
      --
      --  Returns :
      --
      --    The category of value
      --
      function Category (Tables : UTF8_Tables;
                         Value  : Code_Point) return General_Category;

      --
      --  Is_* -- Category tests
      --
      function Is_Alphanumeric (T : UTF8_Tables;
                                V  : Code_Point) return Boolean;
      function Is_Other_Format (T : UTF8_Tables;
                                V  : Code_Point) return Boolean;

      function Is_Digit   (T : UTF8_Tables; Value : Code_Point) return Boolean;
      function Is_Control (T : UTF8_Tables; Value : Code_Point) return Boolean;
      function Is_Letter  (T : UTF8_Tables; Value : Code_Point) return Boolean;
      function Is_Lower   (T : UTF8_Tables; Value : Code_Point) return Boolean;
      function Is_Space   (T : UTF8_Tables; Value : Code_Point) return Boolean;
      function Is_Title   (T : UTF8_Tables; Value : Code_Point) return Boolean;
      function Is_Upper   (T : UTF8_Tables; Value : Code_Point) return Boolean;

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
      function Is_Identifier_Start  (Tables : UTF8_Tables; Value : Code_Point)
                                  return Boolean;
      function Is_Identifier_Extend (Tables : UTF8_Tables; Value : Code_Point)
                                  return Boolean;

      procedure Put (Destination : in out Types.Byte_Array;
                     Pointer     : in out Types.Pos32;
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

      function Is_Valid_UTF8_Code_Point (Source  : Types.Byte_Array;
                                         Pointer : Types.Pos32) return Boolean;

      --
      --  Get -- Get one UTF-8 code point
      --
      --    Source  - The source Str32
      --    Pointer - The Str32 position to start at
      --    Value   - The result
      --
      --   This  procedure  decodes one UTF-8 code point from the Str32 Source.
      --   It starts at Source (Pointer). After successful completion
      --   Pointer is advanced to the first character following the input.
      --   The result is returned through the parameter Value.
      --
      procedure Get (Source  : Types.Byte_Array;
                     Pointer : in out Types.Pos32;
                     Value   : out Code_Point);

      function Is_Valid_UTF8 (Source : Types.Byte_Array) return Boolean;

      --
      --  Length -- The length of an UTF-8 Str32
      --
      --    Source - The Str32 containing UTF-8 encoded code points
      --
      --  Returns :
      --
      --    The number of UTF-8 encoded code points in Source
      --
      function Length (Source : Types.Byte_Array) return Types.Nat32;

      function To_Lowercase (T : UTF8_Tables;
                             V : Types.Byte_Array) return Types.Byte_Array;

      function To_Uppercase (T : UTF8_Tables;
                             V : Types.Byte_Array) return Types.Byte_Array;

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

      subtype Categorization       is Stda.Pre_Utf8_Defs.Categorization;
      subtype Categorization_Index is Stda.Pre_Utf8_Defs.Categorization_Index;
      subtype Categorization_Array is Stda.Pre_Utf8_Defs.Categorization_Array;

      type Points_Range is record
         From     : Code_Point;
         To       : Code_Point;
         Category : General_Category;
      end record;
      type Range_Index is range 1 .. 2077;
      type Range_Array is array (Range_Index) of Points_Range;

      type UTF8_Tables is record
         Categorization_Table : Categorization_Array;
         Points_Table         : Range_Array;
      end record;

   end UTF8;

   ----------------------------------------------------------------------------
   --  File:           multi_precision_integers.ads
   --
   --  Description:    Multiple precision integers package
   --
   --  Date/version:   Aug-2007: - No more generics (Long_Block_type,
   --                              Block_type,... always the largest possible
   --                              idea: J.C.)
   --                            - Fixed Basic(...) (based on J.C.'s remarks)
   --                  Nov-2006: - unsigned types for blocks
   --                            - a block uses 2 bits more
   --                            - Ada95+ only
   --                  Mar-2002: Bugs fixed related to zero field, division
   --                  Nov-1999: - procedures (no stack, less copies !)
   --                            - new data structure
   --                  Dec-1996: First version (operators only)
   --
   --  Author:         G. de Montmollin, Univ. Neuchatel
   --
   --  Thanks to:      Duncan Sands, Univ. Paris-Sud, CNRS
   --                  Jeffrey R. Carter
   --
   --  Tested on:      Intel 586 (32 bit) - Windows 98, NT4+ - GNAT 3.13p+
   --                  Intel 586 (32 bit) - Windows 98, NT4+ - ObjectAda 7.2.1+
   --                  Alpha-AXP (64 bit) - OpenVMS 7.1      - Compaq Ada
   --
   --  Division algorithm adaptated from BigInt 1.0 library,
   --  by Stephen Adams, that refers to
   --  D. E. Knuth, the Art of computer programming
   --  volume 2, "Seminumerical Algorithms"
   --  section 4.3.1, "Multiple-Precision Arithmetic"
   --
   ----------------------------------------------------------------------------

   --  https://medium.com/better-programming/
   --  how-prime-numbers-keep-the-internet-secure-680cc1743133
   --  The article says that checking if a large number is prime is very hard,
   --  and therefore we use it to power encryption. This premise is wrong.
   --  Checking if a number is prime, isn't hard by computer standards.
   --  The actual hard problem we are using to power encryption is
   --  "given a large mystery number X that is the product of two large primes
   --  Y and Z, find the Y and Z" - also known as "prime factorization" (which
   --  you mention in the article). Also, it is easy to multiple
   --  two large numbers Y and Z, to see if it equals X - ie.
   --  if the answers given were correct.
   --  Last of all, each large number always only has a single way
   --  to be expressed as a product of primes.
   --
   --  In fact, encryption being powered by prime numbers RELIES on
   --  it being easy to distinguish whether a large number is prime,
   --  quickly. That's how we generate new Ys and Zs to create
   --  the above scenario.
   --
   --  To summarize:
   --
   --  Finding prime factors of a large number is hard. This is what gives
   --  the difficulty of breaking the encryption.
   --
   --  Every large number only has a single way to be factored into primes.
   --  This is what ensures there is only "1 correct answer" to the encrpytion.
   --
   --  Multiplying two large numbers is fast - this is what makes it easy
   --  to check the result.
   --
   --  Big primes are easy to generate. This is what makes it practical
   --  to use in encryption.
   --
   --  One of the main reasons there is so much hype around Quantum computers,
   --  is because #1 no longer holds - there are known ways for
   --  Quantum computers to factor large numbers quickly.
   --
   --  That said, the rest of the article going through RSA etc.
   --  is good material for folks wondering how it works. Thanks for sharing.

   --  You encrypt with e, and decrypt with d (that's why those names
   --  were chosen). Specifically, to encrypt your message m,
   --  you compute c = m ** (e) mod pq.
   --
   --  To decrypt your message you compute
   --  c ** (d) = (m ** (e) ) ** (d) mod pq
   --    = m ** (ed) mod pq = m ** (1) mod pq = m mod pq.
   --  The important thing is that the computation c**d is enough,
   --  but mathematically we can see (from the properties of e, d)
   --  that the result is always just m.
   --
   --  The numbers e and d are symmetric, so you could instead
   --  "encrypt with d and decrypt with e", but the important thing is that you
   --  don't reveal both.
   --
   --  If someone knows one of them, that tells them (essentially) nothing.
   --  It's just a random number.
   --
   --  But if you know both, then you can compute p and q very easily,
   --  which breaks the cryptosystem.
   --
   --  This is a 32-bit version of the Big_Integers package which means
   --  it can be compiled by both 32- and 64-bit compilers which is good,
   --  but does not take advantage of potential benefit of a 64-bit specific
   --  implementation.
   package Big_Integers is

      -- Integers for array indexing --

      subtype Index_int is Integer;

      -- THE multi-precision integer type --

      type Multi_int (n : Index_int) is private;

      -- Integer type for small values --

      subtype Basic_int is Integer; -- the "normal" signed integer

      ----------------------------------------------------------------
      -- Debug mode: checks results of arithmetic operations and    --
      -- Multi_int variables' integrity.                            --
      -- CAUTION: Debug = True reduces monstruously the performance --
      ----------------------------------------------------------------

      Debug : constant Boolean := False;

      ---------------------------------------------
      ----- Informations, conversions, filling ----
      ---------------------------------------------

      --  Convert Basic_int to Multi_int
      function Multi (small : Basic_int) return Multi_int;

      --  Convert Multi_int to Basic_int
      --  (when possible, else: Cannot_fit raised)
      function Basic (large : Multi_int) return Basic_int;

      --  Fill an Multi_int of greater array dimension with a smaller one
      procedure Fill (what : out Multi_int; with_smaller : Multi_int);
      procedure Fill (what : out Multi_int; with_basic : Basic_int);

      --  Comparisons
      function Equal (i1, i2 : Multi_int) return Boolean;
      function Equal (i1 : Multi_int; i2 : Basic_int) return Boolean;
      function ">" (i1, i2 : Multi_int) return Boolean;
      function ">" (i1 : Multi_int; i2 : Basic_int) return Boolean;
      function "<" (i1, i2 : Multi_int) return Boolean;
      function "<" (i1 : Multi_int; i2 : Basic_int) return Boolean;
      function ">=" (i1, i2 : Multi_int) return Boolean;
      function ">=" (i1 : Multi_int; i2 : Basic_int) return Boolean;
      function "<=" (i1, i2 : Multi_int) return Boolean;
      function "<=" (i1 : Multi_int; i2 : Basic_int) return Boolean;

      --  Other informations
      function Bits_per_block return Positive;

      -------------------------------------------------------------------------
      ------- Arithmetic operators.                                   ---------
      ------- For speed, the "procedure" variants should be preferred ---------
      -------------------------------------------------------------------------

      ---------------------------
      ----- Unary operators -----
      ---------------------------

      procedure Opp (i : in out Multi_int);
      function "+" (i : Multi_int) return Multi_int;
      function "-" (i : Multi_int) return Multi_int;

      procedure Abso (i : in out Multi_int);
      function "ABS" (i : Multi_int) return Multi_int;

      function Sign (i : Multi_int) return Basic_int;
      function Even (i : Multi_int) return Boolean;
      function Odd (i : Multi_int) return Boolean;

      ----------------------------
      ----- Binary operators -----
      ----------------------------

      ---------------------------
      -- Addition, subtraction --
      ---------------------------

      procedure Add (i1, i2 : in Multi_int; i3 : in out Multi_int);

      function "+" (i1, i2 : Multi_int) return Multi_int;
      function "+" (i1 : Multi_int; i2 : Basic_int) return Multi_int;
      function "+" (i1 : Basic_int; i2 : Multi_int) return Multi_int;

      procedure Sub      (i1, i2 : in Multi_int; i3 : in out Multi_int);
      procedure Subtract (i1, i2 : in Multi_int; i3 : in out Multi_int)
                          renames Sub;

      function "-" (i1, i2 : Multi_int) return Multi_int;
      function "-" (i1 : Multi_int; i2 : Basic_int) return Multi_int;
      function "-" (i1 : Basic_int; i2 : Multi_int) return Multi_int;

      --------------------
      -- Multiplication --
      --------------------

      procedure Multiply (i1, i2 : in Multi_int; i3 : in out Multi_int);
      procedure Mult     (i1, i2 : in Multi_int; i3 : in out Multi_int)
                          renames Multiply;

      procedure Multiply
        (i1 : in Multi_int; i2 : Basic_int; i3 : in out Multi_int);
      procedure Mult
        (i1 : in Multi_int; i2 : Basic_int; i3 : in out Multi_int)
         renames Multiply;

      function "*" (i1, i2 : Multi_int) return Multi_int;
      function "*" (i1 : Multi_int; i2 : Basic_int) return Multi_int;
      function "*" (i1 : Basic_int; i2 : Multi_int) return Multi_int;

      -------------------------
      -- Division, Remainder --
      -------------------------

      procedure Div_Rem (i1 : in     Multi_int; i2 : in     Basic_int;
                         q  :    out Multi_int; r :    out Basic_int);
      procedure Div_Rem (i1, i2 : in Multi_int;  q, r :   out Multi_int);

      procedure Divide (i1, i2 : in Multi_int; q : out Multi_int);

      function "/" (i1, i2 : Multi_int) return Multi_int;
      function "/" (i1 : Multi_int; i2 : Basic_int) return Multi_int;
      function "Rem" (i1, i2 : Multi_int) return Multi_int;
      function "Rem" (i1 : Multi_int; i2 : Basic_int) return Multi_int;
      function "Rem" (i1 : Multi_int; i2 : Basic_int) return Basic_int;
      function "Mod" (i1, i2 : Multi_int) return Multi_int;
      function "Mod" (i1 : Multi_int; i2 : Basic_int) return Multi_int;
      function "Mod" (i1 : Multi_int; i2 : Basic_int) return Basic_int;

      -----------
      -- Power --
      -----------

      procedure Power (i : Multi_int; n : Natural; ipn : out Multi_int);
      function "**" (i : Multi_int; n : Natural) return Multi_int;
      --  + 26-Mar-2002 :
      procedure Power (i : Multi_int; n : Multi_int; ipn : out Multi_int;
                       modulo : Multi_int);

      Cannot_fit, Empty_multi_int : exception;

      Array_too_small : exception;

      Result_undersized,
      Quotient_undersized,
      Remainder_undersized : exception;

      Division_by_zero : exception;

      Zero_power_zero, Power_negative : exception;
      Power_modulo_non_positive : exception;

      -------------------------------------------------------------------------
      --  File:            Multi_precision_integers-IO.ads
      --  Description:     Child of package 'Multi_precision_integers: I/O
      --  Date/version:    2006 ; 15-Feb-2002 / 22.XI.1999 / 22.12.1996
      --  Author:          Gautier de Montmollin
      -------------------------------------------------------------------------
      package IO is

         subtype Number_Base is Integer range 2 .. 16;

         Default_Base : constant Number_Base := 10;

         --  Returns the number of digits in the specified base:
         function Number_of_digits
           (i : Multi_int; base : Number_Base := 10) return Natural;

         --  Returns the image of i in the specified base:
         function Str (i : Multi_int; base : Number_Base := 10) return String;

         --  Returns the value of number in string:
         function Val (s : String) return Multi_int;

         --  Output to file, in block format:
         --     procedure Put_in_blocks (File  : in File_Type;
         --                            Item  : in Multi_int);

         --  Output to standard input, in block format:
         --   procedure Put_in_blocks (Item  : in Multi_int);

         ---- The following mimic the Text_IO.Integer_IO

         --  Get from file:
         --     procedure Get (File  : in  File_Type;
         --                  Item  : out Multi_int;
         --                  Width : in Field := 0);

         --  Get from standard input:
         --     procedure Get (Item  : out Multi_int;
         --                  Width : in  Field := 0);

         --  Put to file:
         --     procedure Put (File  : in File_Type;
         --                  Item  : in Multi_int;
         --                  Width : in Field := 0;
         --                  Base  : in Number_Base := Default_Base);
         --  Width=0 : no default formatting, since inpredicatble length

         --  Put to standard output:
         --     procedure Put (Item  : in Multi_int;
         --                  Width : in Field := 0;
         --                  Base  : in Number_Base := Default_Base);
         --  Width=0 : no default formatting, since inpredicatble length

         --  Get from string:
         --    procedure Get(From : in  String;
         --                  Item : out Multi_int;
         --                  Last : out Positive);

         --  Put to string:
         --     procedure Put (To   : out String;
         --                  Item : in Multi_int;
         --                  Base : in Number_Base := Default_Base);

      end IO;

   private

      --> Long_Block_type is used for + and * of blocks.
      --  It is by design
      --    a/ the largest possible modular integer, and
      --    b/ twice the size of Block_type defined below.
      --  With the double of bits (2n) one can store m**2 + 2m without overflow
      --    where m = 2**n - 1 is the largest value possible on n bits.
      type Long_Block_type is mod 2 ** 32;

      --> Same size as Long_Block_type, but signed:
      type Long_Block_type_signed is range
        -2**(Long_Block_type'Size - 1) .. 2**(Long_Block_type'Size - 1) - 1;

      --> Block_type: unsigned integer used to store a chunk of a Multi_int.
      type Block_type is mod 2 ** (Long_Block_type'Size / 2);

      Block_type_bits : constant := Block_type'Size;

      cardblock : constant := 2 ** Block_type_bits;
      --  Number of possible values
      maxblock : constant := Block_type'Last;
      --  NB: GNAT (2006) optimizes out correctly
      --  the Block_type(l and maxblock_long) to Block_type(l),
      --  the latter form being caught when range checks are on.

      type Block_array is array (Index_int range <>) of Block_type;
      --  2006: was "of Basic_int" for obscure reasons...

      type Multi_int (n : Index_int) is record
         blk :       Block_array (0 .. n); -- the n blocks with ABSOLUTE value
         neg :       Boolean;        -- negative flag
         zero :      Boolean := True;-- zero flag (supercedes the other fields)
         last_used : Index_int;      -- the others blocks are supposed 0
      end record;

      --  NB the `zero' field supercedes EVERY other information
      --  (last_used, neg)

      -------------------------------------------------------------------------
      --   Format of type Multi_int.blk: ( i_0, i_1, ..., i_k, *, ..., * )   --
      --   i_0..i_k are >=0 ; others (*) are treated as 0                    --
      -------------------------------------------------------------------------

      --  Some internal procedures use by check:

      procedure Multiply_internal_copy_export
        (i1, i2 : in Multi_int; i3 : in out Multi_int);

      procedure Div_Rem_internal_both_export
        (i1, i2 : in Multi_int; q, r : in out Multi_int);

   end Big_Integers;

end Stdb;
