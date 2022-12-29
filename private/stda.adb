with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Exceptions;
with Stda.A;
with Stda.B;
with Stda.C;
with Stda.D;
with Stda.E;

package body Stda is

   package Positive_IO is new Ada.Text_IO.Integer_IO (Positive);

   package Nat32_IO is new Ada.Text_IO.Integer_IO (Nat32);

   use type Ada.Text_IO.Count;

   procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1) renames
     Ada.Text_IO.New_Line;

   procedure Put_Line (Item : String) renames Ada.Text_IO.Put_Line;

   procedure Put (Item : String) renames Ada.Text_IO.Put;

   procedure Put (Number : Positive) is
   begin
      Positive_IO.Put (Item  => Number,
                       Width => 0);
   end Put;

   procedure Put_Repeated (Item   : String;
                           Repeat : Pos32) is
   begin
      for I in Pos32 range 1 .. Repeat loop
         Put (Item);
      end loop;
   end Put_Repeated;

   package body Conversions is

      function To_String (This : Ada_Code_Location) return String renames
        A.To_String;

      function To_Byte (Char : Character) return Byte_Type is
      begin
         return Character'Pos (Char);
      end To_Byte;

      function To_Byte_Array (Text : String) return Byte_Array is
         Byte : Byte_Array (1 .. Pos32 (Text'Last));
      begin
         for I in Text'Range loop
            Byte (Pos32 (I)) := Character'Pos (Text (I));
         end loop;
         return Byte;
      end To_Byte_Array;

   end Conversions;

   package body File_IO is

      procedure Finalize (File : in out Text_File) is
      begin
         if Ada.Text_IO.Is_Open (File.File) then
            Ada.Text_IO.Close (File.File);
         end if;
      end Finalize;

   end File_IO;

   function To_String (This : Ada_Code_Location) return String renames
     Conversions.To_String;

   package body Latin_1 is

      function To_Int32 (Value : Character) return Character_As_Int32 is
      begin
         return Character'Pos (Value);
      end To_Int32;

      function Is_Graphic_Character (Value : Character_As_Int32)
                                     return Boolean is
      begin
         if
           Value >= To_Int32 (Latin_1.Space) and
           Value <= To_Int32 (Latin_1.Tilde)
         then
            return True;
         else
            return False;
         end if;
      end Is_Graphic_Character;

   end Latin_1;

   package body String_Split is

      procedure Put (Index : Interval_Array_Index;
                     Width : Ada.Text_IO.Field := 0) is
      begin
         Nat32_IO.Put (Nat32 (Index), Width => Width);
      end Put;

      --  Returns the number of digits of the image of the positive number.
      function Number_Of_Image_Digits (Value : Pos32) return Pos32 is
      begin
         case Value is
         when          1 ..          9 => return 1;
         when         10 ..         99 => return 2;
         when        100 ..        999 => return 3;
         when       1000 ..       9999 => return 4;
         when      10000 ..      99999 => return 5;
         when     100000 ..     999999 => return 6;
         when    1000000 ..    9999999 => return 7;
         when   10000000 ..   99999999 => return 8;
         when  100000000 ..  999999999 => return 8;
         when 1000000000 .. 2147483647 => return 9;
         end case;
      end Number_Of_Image_Digits;

      --  Example of output:
      --  (Last => 2,
      --   1 => (First => 1,
      --     => (Last  => 1,
      --   2 => (First => 3,
      --     => (Last  => 5)
      procedure Put (Interval : Variable_Length_Interval_Array) is
         Largest_Index : constant Pos32 := Pos32 (Interval.Interval'Last);
         Width : constant Ada.Text_IO.Field :=
           Ada.Text_IO.Field (Number_Of_Image_Digits (Largest_Index));
      begin
         Put ("(Last => ");
         Put (Interval.Last);
         Put (",");
         New_Line;
         for I in Interval.Interval'Range loop
            Put (" ");
            Put (I, Width);
            Put (" => (First => ");
            Put (Interval.Interval (I).First);
            Put (",");
            New_Line;
            Put_Repeated (" ", Repeat => Pos32 (Width + 1));
            Put (" => (Last  => ");
            Put (Interval.Interval (I).Last);
            if I = Interval.Interval'Last then
               Put (")");
            else
               Put (",");
               New_Line;
            end if;
         end loop;
      end Put;

      type Split_String_State is (Ignore_Separator,
                                  Non_Separator);

      function Get_Slices
        (Line   : String;
         Sep    : Character) return Variable_Length_Interval_Array
      is
         State : Split_String_State;
         Count : Nat32 := 0;
         Current    : Interval_Array_Index := 1;
         Prev_Index : Positive := Line'First;
      begin
         if Line (Line'First) = Sep then
            State := Ignore_Separator;
         else
            State := Non_Separator;
            Count := 1;
         end if;

         for I in Line'Range loop
            case State is
               when Ignore_Separator =>
                  if Line (I) /= Sep then
                     Count := Count + 1;
                     State := Non_Separator;
                  end if;
               when Non_Separator =>
                  if Line (I) = Sep then
                     State := Ignore_Separator;
                  end if;
            end case;
         end loop;

         declare
            Slices : Variable_Length_Interval_Array :=
              (Last     => Interval_Array_Index (Count),
               Interval => (others => (First => 0, Last => 0)));
         begin
            if Line (Line'First) = Sep then
               State := Ignore_Separator;
            else
               State := Non_Separator;
            end if;

            for I in Line'Range loop
               case State is
               when Ignore_Separator =>
                  if Line (I) /= Sep then
                     State := Non_Separator;
                     Prev_Index := I;
                     if I = Line'Last then
                        Slices.Interval (Current).First := I;
                        Slices.Interval (Current).Last  := I;
                     end if;
                  end if;
               when Non_Separator =>
                  if Line (I) = Sep then
                     State := Ignore_Separator;
                     Slices.Interval (Current).First := Prev_Index;
                     Slices.Interval (Current).Last  := I - 1;
                     Current := Current + 1;
                  elsif I = Line'Last then
                     Slices.Interval (Current).First := Prev_Index;
                     Slices.Interval (Current).Last  := I;
                  end if;
               end case;
            end loop;
            return Slices;
         end;
      end Get_Slices;

   end String_Split;

   package body UTF8 is

      subtype Categorization       is Stda.A.Categorization;
      subtype Categorization_Index is Stda.A.Categorization_Index;
      subtype Categorization_Array is Stda.A.Categorization_Array;

      use type Code_Point_Base;
      use type Categorization_Index;
      use type Stda.D.Range_Index;

      type UTF8_Tables is record
         Categorization_Table : Categorization_Array;
         Points_Table         : Stda.D.Range_Array;
      end record;

      Tables : UTF8_Tables;

      --  Contains two tables of data that is of private use
      --  of the UTF8 functionality.
      --  There should only be one instance of this type
      --  in the whole application that needs UTF8 functionality.

      use type Types.Pos32;
      use type Byte_Type;

      procedure Initialize (This : out UTF8_Tables) is
      begin
         Stda.A.Initialize (This.Categorization_Table);
         Stda.B.Initialize (This.Categorization_Table);
         Stda.C.Initialize (This.Categorization_Table);
         Stda.D.Initialize (This.Points_Table);
         Stda.E.Initialize (This.Points_Table);
      end Initialize;

      function Mapping (This  : UTF8_Tables;
                        Index : Categorization_Index)
                        return Categorization;

      function Mapping (This  : UTF8_Tables;
                        Index : Categorization_Index)
                        return Categorization is
      begin
         return This.Categorization_Table (Index);
      end Mapping;

      function Mapping (This  : UTF8_Tables;
                        Index : Stda.D.Range_Index)
                        return Stda.D.Points_Range;

      function Mapping (This  : UTF8_Tables;
                        Index : Stda.D.Range_Index)
                        return Stda.D.Points_Range is
      begin
         return This.Points_Table (Index);
      end Mapping;

      procedure Put (Destination : in out Types.Byte_Array;
                     Pointer     : in out Types.Pos32;
                     Value       : Code_Point) is
      begin
         if Value <= 16#7F# then
            Destination (Pointer) := Byte_Type (Value);
            Pointer := Pointer + 1;
         elsif Value <= 16#7FF# then
            Destination (Pointer) := Byte_Type (16#C0# or Value / 2**6);
            Destination (Pointer + 1)
              := Byte_Type (16#80# or (Value and 16#3F#));
            Pointer := Pointer + 2;
         elsif Value <= 16#FFFF# then
            Destination (Pointer) := Byte_Type (16#E0# or Value / 2**12);
            Destination (Pointer + 1)
              := Byte_Type (16#80# or (Value / 2**6 and 16#3F#));
            Destination (Pointer + 2)
              := Byte_Type (16#80# or (Value and 16#3F#));
            Pointer := Pointer + 3;
         else
            Destination (Pointer + 0)
              := Byte_Type (16#F0# or Value / 2**18);

            Destination (Pointer + 1)
              := Byte_Type (16#80# or (Value / 2**12 and 16#3F#));

            Destination (Pointer + 2)
              := Byte_Type (16#80# or (Value / 2**6 and 16#3F#));

            Destination (Pointer + 3)
              := Byte_Type (16#80# or (Value and 16#3F#));
            Pointer := Pointer + 4;
         end if;
      end Put;

      function Image (Value : Code_Point) return Types.Byte_Array is
         Result  : Types.Byte_Array (1 .. 4);
         Pointer : Types.Pos32 := 1;
      begin
         UTF8.Put (Result, Pointer, Value);
         return Result (1 .. Pointer - 1);
      end Image;

      function Image (Value : Code_Point) return String is
         Temp   : constant Types.Byte_Array := Image (Value);
         Result : String (Natural (Temp'First) .. Natural (Temp'Last));
      begin
         for I in Temp'Range loop
            Result (Natural (I)) := Character'Val (Temp (I));
         end loop;
         return Result;
      end Image;

--        procedure Image (Value : Types.Byte_Array;
--                         Text  : out Types.Variable_String)
--        is
--           Result  : Full_String;
--           Current : Positive := 1;
--           P : Types.Pos32 := Value'First;
--
--           CP : Code_Point;
--        begin
--           while P <= Value'Last loop
--              if
--                (not UTF8.Is_Valid_UTF8_Code_Point
--                   (Source => Value,
--                    Pointer => P))
--              then
--                 raise Constraint_Error;
--              end if;
--
--              Get (Source  => Value,
--                   Pointer => P,
--                   Value   => CP);
--              BStrings.Append (Result, Image (CP));
--           end loop;
--           return +Result;
--        end Image;

      procedure Find (Code  : Code_Point;
                      Found : out Boolean;
                      Index : in out Categorization_Index);

      procedure Find (Code  : Code_Point;
                      Found : out Boolean;
                      Index : in out Categorization_Index)
      is
         From    : Categorization_Index'Base := Categorization_Index'First;
         To      : Categorization_Index'Base := Categorization_Index'Last;
         This    : Categorization_Index;
         Current : Code_Point;
      begin
         while From <= To loop
            This    := From + (To - From) / 2;
            Current := Mapping (Tables, This).Code;

            if Current < Code then
               From := This + 1;
            elsif Current > Code then
               To := This - 1;
            elsif Current = Code then
               Found := True;
               Index := This;
               return;
            else
               Found := False;
               return;
            end if;
         end loop;
         Found := False;
      end Find;

      function Is_Uppercase
        (Value  : Code_Point) return Boolean
      is
         Index : Categorization_Index := Categorization_Index'First;
         Found : Boolean;
      begin
         Find (Value, Found, Index);
         if Found then
            return Mapping (Tables, Index).Upper = Value;
         else
            return False;
         end if;
      end Is_Uppercase;

      function Has_Case (Value : Code_Point) return Boolean
      is
         Index : Categorization_Index := Categorization_Index'First;
         Found : Boolean;
      begin
         Find (Value, Found, Index);
         if Found then
            return
              (Mapping (Tables, Index).Lower = Value
               or else Mapping (Tables, Index).Upper = Value);
         else
            return False;
         end if;
      end Has_Case;

      function Is_Lowercase (Value  : Code_Point) return Boolean is
         Index : Categorization_Index := Categorization_Index'First;
         Found : Boolean;
      begin
         Find (Value, Found, Index);
         if Found then
            return Mapping (Tables, Index).Lower = Value;
         else
            return False;
         end if;
      end Is_Lowercase;

      function To_Lowercase (Value  : Code_Point) return Code_Point is
         Index : Categorization_Index := Categorization_Index'First;
         Found : Boolean;
      begin
         Find (Value, Found, Index);
         if Found then
            return Mapping (Tables, Index).Lower;
         else
            return Value;
         end if;
      end To_Lowercase;

      function To_Uppercase (Value : Code_Point) return Code_Point is
         Index : Categorization_Index := Categorization_Index'First;
         Found : Boolean;
      begin
         Find (Value, Found, Index);
         if Found then
            return Mapping (Tables, Index).Upper;
         else
            return Value;
         end if;
      end To_Uppercase;

      function Category (Value  : Code_Point) return General_Category is
         From    : Stda.D.Range_Index := Stda.D.Range_Index'First;
         To      : Stda.D.Range_Index := Stda.D.Range_Index'Last;
         This    : Stda.D.Range_Index;
         Current : Stda.D.Points_Range;
      begin
         loop
            This    := (From + To) / 2;
            Current := Mapping (Tables, This);
            if Current.From > Value then
               exit when This = From;
               To := This - 1;
            elsif Current.To < Value then
               exit when This = To;
               From := This + 1;
            else
               return Current.Category;
            end if;
         end loop;
         return Co;
      end Category;

      function Is_Alphanumeric (V : Code_Point) return Boolean is
      begin
         case Category (V) is
         when Letter | Nd => return True;
         when others      => return False;
         end case;
      end Is_Alphanumeric;

      function Is_Control (Value : Code_Point) return Boolean is
      begin
         return Category (Value) = Cc;
      end Is_Control;

      function Is_Identifier_Extend (Value  : Code_Point) return Boolean is
      begin
         case Category (Value) is
         when Mn | Mc | Nd | Pc | Cf =>
            return True;
         when others =>
            return False;
         end case;
      end Is_Identifier_Extend;

      function Is_Identifier_Start (Value  : Code_Point) return Boolean is
      begin
         case Category (Value) is
         when Letter | Nl =>
            return True;
         when others =>
            return False;
         end case;
      end Is_Identifier_Start;

      function Is_ISO_646 (Value  : Code_Point) return Boolean is
      begin
         return Value <= 16#7F#;
      end Is_ISO_646;

      function Is_Letter (Value : Code_Point) return Boolean is
      begin
         return Category (Value) in Letter;
      end Is_Letter;

      function Is_Lower (Value : Code_Point) return Boolean is
      begin
         return Category (Value) = Ll;
      end Is_Lower;

      function Is_Digit (Value : Code_Point) return Boolean is
      begin
         return Category (Value) = Nd;
      end Is_Digit;

      function Is_Other_Format (V  : Code_Point) return Boolean is
      begin
         return Category (V) = Cf;
      end Is_Other_Format;

      function Is_Space (Value : Code_Point) return Boolean is
      begin
         return Category (Value) = Zs;
      end Is_Space;

      function Is_Subscript_Digit (Value : Code_Point) return Boolean is
      begin
         return Value in 16#2080# .. 16#208A#;
      end Is_Subscript_Digit;

      function Is_Superscript_Digit (Value : Code_Point) return Boolean is
      begin
         case Value is
         when 16#B2# .. 16#B3# | 16#B9# | 16#2070# .. 16#2079# =>
            return True;
         when others =>
            return False;
         end case;
      end Is_Superscript_Digit;

      function Is_Title (Value : Code_Point) return Boolean is
      begin
         return Category (Value) = Lt;
      end Is_Title;

      function Is_Upper (Value : Code_Point) return Boolean is
      begin
         return Category (Value) = Lu;
      end Is_Upper;

      function Is_Valid_UTF8_Code_Point
        (Source  : Types.Byte_Array;
         Pointer : Types.Pos32) return Boolean is
      begin
         if (Source'First <= Pointer and Pointer <= Source'Last) then
            if (Source (Pointer) in 0 .. 16#7F#) then
               return Pointer < Types.Pos32'Last;
            elsif Pointer < Source'Last then
               if
                 Source (Pointer + 0) in 16#C2# .. 16#DF# and
                 Source (Pointer + 1) in 16#80# .. 16#BF#
               then
                  return Pointer < Types.Pos32'Last - 1;
               elsif Pointer < Source'Last - 1 then
                  if
                    Source (Pointer + 0) = 16#E0# and
                    Source (Pointer + 1) in 16#A0# .. 16#BF# and
                    Source (Pointer + 2) in 16#80# .. 16#BF#
                  then
                     return Pointer < Types.Pos32'Last - 2;
                  elsif
                    Source (Pointer + 0) in 16#E1# .. 16#EF# and
                    Source (Pointer + 1) in 16#80# .. 16#BF# and
                    Source (Pointer + 2) in 16#80# .. 16#BF#
                  then
                     return Pointer < Types.Pos32'Last - 2;
                  elsif Pointer < Source'Last - 2 then
                     if
                       Source (Pointer + 0) = 16#F0# and
                       Source (Pointer + 1) in 16#90# .. 16#BF# and
                       Source (Pointer + 2) in 16#80# .. 16#BF# and
                       Source (Pointer + 3) in 16#80# .. 16#BF#
                     then
                        return Pointer < Types.Pos32'Last - 3;
                     elsif
                       Source (Pointer + 0) in 16#F1# .. 16#F3# and
                       Source (Pointer + 1) in 16#80# .. 16#BF# and
                       Source (Pointer + 2) in 16#80# .. 16#BF# and
                       Source (Pointer + 3) in 16#80# .. 16#BF#
                     then
                        return Pointer < Types.Pos32'Last - 3;
                     elsif
                       Source (Pointer + 0) = 16#F4# and
                       Source (Pointer + 1) in 16#80# .. 16#8F# and
                       Source (Pointer + 2) in 16#80# .. 16#BF# and
                       Source (Pointer + 3) in 16#80# .. 16#BF#
                     then
                        return Pointer < Types.Pos32'Last - 3;
                     else
                        return False;
                     end if;
                  else
                     return False;
                  end if;
               else
                  return False;
               end if;
            else
               return False;
            end if;
         else
            return False;
         end if;
      end Is_Valid_UTF8_Code_Point;

      procedure Get
        (Source  : Types.Byte_Array;
         Pointer : in out Types.Pos32;
         Value   : out Code_Point)
      is
         Accum   : Code_Point'Base;
         Accum_1 : Code_Point'Base;
         Accum_2 : Code_Point'Base;
         Code    : Code_Point'Base;
         Temp_1  : Code_Point'Base;
         Temp_2  : Code_Point'Base;
      begin
         Code := Code_Point (Source (Pointer));

         case Code is
         when 0 .. 16#7F# => -- 1 byte (ASCII)
            Value   := Code;
            Pointer := Pointer + 1;
         when 16#C2# .. 16#DF# => -- 2 bytes
            Accum   := (Code and 16#1F#) * 2**6;
            Code    := Code_Point (Source (Pointer + 1));
            Value   := Accum or (Code and 16#3F#);
            Pointer := Pointer + 2;
         when 16#E0# => -- 3 bytes
            Temp_1  := Code_Point (Source (Pointer + 1));
            Accum   := (Temp_1 and 16#3F#) * 2**6;
            Code    := Code_Point (Source (Pointer + 2));
            Value   := Accum or (Code and 16#3F#);
            Pointer := Pointer + 3;
         when 16#E1# .. 16#EF# => -- 3 bytes
            Accum_1 := (Code and 16#0F#) * 2**12;
            Temp_1  := Code_Point (Source (Pointer + 1));
            Accum   := Accum_1 or (Temp_1 and 16#3F#) * 2**6;
            Code    := Code_Point (Source (Pointer + 2));
            Value   := Accum or (Code and 16#3F#);
            Pointer := Pointer + 3;
         when 16#F0# => -- 4 bytes
            Temp_1  := Code_Point (Source (Pointer + 1));
            Accum_1 := (Temp_1 and 16#3F#) * 2**12;
            Temp_2  := Code_Point (Source (Pointer + 2));
            Accum   := Accum_1 or (Temp_2 and 16#3F#) * 2**6;
            Code    := Code_Point (Source (Pointer + 3));
            Value   := Accum or (Code and 16#3F#);
            Pointer := Pointer + 4;
         when 16#F1# .. 16#F3# => -- 4 bytes
            Accum_1 := (Code and 16#07#) * 2**18;
            Temp_1  := Code_Point (Source (Pointer + 1));
            Accum_2 := Accum_1 or (Temp_1 and 16#3F#) * 2**12;
            Temp_2  := Code_Point (Source (Pointer + 2));
            Accum   := Accum_2 or (Temp_2 and 16#3F#) * 2**6;
            Code    := Code_Point (Source (Pointer + 3));
            Value   := Accum or (Code and 16#3F#);
            Pointer := Pointer + 4;
         when 16#F4# => -- 4 bytes
            Accum_1 := (Code and 16#07#) * 2**18;
            Temp_1  := Code_Point (Source (Pointer + 1));
            Accum_2 := Accum_1 or (Temp_1 and 16#3F#) * 2**12;
            Temp_2  := Code_Point (Source (Pointer + 2));
            Accum   := Accum_2 or (Temp_2 and 16#3F#) * 2**6;
            Code    := Code_Point (Source (Pointer + 3));
            Value   := Accum or (Code and 16#3F#);
            Pointer := Pointer + 4;
         when others =>
            raise Constraint_Error;
            --  This exception will never be raised if pre-conditions are met.
         end case;
      end Get;

      function Is_Valid_UTF8 (Source : Types.Byte_Array) return Boolean is
         Accum : Code_Point;

         Index : Types.Pos32 := Source'First;
      begin
         while Index <= Source'Last loop
            if Is_Valid_UTF8_Code_Point (Source, Index) then
               Get (Source, Index, Accum);
            else
               exit;
            end if;
         end loop;
         return Index = Source'Last + 1;
      end Is_Valid_UTF8;

      function Length (Source : Types.Byte_Array) return Nat32 is
         Count : Nat32 := 0;
         Accum : Code_Point;

         Index : Types.Pos32 := Source'First;
      begin
         while Index <= Source'Last loop
            if Is_Valid_UTF8_Code_Point (Source, Index) then
               Get (Source, Index, Accum);
               Count := Count + 1;
            else
               exit;
            end if;
         end loop;
         return Count;
      end Length;

      function To_Lowercase (V : Types.Byte_Array) return Types.Byte_Array is
         Result : Types.Byte_Array (1 .. V'Length);
         From   : Types.Pos32 := V'First;
         To     : Types.Pos32 := 1;
         Code   : Code_Point;
      begin
         while From <= V'Last loop
            UTF8.Get (V, From, Code);
            Code := To_Lowercase (Code);
            UTF8.Put (Result, To, Code);
         end loop;
         return Result (1 .. To - 1);
      end To_Lowercase;

      function To_Uppercase (V : Types.Byte_Array) return Types.Byte_Array is
         Result : Types.Byte_Array (1 .. V'Length);
         From   : Types.Pos32 := V'First;
         To     : Types.Pos32 := 1;
         Code   : Code_Point;
      begin
         while From <= V'Last loop
            UTF8.Get (V, From, Code);
            Code := To_Uppercase (Code);
            UTF8.Put (Result, To, Code);
         end loop;
         return Result (1 .. To - 1);
      end To_Uppercase;

      procedure Initialize is
      begin
         Initialize (Tables);
      end Initialize;

   end UTF8;

end Stda;
