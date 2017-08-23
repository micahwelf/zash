with Ada.Characters.Handling;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Wide_Wide_Latin_1; use Ada.Characters.Wide_Wide_Latin_1;
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
package body JSON is

   -----------
   -- Types --
   -----------

   package body Types is


      ------------------
      -- Is_Character --
      ------------------

      function Is_Character
        (Item : Standard.Wide_Wide_Character) return Standard.Boolean is
      begin
         return Wide_Wide_Character'Pos (Item) < 256;
      end;


      ---------------
      -- Is_String --
      ---------------

      function Is_String
        (Item : Standard.Wide_Wide_String) return Standard.Boolean is
         Result : Standard.Boolean := True;
      begin
         for C of Item loop
            if not
              (Wide_Wide_Character'Pos (C) < 256)
            then
               Result := False;
            end if;
         end loop;
         return Result;
      end;


      ---------------
      -- To_String --
      ---------------

      function To_String
        (Item       : Standard.Wide_Wide_String;
         Substitute : Character := ' ') return Standard.String is
         Result : Standard.String (Item'Range);
      begin
         for Index in Item'Range loop
            if
              Is_Character (Item (Index))
            then
               Result (Index) := Character'Val (Wide_Wide_Character'Pos (Item (Index)));
            else
               Result (Index) := Substitute;
            end if;
         end loop;
         return Result;
      end;


      --------------------
      -- To_Wide_String --
      --------------------

      function To_Wide_String
        (Item       : Standard.String) return Standard.Wide_Wide_String is
         Result : Standard.Wide_Wide_String (Item'Range);
      begin
         for Index in Item'Range loop
            Result (Index) := Wide_Wide_Character'Val (Character'Pos (Item (Index)));
         end loop;
         return Result;
      end;


      --------------
      -- Get_Type -- JSON_Type
      --------------

      function Get_Type (Str : Json_String) return Json_Type is
         use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
         Temp   : Wide_Wide_String := Decode (Str);
         Front  : Types.Json_Type := Undefined;
         Back   : Types.Json_Type := Undefined;
         Index  : Natural := 0;
         RIndex : Integer := Temp'Last + 1;
      begin
         for C of Temp loop
            Index := Index + 1;
            if Is_Space (C)
              or Is_Line_Terminator (C)
              or HT = C
            then null;
            elsif '-' = C or Is_Digit (C)
            then Front := Number; exit;

            elsif '{' = C
            then Front := Object; exit;

            elsif '[' = C
            then Front := Vector; exit;

            elsif 'n' = C
            then Front := NUL; exit;

            elsif 't' = C or 'f' = C
            then Front := Boolean; exit;

            elsif '"' = C
            then Front := String; exit;

            else exit;

            end if;

         end loop;

         for C of reverse Temp loop
            RIndex := RIndex - 1;
            if Is_Space (C)
              or Is_Line_Terminator (C)
              or HT = C
            then null;

            elsif  Is_Digit (C)
            then Back := Number; exit;

            elsif '}' = C
            then Back := Object; exit;

            elsif ']' = C
            then Back := Vector; exit;

            elsif 'l' = C or 'e' = C
            then
               if Temp (Index .. RIndex) = "null" then Back := NUL; exit;
               elsif Temp (Index .. RIndex) = "true" then Back := Boolean; exit;
               elsif Temp (Index .. RIndex) = "false" then Back := Boolean; exit;
               end if;
            elsif '"' = C
            then Back := String; exit;

            else exit;

            end if;

         end loop;

         return (if Front = Back then Front else Undefined);

      end Get_Type;

      --------------
      -- Get_Type -- HTML_Type
      --------------

      function Get_Type (Str : HTML_String) return HTML_Type is
      begin
         --  Generated stub: replace with real body!
         --pragma Compile_Time_Warning (Standard.True, "Get_Type unimplemented");
         raise Program_Error with "Unimplemented function Get_Type (HTML_Type)";
         return Get_Type (Str => Str);
      end Get_Type;

      --------------
      -- Get_Type -- CSS_Type
      --------------

      function Get_Type (Str : CSS_String) return CSS_Type is
      begin
         --  Generated stub: replace with real body!
         --pragma Compile_Time_Warning (Standard.True, "Get_Type unimplemented");
         raise Program_Error with "Unimplemented function Get_Type (CSS_Type)";
         return Get_Type (Str => Str);
      end Get_Type;

      -----------
      -- Image -- Integer --> String
      -----------

      function Image (Number : Integer) return Standard.String is

         N : Integer          := (if Number < 0 then -Number else Number);
         S : Standard.String (1 .. 48) := (others => ' ');
         L : Natural          := 0;
         procedure Image_Integer
           (N : in     Natural;
            S : in out Standard.String;
            L : in out Natural)
         is
         begin
            null;
            if N >= 10
            then
               Image_Integer (N / 10, S, L);
               L     := L + 1;
               S (L) := Character'Val (48 + (N rem 10));
            else
               L     := L + 1;
               S (L) := Character'Val (48 + (N rem 10));
            end if;
         end Image_Integer;

      begin

         if Number < 0
         then
            L := L + 1;
            S (L) := '-';
         end if;
         Image_Integer (N, S, L);
         return S (1 .. L);
      end Image;

      -----------
      -- Image -- Integer --> Wide_Wide_String
      -----------

      function Image (Number : Integer) return Standard.Wide_Wide_String is

         N : Integer          := (if Number < 0 then -Number else Number);
         S : Standard.Wide_Wide_String (1 .. 48) := (others => ' ');
         L : Natural          := 0;
         procedure Image_Integer
           (N : in     Natural;
            S : in out Standard.Wide_Wide_String;
            L : in out Natural)
         is
         begin
            null;
            if N >= 10
            then
               Image_Integer (N / 10, S, L);
               L     := L + 1;
               S (L) := Wide_Wide_Character'Val (48 + (N rem 10));
            else
               L     := L + 1;
               S (L) := Wide_Wide_Character'Val (48 + (N rem 10));
            end if;
         end Image_Integer;

      begin

         if Number < 0
         then
            L := L + 1;
            S (L) := '-';
         end if;
         Image_Integer (N, S, L);
         return S (1 .. L);
      end Image;

      -----------
      -- Value -- String --> Integer
      -----------

      function Value (Image : Standard.String) return Integer is
      begin
         return Integer (Long_Long_Float'Value (Image));
      end Value;

      -----------
      -- Value -- Wide_Wide_String --> Integer
      -----------

      function Value (Image : Standard.Wide_Wide_String) return Integer is
      begin

         return Integer (Long_Long_Float'Value (To_String (Image)));
      end Value;


      --
      -- Floats used with JSON Import/Export
      --

      -----------
      -- Image -- Long_Long_Float --> String
      -----------

      function Image (Number : Long_Long_Float) return Standard.String is
         N            : Long_Long_Float := (if Number < 0.0 then -Number else Number);
         Sign         : Standard.String := (if Number < 0.0 then "-" else "");
         Number_Left  : Long_Long_Integer := Long_Long_Integer (Long_Long_Float'Floor (N));
         Number_Right : Long_Long_Float := N - Long_Long_Float'Floor (N) + 1.0;
         Left         : Standard.String := Number_Left'Image;
         Right        : Standard.String := Number_Right'Image;
         Left_Start   : Natural := Left'First;
         Left_Finish  : Natural := Left'Last;
         Right_Start  : Natural := Right'First;
         Right_Finish : Natural := Right'First - 1;
      begin
         for X in Left'Range
         loop
            if Left (X) in '1' .. '9'
            then Left_Start := X; exit;
            end if;
         end loop;

         for X in Right'Range
         loop
            if Right (X) = '.'
            then Right_Start := X; exit;
            end if;
         end loop;

         for X in Right'First .. Right'Last - 2
         loop
            if Right (X) in '1' .. '9'
            then Right_Finish := X;
            elsif Right (X) = 'E'
            then exit;
            end if;
         end loop;

         return Sign & Left (Left_Start .. Left_Finish) & Right (Right_Start .. Right_Finish);

      end Image;


      -----------
      -- Image -- Long_Long_Float --> Wide_Wide_String
      -----------

      function Image (Number : Long_Long_Float) return Standard.Wide_Wide_String is
         N            : Long_Long_Float := (if Number < 0.0 then -Number else Number);
         Sign         : Standard.Wide_Wide_String := (if Number < 0.0 then "-" else "");
         Number_Left  : Long_Long_Integer := Long_Long_Integer (Long_Long_Float'Floor (N));
         Number_Right : Long_Long_Float := N - Long_Long_Float'Floor (N) + 1.0;
         Left         : Standard.Wide_Wide_String := To_Wide_String (Number_Left'Image);
         Right        : Standard.Wide_Wide_String := To_Wide_String (Number_Right'Image);
         Left_Start   : Natural := Left'First;
         Left_Finish  : Natural := Left'Last;
         Right_Start  : Natural := Right'First;
         Right_Finish : Natural := Right'First - 1;
      begin
         for X in Left'Range
         loop
            if Left (X) in '1' .. '9'
            then Left_Start := X; exit;
            end if;
         end loop;

         for X in Right'Range
         loop
            if Right (X) = '.'
            then Right_Start := X; exit;
            end if;
         end loop;

         for X in Right'First .. Right'Last - 2
         loop
            if Right (X) in '1' .. '9'
            then Right_Finish := X;
            elsif Right (X) = 'E'
            then exit;
            end if;
         end loop;

         return Sign & Left (Left_Start .. Left_Finish) & Right (Right_Start .. Right_Finish);

      end Image;


      -----------
      -- Value --
      -----------

      function Value (Image : Standard.String) return Long_Long_Float is
      begin
         return Long_Long_Float'Value (Image);
      end Value;


      -----------
      -- Value --
      -----------

      function Value (Image : Wide_Wide_String) return Long_Long_Float is
      begin
         return Long_Long_Float'Value (To_String (Image));
      end Value;



      --
      -- Strings used with JSON Import/Export
      --


      ------------------
      -- Escape_Image --
      ------------------

      function Escape_Image (Value : in Wide_Wide_Character) return Wide_Wide_String is
         use Ada;
         Hex : constant array (Natural range 0 .. 15) of Wide_Wide_Character :=
           (0      => '0', 1 => '1', 2 => '2', 3 => '3', 4 => '4',
            5      => '5', 6 => '6', 7 => '7', 8 => '8', 9 => '9',
            10     => 'a', 11 => 'b', 12 => 'c', 13 => 'd', 14 => 'e', 15 => 'f',
            others => Latin.NUL);
         C   : constant Wide_Wide_Character := Value;
         O   : Wide_Wide_String (1 .. 4) := "0000";
         N   : Natural := Wide_Wide_Character'Pos (C);
      begin
         for X in reverse O'Range
         loop
            O (X) := Hex (N mod 16);
            N := (N - (N mod 16)) / 16;
         end loop;
         return Wide_Wide_String'("\u") & O;
      end Escape_Image;


      ------------------
      -- Escape_Value --
      ------------------

      function Escape_Value (Image : Wide_Wide_String) return Wide_Wide_Character is
         use Ada;
         subtype Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
         UnHex : constant array (Wide_Wide_Character) of Natural :=
           ('0'    => 0,  '1' => 1,  '2' => 2,  '3' => 3,  '4' => 4,
            '5'    => 5,  '6' => 6,  '7' => 7,  '8' => 8,  '9' => 9,
            'A'    => 10, 'B' => 11, 'C' => 12, 'D' => 13, 'E' => 14, 'F' => 15,
            'a'    => 10, 'b' => 11, 'c' => 12, 'd' => 13, 'e' => 14, 'f' => 15,
            others => 0);
         S     : constant Wide_Wide_String := Image;
         V     : Natural := 0;
      begin

         -- Check for valid 4-digit Hex string.
         for X of S (S'Last - 3 .. S'Last)
         loop
            if not Ada.Wide_Wide_Characters.Handling.Is_Hexadecimal_Digit (X)
            then
               raise Error.Syntax_Invalid_Escape with "converting Hexadecimal quartet failed.";
            end if;
         end loop;

         -- Generate the value from base sixteen numerical characters.
         for C of S
         loop
            V := (V * 16) + (UnHex (C));
         end loop;

         return Wide_Wide_Character'Val (V);

      end Escape_Value;


      -----------
      -- Value -- Ada Value --> JSON String     (not Json_String)
      -----------

      function Image (Value : Standard.Wide_Wide_String) return Standard.String is
      begin
         return To_String (Image (Value));
      end Image;


      -----------
      -- Value -- Ada Value --> JSON Wide_Wide_String
      -----------

      function Image (Value : in Standard.Wide_Wide_String) return Standard.Wide_Wide_String is

         Index : Integer := Value'First;

         function Required_Length return Integer is
            Result     : Integer := Value'Last;
            Extra      : constant array (Standard.Wide_Wide_Character) of Standard.Integer
              := (Latin.BS .. Latin.LF                  => 1,
                  Latin.FF .. Latin.CR                  => 1,
                  '"'                                   => 1,
                  '\'                                   => 1,
                  '/'                                   => 1,
                  Latin.NUL .. Latin.BEL                => 5,
                  Latin.VT                              => 5,
                  Latin.SO .. Latin.US                  => 5,
                  Latin.DEL .. Wide_Wide_Character'Last => 5,
                  others                                => 0);
         begin
            for C of Value loop
               Result := Result + Extra (C);
            end loop;
            return Result;
         end Required_Length;

         Result : Standard.Wide_Wide_String (1 .. (Required_Length));

         procedure Append (C : Wide_Wide_Character) is
         begin
            if not (Index > Result'Last) then
               Result (Index) := C;
               Index := Index + 1;
            else
               raise Constraint_Error with "Error in Image, converting:" & Encode (Value);
            end if;
         end Append;

         procedure Append (S : Wide_Wide_String) is
         begin
            for C of S loop
               Append (C => C);
            end loop;
         end Append;
      begin
         for C of Value loop
            case C is
               when Latin.BS => Append (S => "\b");
               when Latin.HT => Append (S => "\t");
               when Latin.LF => Append (S => "\n");
               when Latin.FF => Append (S => "\f");
               when Latin.CR => Append (S => "\r");
               when '"' => Append (S => "\""");
               when '\' => Append (S => "\\");
               when '/' => Append (S => "\/");
               when Latin.NUL .. Latin.BEL => Append (S => Escape_Image (C));
               when Latin.VT => Append (S => Escape_Image (C));
               when Latin.SO .. Latin.US => Append (S => Escape_Image (C));
               when Latin.DEL .. Wide_Wide_Character'Last => Append (S => Escape_Image (C));
               when others => Append (C => C);
            end case;
         end loop;
         return Result;
      end Image;


      -----------
      -- Value -- JSON String --> Ada Value       (Json_Type=>String, not Json_String)
      -----------

      function Value (Image : Standard.String) return Standard.Wide_Wide_String is
      begin
         return Value (Decode (Image));
      end Value;

      -----------
      -- Value -- JSON Wide_Wide_String --> Ada Value
      -----------

      function Value (Image : Standard.Wide_Wide_String) return Standard.Wide_Wide_String is

         Index       : Integer := Image'First;
         Escape_Flag : Standard.Boolean := False;

         function Required_Length return Integer is
            Result : Integer := Image'Last;
            Extra  : constant array (Standard.Wide_Wide_Character) of Standard.Integer
              := ('b'    => 1,
                  't'    => 1,
                  'n'    => 1,
                  'f'    => 1,
                  'r'    => 1,
                  '"'    => 1,
                  '\'    => 1,
                  '/'    => 1,
                  'u'    => 5,
                  others => 0);
         begin
            for C of Image loop
               if Escape_Flag then
                  Result := Result - Extra (C);
                  Escape_Flag := False;
               elsif C = '\' then
                  Escape_Flag := True;
               end if;
            end loop;
            return Result;
         end Required_Length;

         Result      : Standard.Wide_Wide_String (1 .. (Required_Length));

         procedure Append (C : Wide_Wide_Character) is
         begin
            if not (Index > Result'Last) then
               Result (Index) := C;
               Index := Index + 1;
            else
               raise Constraint_Error with "Error in Value, converting:" & Encode (Image);
            end if;
         end Append;

      begin

         for X in Image'Range loop
            if Escape_Flag then
               case Image (X) is
                  when 'b' => Append (C => Latin.BS);
                  when 't' => Append (C => Latin.HT);
                  when 'n' => Append (C => Latin.LF);
                  when 'f' => Append (C => Latin.FF);
                  when 'r' => Append (C => Latin.CR);
                  when '"' => Append (C        => '"');
                  when '\' => Append (C        => '\');
                  when '/' => Append (C           => '/');
                  when 'u' => Append (C => Escape_Value (Image (X + 1 .. X + 4)));
                  when others => Append (C => Image (X));
               end case;
               Escape_Flag := False;
            elsif Image (X) = '\' then
               Escape_Flag := True;
            end if;
         end loop;
         return Result;
      end Value;


   end Types;

   package body Scanner is

      --------------
      -- Get_Type --
      --------------

      function Get_Type (Subject : Of_Context.Vector) return Types.Json_Type is
      begin
         return Subject.Last_Element.Format;
      end Get_Type;

      ------------
      -- Pop_To --
      ------------

      function Pop (Subject : in out Of_Context.Vector) return Positive is
         Number : Positive := Subject.Last_Element.Index;
      begin
         Subject.Delete_Last;
         return Number;
      end Pop;

   end Scanner;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Json : Node) return Types.Json_Type is
   begin
      return Json.Node_Type;
   end Get_Type;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : in Node) return Boolean is
      use Types;
      use Of_Array;
      use Of_Object;
   begin
      if Left.Node_Type /= Right.Node_Type
      then return False;
      end if;

      case Left.Node_Type is
         when Types.NUL =>
            return True;
         when Types.Boolean =>
            return Left.Value.Boolean = Right.Value.Boolean;
         when Types.Number =>
            return Left.Value.Number = Right.Value.Number;
         when Types.String =>
            return Left.Value.String = Right.Value.String;
         when Types.Vector =>
            return Left.Value.Vector = Right.Value.Vector;
         when Types.Object =>
            return Left.Value.Object = Right.Value.Object;
         when others =>
            return False;
      end case;

   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left : in Types.Json_Type; Right : in Node) return Boolean is
      use Types;
   begin

      return Left = Right.Node_Type;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left : in Node; Right : in Types.Json_Type) return Boolean is
      use Types;
   begin
      return Left.Node_Type = Right;
   end "=";

   ---------
   -- "/" --
   ---------

   function "/"
     (Left  : in Wide_Wide_String;
      Right : in Wide_Wide_String)
      return Types.Json_Path
   is
      use Types;
      use Ada.Strings.Wide_Wide_Unbounded;
      Result : Types.Json_Path := ((Element_Type => Types.String, String => To_Unbounded_Wide_Wide_String (Left)),
                                   (Element_Type => Types.String, String => To_Unbounded_Wide_Wide_String (Right)));
   begin
      return Result;
   end "/";

   ---------
   -- "/" --
   ---------

   function "/"
     (Left  : in Wide_Wide_String;
      Right : in Integer)
      return Types.Json_Path
   is
      use Types;
      use Ada.Strings.Wide_Wide_Unbounded;
      Result : Types.Json_Path := ((Element_Type => Types.String, String => To_Unbounded_Wide_Wide_String (Left)),
                                   (Element_Type => Types.Number, Number => Right));
   begin
      return Result;
   end "/";

   ---------
   -- "/" --
   ---------

   function "/"
     (Left  : in Integer;
      Right : in Wide_Wide_String)
      return Types.Json_Path
   is
      use Types;
      use Ada.Strings.Wide_Wide_Unbounded;
      Result : Types.Json_Path := ((Element_Type => Types.Number, Number => Left),
                                   (Element_Type => Types.String, String => To_Unbounded_Wide_Wide_String (Right)));
   begin
      return Result;
   end "/";

   ---------
   -- "/" --
   ---------

   function "/"
     (Left  : in Integer;
      Right : in Integer)
      return Types.Json_Path
   is
      use Types;
      use Ada.Strings.Wide_Wide_Unbounded;
      Result : Types.Json_Path := ((Element_Type => Types.Number, Number => Left),
                                   (Element_Type => Types.Number, Number => Right));
   begin
      return Result;
   end "/";

   ---------
   -- "/" --
   ---------

   function "/"
     (Left  : in Types.Json_Path;
      Right : in Integer)
      return Types.Json_Path
   is
      use Types;
      use Ada.Strings.Wide_Wide_Unbounded;
      Result : Types.Json_Path := (1 => (Element_Type => Types.Number, Number => Right));
   begin
      return Left & Result;
   end "/";

   ---------
   -- "/" --
   ---------

   function "/"
     (Left  : in Types.Json_Path;
      Right : in Wide_Wide_String)
      return Types.Json_Path
   is
      use Types;
      use Ada.Strings.Wide_Wide_Unbounded;
      Result : Types.Json_Path := (1 => (Element_Type => Types.String, String => To_Unbounded_Wide_Wide_String (Right)));
   begin
      return Left & Result;
   end "/";

   ------------
   -- Import --
   ------------

   function Import (Source : in Json_String) return Node is
      use Types;
      use Scanner;
      Stack    : Scanner.Of_Context.Vector;
      Heap     : Scanner.Of_Node.Vector;
      State    : Scanner.State := Scanner.Ready;
      Src      : Wide_Wide_String := Decode (Source);
      Src_Type : Types.Json_Type := Types.Get_Type (Source);
      G_Cursor : Natural := 0;
      Length   : Natural := 0;
      Result   : Node;
      function Is_Hexadecimal (Str : in Wide_Wide_String) return Standard.Boolean is
         use Ada.Wide_Wide_Characters.Handling;
      begin
         for X of Str loop
            if not Is_Hexadecimal_Digit (X) then return False; end if;
         end loop;
         return True;
      end Is_Hexadecimal;
   begin

      for Cursor in Src'Range
      loop

         case State is
            when Scanner.Ready =>

               case Src (Cursor) is
                  when 'n' =>
                     State := Scanner.Literal;
                     if Src'Last - Cursor >= 3
                       and then
                         Src (Cursor .. Cursor + 3) = "null"
                     then Heap.Append ((Types.NUL,
                                       First => Natural (Cursor),
                                       Last  => Natural (Cursor + 3)));
                     else
                        G_Cursor := Cursor;
                        raise Error.Syntax_Invalid_Literal
                        with " ... perhaps ""null"" was meant ";
                     end if;

                  when 't' =>
                     if Src'Last - Cursor >= 3
                       and then
                         Src (Cursor .. Cursor + 3) = "true"
                     then Heap.Append ((Types.Boolean,
                                       First => Natural (Cursor),
                                       Last  => Natural (Cursor + 3)));
                     else
                        G_Cursor := Cursor;
                        raise Error.Syntax_Invalid_Literal
                        with " ... perhaps ""true"" was meant ";
                     end if;

                  when 'f' =>
                     if Src'Last - Cursor >= 4
                       and then
                         Src (Cursor .. Cursor + 4) = "false"
                     then Heap.Append ((Types.Boolean,
                                       First => Natural (Cursor),
                                       Last  => Natural (Cursor + 4)));
                     else
                        G_Cursor := Cursor;
                        raise Error.Syntax_Invalid_Literal
                        with " ... perhaps ""false"" was meant ";
                     end if;

                  when '-' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' =>
                     State := Scanner.Literal;
                     Heap.Append ((Types.Number,
                                  First => Natural (Cursor),
                                  Last  => Natural (Cursor)));
                     for Sub_Cursor in Cursor .. Src'Last loop

                        case Src (Sub_Cursor) is
                           when '-' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' | 'E' | 'e' | '.' | '+' =>
                              Heap (Heap.Last_Index).Last := Sub_Cursor;
                           when others => G_Cursor := Cursor; raise Program_Error with "Program examining in number mode, should have escaped already";
                        end case;

                        if Sub_Cursor < Src'Last then

                           case Src (Sub_Cursor + 1) is
                              when '-' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' | 'E' | 'e' | '.' | '+' =>
                                 null;
                              when others => exit;
                           end case;

                        end if;

                     end loop;

                  when '"' =>
                     State := Scanner.String;
                     if Cursor = Src'Last
                     then G_Cursor := Cursor; raise Error.Syntax_No_Close_String; end if;
                     Heap.Append ((Types.String,
                                  First => Natural (Cursor + 1),
                                  Last  => Natural (Cursor)));
                  when '{' =>
                     if Cursor = Src'Last
                     then G_Cursor := Cursor; raise Error.Syntax_No_Close_Object; end if;
                     Heap.Append ((Types.Object,
                                  First  => Natural (Cursor),
                                  others => <>));
                     Stack.Append ((Types.Object,
                                   Positive (Heap.Last_Index)));
                  when '[' =>
                     if Cursor = Src'Last
                     then G_Cursor := Cursor; raise Error.Syntax_No_Close_Array; end if;
                     Heap.Append ((Types.Vector,
                                  First  => Natural (Cursor),
                                  others => <>));
                     Stack.Append ((Types.Vector,
                                   Positive (Heap.Last_Index)));
                  when '}' =>
                     if Stack.Is_Empty or else Get_Type (Stack) /= Types.Object
                     then G_Cursor := Cursor; raise Error.Syntax_Extra_Close_Object;
                     else Heap (Pop (Stack)).Last := Cursor;
                     end if;

                  when ']' =>
                     if Stack.Is_Empty or else Get_Type (Stack) /= Types.Vector
                     then G_Cursor := Cursor; raise Error.Syntax_Extra_Close_Array;
                     else Heap (Pop (Stack)).Last := Cursor;
                     end if;

                  when others =>
                     G_Cursor := Cursor;
                     raise Error.Syntax_Invalid_Value;
               end case;

            when Scanner.Literal =>
               if Cursor = Heap.Last_Element.Last
               then State := Scanner.Ready; end if;

            when Scanner.String =>
               case Src (Cursor) is
                  when '\' =>
                     State := Scanner.Escape;
                     if not (Src'Last - Cursor >= 1)
                     then G_Cursor := Cursor; raise Error.Syntax_Invalid_Escape; end if;
                  when '"' =>
                     Heap (Heap.Last_Index).Last := Cursor - 1;
                     State := Scanner.Ready;
                  when others =>
                     if Cursor = Src'Last
                     then G_Cursor := Cursor; raise Error.Syntax_No_Close_String; end if;
               end case;

            when Scanner.Escape =>
               case Src (Cursor) is
                  when 'u' =>
                     if Src'Last - Cursor >= 4 and then Is_Hexadecimal (Src (Cursor + 1 .. Cursor + 4))
                     then null;
                     else G_Cursor := Cursor; raise Error.Syntax_Invalid_Escape;
                     end if;
                  when others =>
                     null;
               end case;

         end case;

      end loop;

      if not Stack.Is_Empty
      then
         raise Program_Error with " End of initial scan, Scanner Stack not empty! ";
      end if;

      declare
         Index          : Positive := Heap.First_Index;
         Parent         : Scanner.Node := Heap (Index);
         procedure Create (Parent_Result  : in out JSON.Node;
                           Heap           : in out Scanner.Of_Node.Vector;
                           Index          : in out Positive;
                           Default_Parent : in Scanner.Node) is
            Implement_Flag : Standard.Boolean := (if Parent_Result.Node_Type = Types.Object
                                                  and Heap (Index).Format = Types.String
                                                  then True
                                                  else False);
            function Implement_Key return Wide_Wide_String is
               Result
               : Wide_Wide_String
                 := Wide_Wide_String'(Types.Value
                                      (Src
                                         (Heap (Index).First .. Heap (Index).Last)));
            begin
               Index := Index + 1;
               return Result;
            end Implement_Key;
            Result         : aliased JSON.Node; --  <-- Child Node
            Parent         : Scanner.Node := Default_Parent;
            Child_Key      : Wide_Wide_String := (if Implement_Flag
                                                  then Implement_Key
                                                  else "");
         begin

            --------------------------------------------------
            --------------------------------------------------
            --  ####   ####   ####   ###   #####  ####
            -- #    #  #   #  #     #   #    #    #
            -- #       ####   ###   #####    #    ###
            -- #    #  #   #  #     #   #    #    #
            --  ####   #   #  ####  #   #    #    ####
            --------------------------------------------------
            --------------------------------------------------

            if Index <= Positive (Heap.Last_Index) then

               case
                    Heap (Index).Format
                  is

                  when Types.Object =>
                     Parent := Heap (Index);
                     Result.Node_Type := Types.Object;
                     Result.Value := (Of_Type =>
                                         Types.Object,
                                      others  =>
                                         <>);
                     Index := Index + 1;
                     while Heap (Index).Last < Parent.Last loop
                        Create (Result, Heap, Index, Parent);
                        Index := Index + 1;
                        exit when Index > Heap.Last_Index ;
                     end loop;

                  when Types.Vector =>
                     Parent := Heap (Index);
                     Result.Node_Type := Types.Vector;
                     Result.Value := (Of_Type =>
                                         Types.Vector,
                                      others  =>
                                         <>);
                     Index := Index + 1;
                     while Heap (Index).Last < Parent.Last loop
                        Create (Result, Heap, Index, Parent);
                        Index := Index + 1;
                        exit when Index > Heap.Last_Index ;
                     end loop;

                  when Types.String =>
                     Result.Node_Type := Types.String;
                     Result.Value := (Of_Type =>
                                         Types.String,
                                      String  =>
                                         To_Unbounded_Wide_Wide_String
                                        (Wide_Wide_String'(Types.Value
                                         (Src
                                              (Heap (Index).First .. Heap (Index).Last)))),
                                      others  => <>);
                     Index := Index + 1;
                  when Types.Number    =>
                     Result.Node_Type := Types.Number;
                     Result.Value := (Of_Type =>
                                         Types.Number,
                                      Number  =>
                                         Long_Long_Float'(Types.Value
                                        (Src
                                           (Heap (Index).First .. Heap (Index).Last))),
                                      others  => <>);
                     Index := Index + 1;
                  when Types.Boolean   =>
                     Result.Node_Type := Types.Boolean;
                     Result.Value := (Of_Type =>
                                         Types.Boolean,
                                      Boolean =>
                                         Standard.Boolean'Value
                                        (Types.To_String
                                           (Src
                                                (Heap (Index).First .. Heap (Index).Last))),
                                      others  => <>);
                     Index := Index + 1;
                  when Types.NUL       =>
                     Result.Node_Type := Types.NUL;
                     Result.Value := (Of_Type => Types.NUL,
                                      others  => <>);
                     Index := Index + 1;
                  when Types.Undefined =>
                     Result.Node_Type := Types.Undefined;
                     Result.Value := (Of_Type => Types.Undefined,
                                      others  => <>);
                     Index := Index + 1;
               end case;

            end if;

            --------------------------------------------------
            --------------------------------------------------
            if Implement_Flag
            then
               Parent_Result.Value.Object.Include (Child_Key,
                                                   Node_Link'(Ada.Finalization.Controlled with
                                                     Reference => new JSON.Node'(Result), others => <>) );
            else
               Parent_Result.Value.Vector.Append ( Node_Link'(Ada.Finalization.Controlled with
                                                   Reference => new JSON.Node'(Result), others => <>) );
            end if;
         end Create;
      begin

         if Index <= Positive (Heap.Last_Index) then

            case
                 Heap (Index).Format
               is

               when Types.Object =>
                  Parent := Heap (Index);
                  Result.Node_Type := Types.Object;
                  Result.Value := (Of_Type =>
                                      Types.Object,
                                   others  =>
                                      <>);
                  Index := Index + 1;
                  while Heap (Index).Last < Parent.Last loop
                     Create (Result, Heap, Index, Parent);
                     Index := Index + 1;
                     exit when Index > Heap.Last_Index ;
                  end loop;

               when Types.Vector =>
                  Parent := Heap (Index);
                  Result.Node_Type := Types.Vector;
                  Result.Value := (Of_Type =>
                                      Types.Vector,
                                   others  =>
                                      <>);
                  Index := Index + 1;
                  while Heap (Index).Last < Parent.Last loop
                     Create (Result, Heap, Index, Parent);
                     Index := Index + 1;
                     exit when Index > Heap.Last_Index ;
                  end loop;

               when Types.String =>
                  Result.Node_Type := Types.String;
                  Result.Value := (Of_Type =>
                                      Types.String,
                                   String  =>
                                      To_Unbounded_Wide_Wide_String
                                     (Wide_Wide_String'(Types.Value
                                      (Src
                                           (Heap (Index).First .. Heap (Index).Last)))),
                                   others  => <>);
                  Index := Index + 1;
               when Types.Number    =>
                  Result.Node_Type := Types.Number;
                  Result.Value := (Of_Type =>
                                      Types.Number,
                                   Number  =>
                                      Long_Long_Float'(Types.Value
                                     (Src
                                        (Heap (Index).First .. Heap (Index).Last))),
                                   others  => <>);
                  Index := Index + 1;
               when Types.Boolean   =>
                  Result.Node_Type := Types.Boolean;
                  Result.Value := (Of_Type =>
                                      Types.Boolean,
                                   Boolean =>
                                      Standard.Boolean'Value
                                     (Types.To_String
                                        (Src
                                             (Heap (Index).First .. Heap (Index).Last))),
                                   others  => <>);
                  Index := Index + 1;
               when Types.NUL       =>
                  Result.Node_Type := Types.NUL;
                  Result.Value := (Of_Type => Types.NUL,
                                   others  => <>);
                  Index := Index + 1;
               when Types.Undefined =>
                  Result.Node_Type := Types.Undefined;
                  Result.Value := (Of_Type => Types.Undefined,
                                   others  => <>);
                  Index := Index + 1;
            end case;

         end if;

      end;

      return Result;

   exception
      when Error.Syntax_Extra_Close_Object =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "JSON Import Failed at Json_String[ " & G_Cursor'Image & " ] => "
            & "Unexpected close object brace:  }");
         return Result;
      when Error.Syntax_Extra_Close_Array =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "JSON Import Failed at Json_String[ " & G_Cursor'Image & " ] => "
            & "Unexpected close array bracket:  ]");
         return Result;
      when Error.Syntax_Extra_Quotation =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "JSON Import Failed at Json_String[ " & G_Cursor'Image & " ] => "
            & "Unexpected quotation mark:  """);
         return Result;
      when Error.Syntax_Invalid_Literal =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "JSON Import Failed at Json_String[ " & G_Cursor'Image & " ] => "
            & "Invalid JSON literal:  null | true | false | <number>");
         return Result;
      when Error.Syntax_No_Close_Object =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "JSON Import Failed at Json_String[ " & G_Cursor'Image & " ] => "
            & "Unexpected end of JSON_String, No end of OBJECT");
         return Result;
      when Error.Syntax_No_Close_Array =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "JSON Import Failed at Json_String[ " & G_Cursor'Image & " ] => "
            & "Unexpected end of JSON_String, No end of ARRAY");
         return Result;
      when Error.Syntax_No_Close_String =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "JSON Import Failed at Json_String[ " & G_Cursor'Image & " ] => "
            & "Unexpected end of JSON_String, No end of STRING");
         return Result;
      when Error.Syntax_Invalid_Value =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "JSON Import Failed at Json_String[ " & G_Cursor'Image & " ] => "
            & "Unexpected Character: " & Types.To_String (Src (G_Cursor .. G_Cursor)));
         return Result;
      when Error.Syntax_Invalid_Escape =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "JSON Import Failed at Json_String[ " & G_Cursor'Image & " ] => "
            & "Invalid Escape Sequence: " & Types.To_String (Src (G_Cursor .. G_Cursor)));
         return Result;
   end Import;

   ------------
   -- Export --
   ------------

   function Export (Source : in Node) return Json_String is
      Src_Type : Types.Json_Type := Source.Node_Type;

      function Image_Object_Children (Parent : Node)
                                      return Json_String
      is
         use Ada.Containers;
         List    : array (1 .. Parent.Value.Object.Length) of Of_Object.Cursor;
         Index   : Count_Type := 0;
         procedure Image_Map (Position : Of_Object.Cursor) is
         begin
            Index := Index + 1;
            List (Index) := Position;
         end;

         function Image_List
           (Index    : in Count_Type)
            return Json_String
         is
            Key_Str : Json_String := Types.Image (Of_Object.Key (List (Index)));
            Val_Str : Json_String := Export (Source => Of_Object.Element (List (Index)).Reference.all);
         begin

            if
              Index < List'Last
            then
               return '"' & Key_Str & '"' & ": " & Val_Str & ", " & Image_List (Index + 1);
            else
               return '"' & Key_Str & '"' & ": " & Val_Str;
            end if;

         end;

      begin
         if
           Parent.Value.Object.Is_Empty
         then
            return "";
         else
            Parent.Value.Object.Iterate (Image_Map'Access);
            return Image_List (List'First);
         end if;
      end Image_Object_Children;

      function Image_Vector_Children (Parent : Node)
                                      return Json_String
      is
         use Ada.Containers;
         List    : array (1 .. Parent.Value.Vector.Length) of Of_Array.Cursor;
         Index   : Count_Type := 0;
         procedure Image_Map (Position : Of_Array.Cursor) is
         begin
            Index := Index + 1;
            List (Index) := Position;
         end;

         function Image_List
           (Index    : in Count_Type)
            return Json_String
         is
            Val_Str : Json_String := Export (Source => Of_Array.Element (List (Index)).Reference.all);
         begin

            if
              Index < List'Last
            then
               return Val_Str & ", " & Image_List (Index + 1);
            else
               return Val_Str;
            end if;

         end;

      begin
         if
           Parent.Value.Vector.Is_Empty
         then
            return "";
         else
            Parent.Value.Vector.Iterate (Image_Map'Access);
            return Image_List (List'First);
         end if;
      end Image_Vector_Children;

      function Json_Image (Source : in Node) return Json_String is
      begin
         return "";
      end Json_Image;

   begin

      case Source.Node_Type is
         when Types.Object    =>
            return "{" & Image_Object_Children (Source) & "}";
         when Types.Vector    =>
            return "[" & Image_Vector_Children (Source) & "]";
         when Types.NUL       =>
            return "null";
         when Types.Boolean   =>
            return (if Source.Value.Boolean then "true" else "false");
         when Types.Number    =>
            return Types.Image (Source.Value.Number);
         when Types.String    =>
            return Types.Image (To_Wide_Wide_String (Source.Value.String));
         when Types.Undefined =>
            raise Error.Undefined with "Attempted to export an undefined Node.";
            return "null";
      end case;

   end Export;

   ---------
   -- Set --
   ---------

   procedure Set (Target : in out Node; Value : in Boolean) is
   begin
      Target :=
        (Node_Type => Types.Boolean,
         Value     => (Of_Type   => Types.Boolean,
                       Boolean   => Value));
   end Set;

   ----------
   -- SetS --
   ----------

   procedure SetS (Target : in out Node; Value : in String) is
   begin
      Target :=
        (Node_Type => Types.String,
         Value     => (Of_Type   => Types.String,
                       String    => To_Unbounded_Wide_Wide_String
                         (Types.To_Wide_String (Value))));
   end SetS;

   ---------
   -- Set --
   ---------

   procedure Set (Target : in out Node; Value : in Wide_Wide_String) is
   begin
      Target :=
        (Node_Type => Types.String,
         Value     => (Of_Type   => Types.String,
                       String    => To_Unbounded_Wide_Wide_String
                         (Value)));
   end Set;

   ----------
   -- SetI --
   ----------

   procedure SetI (Target : in out Node; Value : in Integer) is
   begin
      Target :=
        (Node_Type => Types.Number,
         Value     => (Of_Type   => Types.Number,
                       Number    => Long_Long_Float (Value)));
   end SetI;

   ---------
   -- Set --
   ---------

   procedure Set (Target : in out Node; Value : in Long_Long_Integer'Base) is
   begin
      Target :=
        (Node_Type => Types.Number,
         Value     => (Of_Type   => Types.Number,
                       Number    => Long_Long_Float (Value)));
   end Set;

   ----------
   -- SetF --
   ----------

   procedure SetF (Target : in out Node; Value : in Float) is
   begin
      Target :=
        (Node_Type => Types.Number,
         Value     => (Of_Type   => Types.Number,
                       Number    => Long_Long_Float (Value)));
   end SetF;

   ---------
   -- Set --
   ---------

   procedure Set (Target : in out Node; Value : in Long_Long_Float'Base) is
   begin
      Target :=
        (Node_Type => Types.Number,
         Value     => (Of_Type   => Types.Number,
                       Number    => Long_Long_Float (Value)));
   end Set;

   ---------
   -- Get --
   ---------

   function Get (Source : in out Node) return Boolean is
   begin
      case Source.Node_Type
         is
         when Types.Boolean =>
            return Source.Value.Boolean;
         when others =>
            raise Error.Not_Boolean;
            return False;
      end case;
   end Get;

   ----------
   -- GetS --
   ----------

   function GetS (Source : in out Node) return String is
   begin
      case Source.Node_Type
         is
         when Types.String =>
            return Types.To_String
              (To_Wide_Wide_String (Source.Value.String));
         when others =>
            raise Error.Not_String;
            return "";
      end case;
   end GetS;

   ---------
   -- Get --
   ---------

   function Get (Source : in out Node) return Wide_Wide_String is
   begin
      case Source.Node_Type
         is
         when Types.String =>
            return To_Wide_Wide_String (Source.Value.String);
         when others =>
            raise Error.Not_String;
            return "";
      end case;
   end Get;

   ----------
   -- GetI --
   ----------

   function GetI (Source : in out Node) return Integer is
   begin
      case Source.Node_Type
         is
         when Types.Number =>
            return Integer (Source.Value.Number);
         when others =>
            raise Error.Not_Number;
            return 0;
      end case;
   end GetI;

   ---------
   -- Get --
   ---------

   function Get (Source : in out Node) return Long_Long_Integer is
   begin
      case Source.Node_Type
         is
         when Types.Number =>
            return Long_Long_Integer (Source.Value.Number);
         when others =>
            raise Error.Not_Number;
            return 0;
      end case;
   end Get;

   ----------
   -- GetF --
   ----------

   function GetF (Source : in out Node) return Float is
   begin
      case Source.Node_Type
         is
         when Types.Number =>
            return Float (Source.Value.Number);
         when others =>
            raise Error.Not_Number;
            return 0.0;
      end case;
   end GetF;

   ---------
   -- Get --
   ---------

   function Get (Source : in out Node) return Long_Long_Float is
   begin
      case Source.Node_Type
         is
         when Types.Number =>
            return Long_Long_Float (Source.Value.Number);
         when others =>
            raise Error.Not_Number;
            return 0.0;
      end case;
   end Get;

   ----------
   -- Copy --
   ----------

   function Copy (Source : in Node) return Node is
      New_Node : Node := Source;
   begin
      return New_Node;
   end Copy;

   ------------
   -- Length --
   ------------

   function Length (Json : in Node) return Natural is
      use Ada.Strings.Wide_Wide_Unbounded;
   begin
      return (case Json.Node_Type is
                 when Types.Object => Natural (Json.Value.Object.Length),
                 when Types.Vector => Natural (Json.Value.Vector.Length),
                 when Types.String => Natural (Length (Json.Value.String)),
                 when others       => 0);
   end Length;

   -----------
   -- Adapt --
   -----------

   function Adapt (Input : in Json_String) return Wide_Wide_String is
      Result : Wide_Wide_String := Types.Value (Image => Input);
   begin
      return Result;
   end Adapt;

   -----------
   -- Adapt --
   -----------

   function Adapt (Input : in Wide_Wide_String) return Json_String is
      Result : Json_String := Types.Image (Value => Input);
   begin
      return Result;
   end Adapt;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference
     (Object : in out Node;
      Key    : Wide_Wide_String)
      return Reference
   is
      function "=" (Left, Right : Types.Json_Type)return Boolean renames Types."=";
      Target : Node_Access := Object'Unrestricted_Access;
   begin
      if
        Object.Node_Type = Types.Object
      then
         return JSON.Reference'(Actual => Object.Value.Object.Element (Key).Reference);
      else
         raise Error.Not_Object;
         return Json.Reference'(Actual => Object'Access);
      end if;
   end Get_Reference;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference
     (Object : in out Node;
      Key    : Positive)
      return Reference
   is
      function "=" (Left, Right : Types.Json_Type)return Boolean renames Types."=";
      Target : Node_Access := Object'Unrestricted_Access;
   begin
      if
        Object.Node_Type = Types.Vector
      then
         return JSON.Reference'(Actual => Object.Value.Vector.Element (Key).Reference);
      else
         raise Error.Not_Vector;
         return Json.Reference'(Actual => Object'Access);
      end if;
   end Get_Reference;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference
     (Object : in out Node;
      Key    : Types.Json_Path)
      return Reference
   is
      function "=" (Left, Right : Types.Json_Type)return Boolean renames Types."=";
      Target : Node_Access := Object'Unrestricted_Access;
   begin
      for T of Key loop
         case T.Element_Type is
            when Types.Number | Types.Vector =>
               case Target.Node_Type is
                  when Types.Vector =>
                     Target := Target.Value.Vector.Element (T.Number).Reference;
                  when others =>
                     declare
                        New_Node : JSON.Node;
                     begin
                        Target.all := (Node_Type => Types.Vector,
                                       others    => <>);
                        Target.Value.Vector.Append (Node_Link'(Ada.Finalization.Controlled with
                                                    Reference => new JSON.Node'(New_Node), others => <>));
                        Target := Target.Value.Vector.Element (T.Number).Reference;
                        raise Error.Not_Vector with "Path Element given as number, "
                          & "but JSON Element is not an array.  Since this is an "
                          & "assignment, The Path specified should now be created.";
                     exception
                        when Error.Not_Vector =>
                           null;
                     end;
               end case;
            when Types.String | Types.Object =>
               case Target.Node_Type is
                  when Types.Object =>
                     Target := Target.Value.Object.Element (To_Wide_Wide_String (T.String)).Reference;
                  when others =>
                     declare
                        New_Node : JSON.Node;
                     begin
                        Target.all := (Node_Type => Types.Vector,
                                       Value     => (Types.Vector, others => <>),
                                       others    => <>);
                        Target.Value.Object.Include (Key      => To_Wide_Wide_String (T.String),
                                                     New_Item => Node_Link'(Ada.Finalization.Controlled with
                                                       Reference => new JSON.Node'(New_Node), others => <>));
                        Target := Target.Value.Object.Element (To_Wide_Wide_String (T.String)).Reference;
                        raise Error.Not_Object with "Path Element given as string, "
                          & "but JSON Element is not an object.  Since this is an "
                          & "assignment, The Path specified should now be created.";
                     exception
                        when Error.Not_Object =>
                           null;
                     end;
               end case;
         end case;
      end loop;
      raise Program_Error with "Unimplemented function Get_Reference";
      return JSON.Reference'(Actual => Target);
   end Get_Reference;

   ----------------------------
   -- Get_Constant_Reference --
   ----------------------------

   function Get_Constant_Reference
     (Object : in out Node;
      Key    : Types.Json_Path)
      return Constant_Reference
   is
      function "=" (Left, Right : Types.Json_Type)return Boolean renames Types."=";
      Target : Node_Access := Object'Unrestricted_Access;
   begin
      for T of Key loop
         case T.Element_Type is
            when Types.Number | Types.Vector =>
               case Target.Node_Type is
                  when Types.Vector =>
                     Target := Target.Value.Vector.Element (T.Number).Reference;
                  when others =>
                     raise Error.Not_Vector with "Path Element given as number, "
                       & "but JSON Element is not an array.  Since this is a "
                       & "value copy, the Path will stop before here. This should"
                       & "be erroneous, please make note.";
               end case;
            when Types.String | Types.Object =>
               case Target.Node_Type is
                  when Types.Object =>
                     Target := Target.Value.Object.Element (To_Wide_Wide_String (T.String)).Reference;
                  when others =>
                     raise Error.Not_Object with "Path Element given as string, "
                       & "but JSON Element is not an object.  Since this is a "
                       & "value copy, the Path will stop before here. This should"
                       & "be erroneous, please make note.";
               end case;
         end case;
      end loop;
      return JSON.Constant_Reference'(Actual => Target);
   exception
      when Error.Not_Vector | Error.Not_Object =>
         return JSON.Constant_Reference'(Actual => Target);
   end Get_Constant_Reference;

   ----------------------------
   -- Get_Constant_Reference --
   ----------------------------

   function Get_Constant_Reference
     (Object : in out Node;
      Key    : Wide_Wide_String)
      return Constant_Reference
   is
      function "=" (Left, Right : Types.Json_Type)return Boolean renames Types."=";
      Target : Node_Access := Object'Unrestricted_Access;
   begin
      case Target.Node_Type is
         when Types.Object =>
            Target := Target.Value.Object.Element (Key => Key).Reference;
         when others =>
            raise Error.Not_Object with "Path Element given as string, "
              & "but JSON Element is not an object.  Since this is a "
              & "value copy, the Path will stop before here. This should"
              & "be erroneous, please make note.";
      end case;
      return JSON.Constant_Reference'(Actual => Target);
   end Get_Constant_Reference;

   ----------------------------
   -- Get_Constant_Reference --
   ----------------------------

   function Get_Constant_Reference
     (Object : in out Node;
      Key    : Integer)
      return Constant_Reference
   is
      function "=" (Left, Right : Types.Json_Type)return Boolean renames Types."=";
      Target : Node_Access := Object'Unrestricted_Access;
   begin
      case Target.Node_Type is
         when Types.Vector =>
            Target := Target.Value.Vector.Element (Index => Key).Reference;
         when others =>
            raise Error.Not_Vector with "Path Element given as number, "
              & "but JSON Element is not an array.  Since this is a "
              & "value copy, the Path will stop before here. This should"
              & "be erroneous, please make note.";
      end case;
      return JSON.Constant_Reference'(Actual => Target);
   end Get_Constant_Reference;


   -------------
   -- Iterate --
   -------------

   procedure Iterate (Node    : in out JSON.Node;
                      Process : not null access procedure (Each_Node : in out JSON.Node))
   is
      procedure Run_On_Object (Target : Of_Object.Cursor) is
      begin
         Process (Of_Object.Element (Target).Reference.all);
      end;
      procedure Run_On_Vector (Target : Of_Array.Cursor) is
      begin
         Process (Of_Array.Element (Target).Reference.all);
      end;
   begin
      case Node.Node_Type is
         when Types.Object =>
            Node.Value.Object.Iterate (Process => Run_On_Object'Access);
         when Types.Vector =>
            Node.Value.Vector.Iterate (Process => Run_On_Vector'Access);
         when others =>
            null;
      end case;
   end Iterate;

   -----------------------------------------------------------------------------
   -- PRIVATE
   -----------------------------------------------------------------------------

   ----------------
   -- Node_Write --
   ----------------

   procedure Node_Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Node)
   is
   begin
      String'Write (Stream, Export (Item));
   end Node_Write;

   ---------------
   -- Node_Read --
   ---------------

   procedure Node_Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Node)
   is
   begin
      null;
   end Node_Read;

   ----------------
   -- Node_Image --
   ----------------

   function Node_Image (Item : in Node) return String is
   begin
      return String'(Export (Item));
   end Node_Image;

   -----------------------
   -- Destroy_Node_Tree --
   -----------------------

   procedure Destroy_Node_Tree (Target : in out Node) is
   begin
      Target := (Node_Type => Types.Undefined,
                 Value     =>
                   (Of_Type => Types.Undefined,
                    others  => <>));
   end Destroy_Node_Tree;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Item : in out Node_Link) is null;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Item : in out Node_Link) is null;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Item : in out Node_Link) is
      Reference : Node_Access := Item.Reference;
   begin
      if Item.Reference /= null
      then Free (Reference); end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Item : in out Node_Stub) is
   begin
      null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Item : in out Node_Stub) is
   begin
      null;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Item : in out Node_Stub) is
   begin
      null;
   end Finalize;

end JSON;

--
--  := (-- '"'                         => True, '\'            => True, '/' => True, 'b' => True,
--                    --'f'                         => True, 'n'            => True, 'r' => True, others => False,
--                    Latin.NUL                                => False,
--                    SOH                                      => False,
--                    STX                                      => False,
--                    ETX                                      => False,
--                    EOT                                      => False,
--                    ENQ                                      => False,
--                    ACK                                      => False,
--                    BEL                                      => False,
--                    BS                                       => True,
--                    HT                                       => True,
--                    LF                                       => True,
--                    VT                                       => False,
--                    FF                                       => True,
--                    CR                                       => True,
--                    SO                                       => False,
--                    SI                                       => False,
--
--                    DLE                                      => False,
--                    DC1                                      => False,
--                    DC2                                      => False,
--                    DC3                                      => False,
--                    DC4                                      => False,
--                    NAK                                      => False,
--                    SYN                                      => False,
--                    ETB                                      => False,
--                    CAN                                      => False,
--                    EM                                       => False,
--                    SUB                                      => False,
--                    ESC                                      => False,
--                    FS                                       => False,
--                    GS                                       => False,
--                    RS                                       => False,
--                    US                                       => False,
--                    '"'                                      => True,
--                    '\'                                      => True,
--                    '/'                                      => True,
--                    others                                   => False);
