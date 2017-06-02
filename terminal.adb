package body Terminal is

   -----------------
   -- Cursor_Move --
   -----------------

   procedure Cursor_Move (X : Integer; Y : Integer) is
      use Ada.Characters.Latin_1;
   begin
      case Y is
         when 0 =>
            null;
         when 1 .. Integer'Last =>
            String'Write
              (Stream
                 (Terminal_Output),
               ESC & '[' & To_String (Natural'(Y)) & 'C');
         when Integer'First .. -1 =>
            String'Write
              (Stream
                 (Terminal_Output),
               ESC & '[' & To_String (Natural'(-Y)) & 'D');
      end case;
      case X is
         when 0 =>
            null;
         when 1 .. Integer'Last =>
            String'Write
              (Stream
                 (Terminal_Output),
               ESC & '[' & To_String (Natural'(X)) & 'C');
         when Integer'First .. -1 =>
            String'Write
              (Stream
                 (Terminal_Output),
               ESC & '[' & To_String (Natural'(-X)) & 'D');
      end case;
   end Cursor_Move;

   -----------------
   -- Screen_Size --
   -----------------

   function Screen_Size return Coord is
   begin
      Codes.Save_Cursor.Apply;
      CUP (999,999)

   ---------------------
   -- Cursor_Position --
   ---------------------

   procedure Cursor_Position (X : Integer; Y : Integer) is
      Lines : String;
      Columns : String;

   begin
      case Y is
         when 0 =>
            null;
         when 1 .. Integer'Last =>
            String'Write
              (Stream
                 (Terminal_Output),
               ESC & '[' & To_String (Natural'(Y)) & 'C');
         when Integer'First .. -1 =>
            String'Write
              (Stream
                 (Terminal_Output),
               ESC & '[' & To_String (Natural'(-Y)) & 'D');
      end case;
      case X is
         when 0 =>
            null;
         when 1 .. Integer'Last =>
            String'Write
              (Stream
                 (Terminal_Output),
               ESC & '[' & To_String (Natural'(X)) & 'C');
         when Integer'First .. -1 =>
            String'Write
              (Stream
                 (Terminal_Output),
               ESC & '[' & To_String (Natural'(-X)) & 'D');
      end case;
      String'Write
        (Stream
           (Terminal_Output),
         ESC & '['
         & To_String (Natural'(Y))
         & To_String (Natural'(X))
         & 'H');
   end Cursor_Position;

   -----------------
   -- Cursor_Move --
   -----------------

   procedure Cursor_Move (Relative_Position : Coord := (0, 0)) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Cursor_Move unimplemented");
      raise Program_Error with "Unimplemented procedure Cursor_Move";
   end Cursor_Move;

   ---------------------
   -- Cursor_Position --
   ---------------------

   procedure Cursor_Position (Absolute_Position:Coord:=(1,1)) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Cursor_Position unimplemented");
      raise Program_Error with "Unimplemented procedure Cursor_Position";
   end Cursor_Position;

   ---------------------
   -- Cursor_Position --
   ---------------------

   function Cursor_Position return Coord is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Cursor_Position unimplemented");
      raise Program_Error with "Unimplemented function Cursor_Position";
      return Cursor_Position;
   end Cursor_Position;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Clear unimplemented");
      raise Program_Error with "Unimplemented procedure Clear";
   end Clear;

   ----------------
   -- Go_To_Line --
   ----------------

   procedure Go_To_Line (X : Integer) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Go_To_Line unimplemented");
      raise Program_Error with "Unimplemented procedure Go_To_Line";
   end Go_To_Line;

   ---------------
   -- Go_To_Col --
   ---------------

   procedure Go_To_Col (Y : Integer) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Go_To_Col unimplemented");
      raise Program_Error with "Unimplemented procedure Go_To_Col";
   end Go_To_Col;

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Code : Bytes;
      Terminal : File_Type := Terminal_Output)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Apply unimplemented");
      raise Program_Error with "Unimplemented procedure Apply";
   end Apply;

   -----------
   -- Apply --
   -----------

   function Apply (Code : Bytes) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Apply unimplemented");
      raise Program_Error with "Unimplemented function Apply";
      return Apply (Code => Code);
   end Apply;

   ---------
   -- "-" --
   ---------

   function "-" (Color : Bytes) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """-"" unimplemented");
      raise Program_Error with "Unimplemented function ""-""";
      return "-" (Color => Color);
   end "-";

   ---------
   -- "+" --
   ---------

   function "+" (Color_Name : String) return Bytes is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """+"" unimplemented");
      raise Program_Error with "Unimplemented function ""+""";
      return "+" (Color_Name => Color_Name);
   end "+";

   -----------
   -- Color --
   -----------

   function Color (Color_Name : String) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Color unimplemented");
      raise Program_Error with "Unimplemented function Color";
      return Color (Color_Name => Color_Name);
   end Color;

   -----------
   -- Background_Color --
   -----------

   function Background_Color (Color_Name : String) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Color unimplemented");
      raise Program_Error with "Unimplemented function Color";
      return Color (Color_Name => Color_Name);
   end Background_Color;

   ---------
   -- CUB --
   ---------

   function CUB (Columns : Natural := 1) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "CUB unimplemented");
      raise Program_Error with "Unimplemented function CUB";
      return CUB (Columns => Columns);
   end CUB;

   ---------
   -- CUB --
   ---------

   procedure CUB
     (Columns  : Natural := 1;
      Terminal : File_Type := Current_Output)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "CUB unimplemented");
      raise Program_Error with "Unimplemented procedure CUB";
   end CUB;

   ---------
   -- CUD --
   ---------

   function CUD (Rows : Natural := 1) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "CUD unimplemented");
      raise Program_Error with "Unimplemented function CUD";
      return CUD (Rows => Rows);
   end CUD;

   ---------
   -- CUD --
   ---------

   procedure CUD
     (Rows     : Natural := 1;
      Terminal : File_Type := Current_Output)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "CUD unimplemented");
      raise Program_Error with "Unimplemented procedure CUD";
   end CUD;

   ---------
   -- CUF --
   ---------

   function CUF (Columns : Natural := 1) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "CUF unimplemented");
      raise Program_Error with "Unimplemented function CUF";
      return CUF (Columns => Columns);
   end CUF;

   ---------
   -- CUF --
   ---------

   procedure CUF
     (Columns  : Natural := 1;
      Terminal : File_Type := Current_Output)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "CUF unimplemented");
      raise Program_Error with "Unimplemented procedure CUF";
   end CUF;

   ---------
   -- CUU --
   ---------

   function CUU (Rows : Natural := 1) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "CUU unimplemented");
      raise Program_Error with "Unimplemented function CUU";
      return CUU (Rows => Rows);
   end CUU;

   ---------
   -- CUU --
   ---------

   procedure CUU
     (Rows     : Natural := 1;
      Terminal : File_Type := Current_Output)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "CUU unimplemented");
      raise Program_Error with "Unimplemented procedure CUU";
   end CUU;

   ---------
   -- CUP --
   ---------

   function CUP
     (Row    : Natural := 1;
      Column : Natural := 1)
      return String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "CUP unimplemented");
      raise Program_Error with "Unimplemented function CUP";
      return CUP (Row => Row, Column => Column);
   end CUP;

   ---------
   -- CUP --
   ---------

   procedure CUP
     (Row      : Natural := 1;
      Column   : Natural := 1;
      Terminal : File_Type := Current_Output)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "CUP unimplemented");
      raise Program_Error with "Unimplemented procedure CUP";
   end CUP;

   ----------------
   -- Get_Report --
   ----------------

   function Get_Report (C : Character) return Values is

      function Image_Val (S : String) return String is

         function Get_Terminal_Response return String is
            T : Character := ' ';
            R : String (1 .. 48) := (others => ' ');
            F : Natural := R'First;
            L : Natural := R'First - 1;
         begin
            loop
               Get (File => Terminal_Input, Item => T);
               case T is
                  when ESC =>
                     L := L + 1;
                     R (L) := ' ';
                  when '0' .. '9' =>
                     L := L + 1;
                     R (L) := T;
                  when others =>
                     if T = C
                     then
                        exit;
                     else
                        L := L + 1;
                        R (L) := ' ';
                     end if;
               end case;
               exit when L = R'Last;
               exit when Terminal_Input.End_Of_File;
            end loop;
         end;

         function Value_Natural (S : in String) return String is
            F : Natural := S'First;
            -- First, sub-string start.

            L : Natural := S'First - 1;
            -- Last, sub-string end.

            V : Natural := 0;
            -- Temporary Value to work work with.

         begin

            -- Search for and start with the first numerical character.
            for I in S'Range
            loop
               if Ada.Characters.Handling.Is_Digit (S (I))
               then
                  L := I;
                  F := I;
                  exit;
               end if;
            end loop;

            -- Extend substring to the length of consecutive numbers.
            for I in F .. S'Last
            loop
               if Ada.Characters.Handling.Is_Digit (S (I))
               then
                  L := I;
               else
                  exit;
               end if;
            end loop;

            -- Generate the value from base ten numerical characters.
            for C of S (F .. L)
            loop
               V := (V * 10) + (Character'Pos (C) - 48);
            end loop;

            -- Return string of this character value prepending all subsequent
            -- characters represented in the S string of numbers.
            case L < F is
            when True =>
               return "";
            when False =>
               case L = S'Last is
                  when True =>
                     return "" & Character'Val (V);
                  when False =>
                     return "" &
                       Character'Val (V) &
                       Value_Natural (S (L + 1 .. S'Last));
               end case;
            end case;

         end Value_Natural;

         R : String := Value_Natural (S);
         --  S is input string of number values R is a new string of characters those
         --  number values represent.

      begin
         return R;
      end Image_Val;


   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Report unimplemented");
      raise Program_Error with "Unimplemented function Get_Report";
      return Get_Report (C => C);
   end Get_Report;

   -----------
   -- Codes --
   -----------

   package body Codes is

      --------
      -- Up --
      --------

      function Up (Rows : Integer := 1) return Bytes is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Up unimplemented");
         raise Program_Error with "Unimplemented function Up";
         return Up (Rows => Rows);
      end Up;

      ----------
      -- Down --
      ----------

      function Down (Rows : Integer := 1) return Bytes is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Down unimplemented");
         raise Program_Error with "Unimplemented function Down";
         return Down (Rows => Rows);
      end Down;

      -------------
      -- Forward --
      -------------

      function Forward (Columns : Integer := 1) return Bytes is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Forward unimplemented");
         raise Program_Error with "Unimplemented function Forward";
         return Forward (Columns => Columns);
      end Forward;

      --------------
      -- Backward --
      --------------

      function Backward (Columns : Integer := 1) return Bytes is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Backward unimplemented");
         raise Program_Error with "Unimplemented function Backward";
         return Backward (Columns => Columns);
      end Backward;

   end Codes;

   -------------
   -- To_Byte --
   -------------

   function To_Byte (Input : Character) return Byte is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "To_Byte unimplemented");
      raise Program_Error with "Unimplemented function To_Byte";
      return To_Byte (Input => Input);
   end To_Byte;

   ------------------
   -- To_Character --
   ------------------

   function To_Character (Data : Byte) return Character is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "To_Character unimplemented");
      raise Program_Error with "Unimplemented function To_Character";
      return To_Character (Data => Data);
   end To_Character;

   --------------
   -- To_Bytes --
   --------------

   function To_Bytes (Input : String) return Bytes is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "To_Bytes unimplemented");
      raise Program_Error with "Unimplemented function To_Bytes";
      return To_Bytes (Input => Input);
   end To_Bytes;

   --------------
   -- To_Bytes --
   --------------

   function To_Bytes (Input : Character) return Bytes is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "To_Bytes unimplemented");
      raise Program_Error with "Unimplemented function To_Bytes";
      return To_Bytes (Input => Input);
   end To_Bytes;

   ---------------
   -- To_String --
   ---------------

   function To_String (Data : Bytes) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "To_String unimplemented");
      raise Program_Error with "Unimplemented function To_String";
      return To_String (Data => Data);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Datum: Byte) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "To_String unimplemented");
      raise Program_Error with "Unimplemented function To_String";
      return To_String (Datum => Datum);
   end To_String;

   ---------
   -- "&" --
   ---------

   function "&" (Left : Bytes; Right : Bytes) return Bytes is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """&"" unimplemented");
      raise Program_Error with "Unimplemented function ""&""";
      return "&" (Left => Left, Right => Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Byte; Right : Bytes) return Bytes is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """&"" unimplemented");
      raise Program_Error with "Unimplemented function ""&""";
      return "&" (Left => Left, Right => Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Bytes; Right : Byte) return Bytes is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """&"" unimplemented");
      raise Program_Error with "Unimplemented function ""&""";
      return "&" (Left => Left, Right => Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Bytes; Right : String) return Bytes is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """&"" unimplemented");
      raise Program_Error with "Unimplemented function ""&""";
      return "&" (Left => Left, Right => Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : String; Right : Bytes) return Bytes is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """&"" unimplemented");
      raise Program_Error with "Unimplemented function ""&""";
      return "&" (Left => Left, Right => Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Bytes; Right : Bytes) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """&"" unimplemented");
      raise Program_Error with "Unimplemented function ""&""";
      return "&" (Left => Left, Right => Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Bytes; Right : Byte) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """&"" unimplemented");
      raise Program_Error with "Unimplemented function ""&""";
      return "&" (Left => Left, Right => Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Byte; Right : Bytes) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """&"" unimplemented");
      raise Program_Error with "Unimplemented function ""&""";
      return "&" (Left => Left, Right => Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Bytes; Right : String) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """&"" unimplemented");
      raise Program_Error with "Unimplemented function ""&""";
      return "&" (Left => Left, Right => Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : String; Right : Bytes) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """&"" unimplemented");
      raise Program_Error with "Unimplemented function ""&""";
      return "&" (Left => Left, Right => Right);
   end "&";


   ---------
   -- "&" --
   ---------

   function "&" (Left : Values; Right : Values) return Values is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """&"" unimplemented");
      raise Program_Error with "Unimplemented function ""&""";
      return "&" (Left => Left, Right => Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Values; Right : Natural) return Values is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """&"" unimplemented");
      raise Program_Error with "Unimplemented function ""&""";
      return "&" (Left => Left, Right => Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Natural; Right : Bytes) return Values is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """&"" unimplemented");
      raise Program_Error with "Unimplemented function ""&""";
      return "&" (Left => Left, Right => Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Natural; Right : Natural) return Values is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """&"" unimplemented");
      raise Program_Error with "Unimplemented function ""&""";
      return "&" (Left => Left, Right => Right);
   end "&";

   ---------
   -- "&" --
   ---------


   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Input : File_Type := Current_Input;
      Output : File_Type := Current_Output)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Initialize unimplemented");
      raise Program_Error with "Unimplemented procedure Initialize";
   end Initialize;

   --------------------
   -- Terminal_Input --
   --------------------

   function Terminal_Input return File_Type is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Terminal_Input unimplemented");
      raise Program_Error with "Unimplemented function Terminal_Input";
      return Terminal_Input;
   end Terminal_Input;

   ---------------------
   -- Terminal_Output --
   ---------------------

   function Terminal_Output return File_Type is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Terminal_Output unimplemented");
      raise Program_Error with "Unimplemented function Terminal_Output";
      return Terminal_Output;
   end Terminal_Output;




   function To_String (Number : Integer)return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "To_String unimplemented");
      raise Program_Error with "Unimplemented function To_String";
      return Terminal_Output;
   end To_String;


   function To_String (Numbers : Values) return String Is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "To_String unimplemented");
      raise Program_Error with "Unimplemented function To_String";
      return Terminal_Output;
   end To_String;




end Terminal;
