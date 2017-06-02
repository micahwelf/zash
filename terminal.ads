with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with GNAT.TTY;
with GNAT.OS_Lib;
with Interfaces.C_Streams;
with Ada.Characters.Latin_1;

package Terminal is
   -- Implementation of VT100 commands for typical terminal.
   -- MS Windows/DOS command prompt support may come later, but modern
   -- MS operating systems inclued VT100 support.
   use Ada.Text_IO;
   use Text_Streams;
   use GNAT;
   use OS_Lib;
   use TTY;
   use Interfaces;
   use C_Streams;

   type Byte is new Integer range 0 .. 255;
   type Bytes is array (Positive range <>) of Byte;
   type Values is array (Positive range <>)of Natural;

--     type Cursor_Direction is (Right, Down, Left, Up);
--     type Cursor_Graphics_Mode is (Reset,
--                                   Bold,
--                                   Dim,
--                                   Underline,
--                                   Blink,
--                                   Inverse,
--                                   Hidden);

   type Coord is record
      Column : Integer := 1;
      Row    : Integer := 1;
   end record;


   procedure Cursor_Move (X : Integer; Y : Integer);
   function Screen_Size return Coord;
   procedure Cursor_Position (X : Integer; Y : Integer);
   procedure Cursor_Move (Relative_Position : Coord := (0, 0));
   procedure Cursor_Position (Absolute_Position:Coord:=(1,1));
   function Cursor_Position return Coord;

   procedure Clear;
   procedure Go_To_Line (X : Integer);
   procedure Go_To_Col (Y : Integer);


   -- Initialize Terminal type and variables.
   procedure Initialize (
                         -- Specify the input File_Type that is connected to a terminal.
                         Input  : File_Type := Current_Input;

                         -- Specify the output File_Type that is connected to a terminal.
                         Output : File_Type := Current_Output);

   function Terminal_Input return File_Type;
   function Terminal_Output return File_Type;

   -- Put the Codes to the given output File / Descriptor.
   procedure Apply
     (Code : Bytes;
      Terminal : File_Type := Terminal_Output);

   function Apply (Code : Bytes) return String;
   function "-" (Color : Bytes) return String;
   function "+" (Color_Name : String) return Bytes;
   function Color (Color_Name : String) return String;
   function Background_Color (Color_Name : String) return String;

   -- Cursor Position Report - VT100 to Host
   --     procedure CPR (Row      : Natural := 0;
   --                    Column   : Natural := 0;
   --                    Terminal : File_Type := Current_Output);

   -- Cursor Backward - Host to VT100 and VT100 to Host
   function CUB (Columns : Natural := 1) return String;
   procedure CUB (Columns  : Natural := 1;
                  Terminal : File_Type := Current_Output);

   -- Cursor Down - Host to VT100 and VT100 to Host
   function CUD (Rows : Natural := 1) return String;
   procedure CUD (Rows     : Natural := 1;
                  Terminal : File_Type := Current_Output);

   -- Cursor Forward - Host to VT100 and VT100 to Host
   function CUF (Columns : Natural := 1) return String;
   procedure CUF (Columns  : Natural := 1;
                  Terminal : File_Type := Current_Output);

   -- Cursor Up - Host to VT100 and VT100 to Host
   function CUU (Rows : Natural := 1) return String;
   procedure CUU (Rows     : Natural := 1;
                  Terminal : File_Type := Current_Output);

   -- Cursor to Position
   function CUP (Row    : Natural := 1;
                 Column : Natural := 1) return String;
   procedure CUP (Row      : Natural := 1;
                  Column   : Natural := 1;
                  Terminal : File_Type := Current_Output);

   -- Get code from VT100 starting with ESC and ending with C, character.
   function Get_Report (C : Character) return Values;


   -- Library of constant Bytes for quick embedding
   package Codes is

      ESC : constant Character := Ada.Characters.Latin_1.ESC;

      Black              : constant Bytes := (27, 91, 51, 48, 109);
      Red                : constant Bytes := (27, 91, 51, 49, 109);
      Green              : constant Bytes := (27, 91, 51, 50, 109);
      Yellow             : constant Bytes := (27, 91, 51, 51, 109);

      Blue               : constant Bytes := (27, 91, 51, 52, 109);
      Violet             : constant Bytes := (27, 91, 51, 53, 109);
      Cyan               : constant Bytes := (27, 91, 51, 54, 109);
      White              : constant Bytes := (27, 91, 51, 55, 109);

      Default            : constant Bytes := (27, 91, 51, 57, 109);
      Magenta            : constant Bytes := (27, 91, 51, 53, 109);
      Turquoise          : constant Bytes := (27, 91, 51, 54, 109);

      Light_Black        : constant Bytes := (27, 91, 57, 48, 109);
      Light_Red          : constant Bytes := (27, 91, 57, 49, 109);
      Light_Green        : constant Bytes := (27, 91, 57, 50, 109);
      Light_Yellow       : constant Bytes := (27, 91, 57, 51, 109);

      Light_Blue         : constant Bytes := (27, 91, 57, 52, 109);
      Light_Violet       : constant Bytes := (27, 91, 57, 53, 109);
      Light_Cyan         : constant Bytes := (27, 91, 57, 54, 109);
      Light_White        : constant Bytes := (27, 91, 57, 55, 109);

      Black_Background   : constant Bytes := (27, 91, 52, 48, 109);
      Red_Background     : constant Bytes := (27, 91, 52, 49, 109);
      Green_Background   : constant Bytes := (27, 91, 52, 50, 109);
      Yellow_Background  : constant Bytes := (27, 91, 52, 51, 109);

      Blue_Background    : constant Bytes := (27, 91, 52, 52, 109);
      Violet_Background  : constant Bytes := (27, 91, 52, 53, 109);
      Cyan_Background    : constant Bytes := (27, 91, 52, 54, 109);
      White_Background   : constant Bytes := (27, 91, 52, 55, 109);

      Default_Background : constant Bytes := (27, 91, 52, 57, 109);
      Magenta_Background  : constant Bytes := (27, 91, 52, 53, 109);
      Turquoise_Background    : constant Bytes := (27, 91, 52, 54, 109);

      -- Light_Black_Background        : constant Bytes := (27, 91, 57, 48, 109); -- 57 might be wrong.
      -- Light_Red_Background          : constant Bytes := (27, 91, 57, 49, 109);
      -- Light_Green_Background        : constant Bytes := (27, 91, 57, 50, 109);
      -- Light_Yellow_Background       : constant Bytes := (27, 91, 57, 51, 109);

      -- Light_Blue_Background         : constant Bytes := (27, 91, 57, 52, 109);
      -- Light_Violet_Background       : constant Bytes := (27, 91, 57, 53, 109);
      -- Light_Cyan_Background         : constant Bytes := (27, 91, 57, 54, 109);
      -- Light_White_Background        : constant Bytes := (27, 91, 57, 55, 109);

      -- Set font attribute where supported.
      Plain              : constant Bytes := (27, 91, 48, 109);
      Bold               : constant Bytes := (27, 91, 49, 109);
      Dim                : constant Bytes := (27, 91, 50, 109);
      Italic             : constant Bytes := (27, 91, 51, 109);
      UnderScore         : constant Bytes := (27, 91, 52, 109);
      Blink              : constant Bytes := (27, 91, 53, 109);
      Inverse            : constant Bytes := (27, 91, 55, 109);
      Reverse_Video      : constant Bytes := (27, 91, 55, 109);
      Invisible          : constant Bytes := (27, 91, 56, 109);
      Hidden             : constant Bytes := (27, 91, 56, 109);

      -- Reset sets all programable terminal settings to original or default.
      -- Clear clears the screen without moving the cursor.
      -- Reset_ANSI enters ANSI Mode as if current mode is VT52, then Resets.
      Reset              : constant Bytes := (27, 99);
      Clear              : constant Bytes := (27, 91, 50, 74);
      Reset_ANSI         : constant Bytes := (27, 69, 27, 99);

      -- Save / Restore only cursor position.
      Save_Coord         : constant Bytes := (27, 91, 115);
      Restore_Coord      : constant Bytes := (27, 91, 117);

      -- Save / Restore both cursor position and attributes.
      Save_Cursor        : constant Bytes := (27, 55);
      Restore_Cursor     : constant Bytes := (27, 56);

      function Up (Rows : Integer := 1) return Bytes; -- ESC & "[1A"
      function Down (Rows : Integer := 1) return Bytes; -- ESC & "[1B"
      function Forward (Columns : Integer := 1) return Bytes; -- ESC & "[1C"
      function Right (Columns : Integer := 1) return Bytes renames Forward;
      function Backward (Columns : Integer := 1) return Bytes; -- ESC & "[1D"
      function Left (Columns : Integer := 1) return Bytes renames Backward;

      LED_Reset          : constant Bytes := (27, 91, 48, 113);
      LED_Scroll         : constant Bytes := (27, 91, 49, 113);
      LED_Caps           : constant Bytes := (27, 91, 51, 113);
      LED_1              : constant Bytes := (27, 91, 49, 113);
      LED_2              : constant Bytes := (27, 91, 50, 113);
      LED_3              : constant Bytes := (27, 91, 51, 113);
      LED_4              : constant Bytes := (27, 91, 52, 113);

      Key_Up             : constant Bytes := (27, 91, 65);
      Key_Down           : constant Bytes := (27, 91, 66);
      Key_Right          : constant Bytes := (27, 91, 67);
      Key_Left           : constant Bytes := (27, 91, 68);

      Color_0            : constant Bytes := (27, 91, 51, 48, 109);
      Color_1            : constant Bytes := (27, 91, 51, 49, 109);
      Color_2            : constant Bytes := (27, 91, 51, 50, 109);
      Color_3            : constant Bytes := (27, 91, 51, 51, 109);
      Color_4            : constant Bytes := (27, 91, 51, 52, 109);
      Color_5            : constant Bytes := (27, 91, 51, 53, 109);
      Color_6            : constant Bytes := (27, 91, 51, 54, 109);
      Color_7            : constant Bytes := (27, 91, 51, 55, 109);
      Color_8            : constant Bytes := (27, 91, 57, 48, 109);
      Color_9            : constant Bytes := (27, 91, 57, 49, 109);
      Color_10           : constant Bytes := (27, 91, 57, 50, 109);
      Color_11           : constant Bytes := (27, 91, 57, 51, 109);
      Color_12           : constant Bytes := (27, 91, 57, 52, 109);
      Color_13           : constant Bytes := (27, 91, 57, 53, 109);
      Color_14           : constant Bytes := (27, 91, 57, 54, 109);
      Color_15           : constant Bytes := (27, 91, 57, 55, 109);
      Color_16           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 54, 109);
      Color_17           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 55, 109);
      Color_18           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 56, 109);
      Color_19           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 57, 109);
      Color_20           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 48, 109);
      Color_21           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 49, 109);
      Color_22           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 50, 109);
      Color_23           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 51, 109);
      Color_24           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 52, 109);
      Color_25           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 53, 109);
      Color_26           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 54, 109);
      Color_27           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 55, 109);
      Color_28           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 56, 109);
      Color_29           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 57, 109);
      Color_30           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 51, 48, 109);
      Color_31           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 51, 49, 109);
      Color_32           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 51, 50, 109);
      Color_33           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 51, 51, 109);
      Color_34           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 51, 52, 109);
      Color_35           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 51, 53, 109);
      Color_36           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 51, 54, 109);
      Color_37           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 51, 55, 109);
      Color_38           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 51, 56, 109);
      Color_39           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 51, 57, 109);
      Color_40           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 52, 48, 109);
      Color_41           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 52, 49, 109);
      Color_42           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 52, 50, 109);
      Color_43           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 52, 51, 109);
      Color_44           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 52, 52, 109);
      Color_45           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 52, 53, 109);
      Color_46           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 52, 54, 109);
      Color_47           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 52, 55, 109);
      Color_48           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 52, 56, 109);
      Color_49           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 52, 57, 109);
      Color_50           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 53, 48, 109);
      Color_51           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 53, 49, 109);
      Color_52           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 53, 50, 109);
      Color_53           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 53, 51, 109);
      Color_54           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 53, 52, 109);
      Color_55           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 53, 53, 109);
      Color_56           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 53, 54, 109);
      Color_57           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 53, 55, 109);
      Color_58           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 53, 56, 109);
      Color_59           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 53, 57, 109);
      Color_60           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 54, 48, 109);
      Color_61           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 54, 49, 109);
      Color_62           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 54, 50, 109);
      Color_63           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 54, 51, 109);
      Color_64           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 54, 52, 109);
      Color_65           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 54, 53, 109);
      Color_66           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 54, 54, 109);
      Color_67           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 54, 55, 109);
      Color_68           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 54, 56, 109);
      Color_69           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 54, 57, 109);
      Color_70           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 55, 48, 109);
      Color_71           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 55, 49, 109);
      Color_72           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 55, 50, 109);
      Color_73           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 55, 51, 109);
      Color_74           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 55, 52, 109);
      Color_75           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 55, 53, 109);
      Color_76           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 55, 54, 109);
      Color_77           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 55, 55, 109);
      Color_78           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 55, 56, 109);
      Color_79           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 55, 57, 109);
      Color_80           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 56, 48, 109);
      Color_81           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 56, 49, 109);
      Color_82           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 56, 50, 109);
      Color_83           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 56, 51, 109);
      Color_84           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 56, 52, 109);
      Color_85           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 56, 53, 109);
      Color_86           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 56, 54, 109);
      Color_87           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 56, 55, 109);
      Color_88           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 56, 56, 109);
      Color_89           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 56, 57, 109);
      Color_90           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 57, 48, 109);
      Color_91           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 57, 49, 109);
      Color_92           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 57, 50, 109);
      Color_93           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 57, 51, 109);
      Color_94           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 57, 52, 109);
      Color_95           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 57, 53, 109);
      Color_96           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 57, 54, 109);
      Color_97           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 57, 55, 109);
      Color_98           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 57, 56, 109);
      Color_99           : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 57, 57, 109);
      Color_100          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 48, 48, 109);
      Color_101          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 48, 49, 109);
      Color_102          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 48, 50, 109);
      Color_103          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 48, 51, 109);
      Color_104          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 48, 52, 109);
      Color_105          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 48, 53, 109);
      Color_106          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 48, 54, 109);
      Color_107          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 48, 55, 109);
      Color_108          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 48, 56, 109);
      Color_109          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 48, 57, 109);
      Color_110          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 49, 48, 109);
      Color_111          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 49, 49, 109);
      Color_112          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 49, 50, 109);
      Color_113          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 49, 51, 109);
      Color_114          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 49, 52, 109);
      Color_115          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 49, 53, 109);
      Color_116          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 49, 54, 109);
      Color_117          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 49, 55, 109);
      Color_118          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 49, 56, 109);
      Color_119          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 49, 57, 109);
      Color_120          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 50, 48, 109);
      Color_121          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 50, 49, 109);
      Color_122          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 50, 50, 109);
      Color_123          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 50, 51, 109);
      Color_124          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 50, 52, 109);
      Color_125          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 50, 53, 109);
      Color_126          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 50, 54, 109);
      Color_127          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 50, 55, 109);
      Color_128          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 50, 56, 109);
      Color_129          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 50, 57, 109);
      Color_130          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 51, 48, 109);
      Color_131          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 51, 49, 109);
      Color_132          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 51, 50, 109);
      Color_133          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 51, 51, 109);
      Color_134          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 51, 52, 109);
      Color_135          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 51, 53, 109);
      Color_136          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 51, 54, 109);
      Color_137          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 51, 55, 109);
      Color_138          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 51, 56, 109);
      Color_139          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 51, 57, 109);
      Color_140          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 52, 48, 109);
      Color_141          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 52, 49, 109);
      Color_142          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 52, 50, 109);
      Color_143          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 52, 51, 109);
      Color_144          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 52, 52, 109);
      Color_145          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 52, 53, 109);
      Color_146          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 52, 54, 109);
      Color_147          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 52, 55, 109);
      Color_148          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 52, 56, 109);
      Color_149          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 52, 57, 109);
      Color_150          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 53, 48, 109);
      Color_151          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 53, 49, 109);
      Color_152          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 53, 50, 109);
      Color_153          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 53, 51, 109);
      Color_154          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 53, 52, 109);
      Color_155          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 53, 53, 109);
      Color_156          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 53, 54, 109);
      Color_157          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 53, 55, 109);
      Color_158          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 53, 56, 109);
      Color_159          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 53, 57, 109);
      Color_160          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 54, 48, 109);
      Color_161          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 54, 49, 109);
      Color_162          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 54, 50, 109);
      Color_163          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 54, 51, 109);
      Color_164          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 54, 52, 109);
      Color_165          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 54, 53, 109);
      Color_166          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 54, 54, 109);
      Color_167          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 54, 55, 109);
      Color_168          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 54, 56, 109);
      Color_169          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 54, 57, 109);
      Color_170          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 55, 48, 109);
      Color_171          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 55, 49, 109);
      Color_172          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 55, 50, 109);
      Color_173          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 55, 51, 109);
      Color_174          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 55, 52, 109);
      Color_175          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 55, 53, 109);
      Color_176          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 55, 54, 109);
      Color_177          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 55, 55, 109);
      Color_178          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 55, 56, 109);
      Color_179          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 55, 57, 109);
      Color_180          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 56, 48, 109);
      Color_181          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 56, 49, 109);
      Color_182          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 56, 50, 109);
      Color_183          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 56, 51, 109);
      Color_184          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 56, 52, 109);
      Color_185          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 56, 53, 109);
      Color_186          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 56, 54, 109);
      Color_187          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 56, 55, 109);
      Color_188          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 56, 56, 109);
      Color_189          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 56, 57, 109);
      Color_190          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 57, 48, 109);
      Color_191          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 57, 49, 109);
      Color_192          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 57, 50, 109);
      Color_193          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 57, 51, 109);
      Color_194          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 57, 52, 109);
      Color_195          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 57, 53, 109);
      Color_196          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 57, 54, 109);
      Color_197          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 57, 55, 109);
      Color_198          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 57, 56, 109);
      Color_199          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 49, 57, 57, 109);
      Color_200          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 48, 48, 109);
      Color_201          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 48, 49, 109);
      Color_202          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 48, 50, 109);
      Color_203          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 48, 51, 109);
      Color_204          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 48, 52, 109);
      Color_205          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 48, 53, 109);
      Color_206          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 48, 54, 109);
      Color_207          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 48, 55, 109);
      Color_208          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 48, 56, 109);
      Color_209          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 48, 57, 109);
      Color_210          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 49, 48, 109);
      Color_211          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 49, 49, 109);
      Color_212          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 49, 50, 109);
      Color_213          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 49, 51, 109);
      Color_214          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 49, 52, 109);
      Color_215          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 49, 53, 109);
      Color_216          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 49, 54, 109);
      Color_217          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 49, 55, 109);
      Color_218          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 49, 56, 109);
      Color_219          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 49, 57, 109);
      Color_220          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 50, 48, 109);
      Color_221          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 50, 49, 109);
      Color_222          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 50, 50, 109);
      Color_223          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 50, 51, 109);
      Color_224          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 50, 52, 109);
      Color_225          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 50, 53, 109);
      Color_226          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 50, 54, 109);
      Color_227          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 50, 55, 109);
      Color_228          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 50, 56, 109);
      Color_229          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 50, 57, 109);
      Color_230          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 51, 48, 109);
      Color_231          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 51, 49, 109);
      Color_232          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 51, 50, 109);
      Color_233          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 51, 51, 109);
      Color_234          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 51, 52, 109);
      Color_235          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 51, 53, 109);
      Color_236          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 51, 54, 109);
      Color_237          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 51, 55, 109);
      Color_238          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 51, 56, 109);
      Color_239          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 51, 57, 109);
      Color_240          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 52, 48, 109);
      Color_241          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 52, 49, 109);
      Color_242          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 52, 50, 109);
      Color_243          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 52, 51, 109);
      Color_244          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 52, 52, 109);
      Color_245          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 52, 53, 109);
      Color_246          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 52, 54, 109);
      Color_247          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 52, 55, 109);
      Color_248          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 52, 56, 109);
      Color_249          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 52, 57, 109);
      Color_250          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 53, 48, 109);
      Color_251          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 53, 49, 109);
      Color_252          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 53, 50, 109);
      Color_253          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 53, 51, 109);
      Color_254          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 53, 52, 109);
      Color_255          : constant Bytes := (27, 91, 51, 56, 59, 53, 59, 50, 53, 53, 109);
   end Codes;

   -- Character and Byte conversions.
   function To_Byte (Input : Character) return Byte;
   function To_Character (Data : Byte) return Character;

   -- Strings to Bytes.
   function To_Bytes (Input : String) return Bytes;
   function To_Bytes (Input : Character) return Bytes;

   -- Bytes to Strings.
   function To_String (Data : Bytes) return String;
   function To_String (Datum: Byte) return String;

   -- Join Bytes.
   function "&" (Left : Bytes; Right : Bytes) return Bytes;
   function "&" (Left : Byte; Right : Bytes) return Bytes;
   function "&" (Left : Bytes; Right : Byte) return Bytes;

   -- Join Strings and Bytes.
   function "&" (Left : Bytes; Right : String) return Bytes;
   function "&" (Left : String; Right : Bytes) return Bytes;

   -- Join Strings and Bytes, returning String.
   function "&" (Left : Bytes; Right : Bytes) return String;
   function "&" (Left : Bytes; Right : Byte) return String;
   function "&" (Left : Byte; Right : Bytes) return String;
   function "&" (Left : Bytes; Right : String) return String;
   function "&" (Left : String; Right : Bytes) return String;

   -- Join Values.
   function "&" (Left : Values; Right : Values) return Values;
   function "&" (Left : Values; Right : Natural) return Values;
   function "&" (Left : Natural; Right : Values) return Values;
   function "&" (Left : Natural; Right : Natural) return Values;

private

   -- Terminal input / output defaults for all action procedures.
   Saved_Terminal_Input : File_Type := Standard_Input;
   Saved_Terminal_Output  : File_Type := Standard_Output;
   Saved_Is_Terminal_Output : Boolean := False;
   Saved_Is_Terminal_Input : Boolean := False;

   function To_String (Number : Integer)return String;
   function To_String (Numbers : Values) return String;

end;
