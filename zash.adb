with Text_IO;
with Text_IO.Text_Streams;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces.C_Streams;
with Interfaces.C;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Directories;
with GNAT.OS_Lib;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Command_Line.Environment;
with Ada.Unchecked_Deallocation;
with Ada.Calendar;
with Ada.Calendar.Time_Zones;  use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting;
with GNAT.Calendar.Time_IO;
with GNAT.Formatted_String;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with GNAT.Regpat;
with Ada; use Ada;
with GNAT; use GNAT;

-- Quick Note:  Test with %HOMEDRIVE%%HOMEPATH% vs $HOME to determine OS

procedure Zash is
   use Ada;
   use Ada.Calendar;
   use Ada.Command_Line;
   use Ada.Directories;
   use Ada.Containers;
   use Text_IO;
   use Ada.Text_IO.Text_Streams;

   Option_Interactive : Boolean := False;

   procedure Put_Help is
      function "+" (Left : String; Right : String) return String is
      begin
         return Left & ASCII.LF & Right;
      end;
   begin
      Put_Line (Current_Error,
                " Version 0.1 of Zash -- Rudimentary command statements"
                + " only capable of launching executables with arguments."
                + " Arguments may be quoted with Ada-style quotes, "
                + " meaning that double quotes may simply be doubled in a"
                + " String to be included: ""ABC""""D"" => ABC""D "
                + " Shell style concatenating of quoted strings is used;"
                + " so just combine as one string: ""ABC""\n\n""DEF"" =>"
                + "                                ABC"
                + "                                "
                + "                                DEF"
                + "Also, as you can see anything not quoted is treated as"
                + "a GNU formatted string which is like C printf."
               );
   end Put_Help;

   procedure Put_Prompt (Date_Format    : String := "%Y-%m-%d ";
                         Show_Directory : Boolean := True)
   is
      use GNAT.Calendar.Time_IO;
      use Ada.Calendar.Time_Zones;
   begin
      if Option_Interactive
      then
         Put (Image
              (Date => Clock, Picture => "%Y-%m-%d")
              & (if Show_Directory
                then " " & Directories.Current_Directory
                else "")
              & " => ");
      end if;
   end;

   function Unix_Time return Time
   is
      use Ada.Calendar.Formatting;

      U_Year     : Year_Number;
      U_Month    : Month_Number;
      U_Day      : Day_Number;
      U_Hour     : Hour_Number;
      U_Minute   : Minute_Number;
      U_Second   : Second_Number;
      U_Duration : Second_Duration;
   begin
      Split
        (Date       => Clock,
         Year       => U_Year,
         Month      => U_Month,
         Day        => U_Day,
         Hour       => U_Hour,
         Minute     => U_Minute,
         Second     => U_Second,
         Sub_Second => U_Duration,
         Time_Zone  => 0);

      return Time_Of
        (Year       => U_Year,
         Month      => U_Month,
         Day        => U_Day,
         Hour       => U_Hour,
         Minute     => U_Minute,
         Second     => U_Second,
         Sub_Second => U_Duration,
         Time_Zone  => UTC_Time_Offset);
   end Unix_Time;

   function Is_Interactive_Terminal return Boolean
   is
      use Ada.Text_IO;
      use Interfaces.C_Streams;
   begin
      return  1 = Isatty (Fileno (Stdin)) and 1 = Isatty (Fileno (Stdout));
   end;

   package Env renames Ada.Environment_Variables;
   -- Ada.Environment_Variables
   --
   --  Value ( Name,  [Default_Value] )
   --  Exists ( Name )
   --  Set ( Name, Value )
   --  Clear ( [Name] )   -- without name, clears all.
   --  Iterate ( access procedure ( Name, Value ) )

   package CL renames Ada.Command_Line;
   -- Ada.Command_Line
   --
   --  Argument_Count
   --  Argument ( Number )
   --  Command_Name

   Env_Display : String := Env.Value ("DISPLAY", ":0");
   Env_Wayland_Display : String := Env.Value ("WAYLAND_DISPLAY", "wayland-0");

   package Action_Full is new Containers.Indefinite_Vectors (Natural, String);
   --     type Statement is new Action_Full.Vector with record
   --        null;
   --     end record;

   package Action_Arguments is new Containers.Indefinite_Vectors (Positive, String);
   --     type Arguments is new  Action_Arguments.Vector with record
   --        null;
   --     end record;

   package Action_List is new Containers.Vectors (Positive, Action_Full.Vector);
   --     type Statement_List is new Action_List.Vector with record
   --        null;
   --     end record;


   type Control_Kind is (Control_With_If,
                         Control_With_Loop,
                         Control_With_Case,
                         Control_With_TryAlways,
                         Control_With_For,
                         Control_With_Foreach,
                         Control_With_Select);
   type Control_Statement (Kind : Control_Kind) is record
      Condition : Action_Arguments;
      Action    : Action_List;
   end record;

   procedure Execute_Statement (Argv : Action_Full.Vector)
   is
      use GNAT.OS_Lib;
      Action_Result : Boolean := False;
   begin
      if Argv.Is_Empty
      then
         null;
      else
         declare
            Command          : String := Argv (0);
            Resolved_Command : String_Access :=
              Locate_Exec_On_Path (Command);
            Arguments        : Argument_List (1 .. Integer (Argv.Last_Index));
         begin

            for N in Arguments'Range
            loop
               Arguments (N) := new String'(Argv (N));
            end loop;

            if False -- Replace this test with builts-in-contain test.
            then
               null;
            elsif Resolved_Command /= null
            then
               Spawn (Program_Name => Resolved_Command.all, Args => Arguments, Success => Action_Result);
            else
               Put_Line (Current_Error, "Command Not Found: " & Argv (0));
            end if;
            -- Free (Resolved_Command);


            if Action_Result
            then
               Set_Exit_Status (Success);
            else
               Set_Exit_Status (Failure);
            end if;
            --Give return status back to calling os/environment.

         end;
      end if;
   end;

   --   procedure Free is new Ada.Unchecked_Deallocation (Object => String,
   --                                                     Name   => GNAT.OS_Lib.String_Access);

   String_Quotation_Mismatch : exception;
   Command_Not_Found         : exception;

   type Parse_Mode is (Parse_Undefined, -- a reasonable substitute for NULL
                       Parse_Plain, -- The default parsing mode -- white space separates arguments.
                       Parse_Quote, -- Ada style quotes - two double-quote in the string creates a double quote instead of two strings.
                       Parse_R_Solidus, -- JSON/SH-like expansion.
                       Parse_Expn, -- Shell-like $ expansion with a few differing/added functionalities.
                       Parse_Glob) -- Zash-style globbing is slightly different * starts a glob phrase like an operater or function.
     with Default_Value => Parse_Plain;

   --  Parse_Statement is the low-level master of the language. It takes
   -- raw input from script or terminal and converts it into a Statement
   -- that Zash understands internally.
   --
   --  Also, it is here that raw string and escape handling is done.
   -- A statement is parsed word by word, and when a word is finalized,
   -- expansions should either complete, or be completed. Functionality
   -- of expansions is thus limited, but very fast.
   function Parse_Statement (S : String)
                                return Action_Full.Vector
   is
      use Ada.Characters.Latin_1;

      In_Quote : Boolean := False;
      In_Rsol  : Boolean := False;
      In_Expn  : Boolean := False;

      C        : Natural := S'First;
      -- Cursor on input String

      L        : Natural := 0;
      -- Length and Cursor of Temporary Buffer

      T        : String (S'Range) := (others => ' ');
      -- Temporary Buffer for builting a statement word.

      R        : Action_Full.Vector;
      -- Return Action Statement (0 => Command, 1 .. inf => Arguments)

      E        : Action_Full.Vector;
      -- Empty Return Statement, never assigned to;

      package Mode_Context is new Containers.Vectors (Positive, Parse_Mode);
      Context  : Mode_Context.Vector;

      -- Give the most recent parsing mode or the default original mode.
      function Current_Mode return Parse_Mode is
      begin
         if Context.Is_Empty
         then
            return Parse_Plain;
         else
            return Context.Last_Element;
         end if;
      end;

      -- Switch to a new parsing mode, preserving a history of previous
      -- modes.
      procedure Enter_Mode (New_Mode : in Parse_Mode) is
      begin
         Context.Append (New_Mode);
      end;

      -- Go to previous parsing mode or do nothing if in original mode.
      procedure Exit_Mode (Test_Mode : Parse_Mode := Parse_Undefined) is
      begin
         if Context.Is_Empty
         then
            null;
         else
            case Test_Mode is
               when Parse_Undefined =>
                  Context.Delete_Last;
               when others =>
                  if Test_Mode = Current_Mode
                  then
                     Context.Delete_Last;
                  end if;
            end case;
         end if;
      end;

      -- Add the character to the end of the current argument or
      -- statement word being parsed.
      procedure Append (Ch : Character) is
      begin
         L := L + 1;
         T (L) := Ch;
      end;

      procedure Next (Num : Integer := 1) is
      begin
         C := C + Num;
      end;

      procedure Finish_Word is
      begin
         R.Append (T (1 .. L));
         L := 0;
         Next;
      end;

   begin
      while C <= S'Last
      loop
         case Current_Mode is
            when Parse_Plain =>
               case S (C) is
                  when '"' =>
                     Enter_Mode (Parse_Quote);
                     Next;
                  when ''' =>
                     Append (S (C + 1));
                     Next (3);
                  when '\' =>
                     Enter_Mode (Parse_R_Solidus);
                     Next;
                  when '$' =>
                     Enter_Mode (Parse_Expn);
                     Next;
                  when NUL | LF | HT | Space | CR =>
                     if L > 0
                     then
                        Finish_Word;
                     else
                        Next;
                     end if;
                  when others =>
                     Append (S (C));
                     Next;
               end case;

            when Parse_Quote =>
               case S (C) is
                  when '"' =>
                     Exit_Mode (Parse_Quote);
                     Next;
                     if C = S'Last
                     then
                        null;
                     elsif S (C + 1) = '"'
                     then
                        Append ('"');
                        Next (2);
                     end if;
                  when others =>
                     Append (S (C));
                     Next;
               end case;
            when Parse_R_Solidus =>
               case S (C) is
                  when 'n' => Append (LF); Next;
                  when '0' => Append (NUL); Next;
                  when 'r' => Append (CR); Next;
                  when 't' => Append (HT); Next;
                  when others => Append (S (C)); Next;
               end case;
               Exit_Mode (Parse_R_Solidus);
            when Parse_Expn =>
               Exit_Mode; -- not supported yet;
            when others =>
               Exit_Mode; -- not supported yet;
         end case;
      end loop;

      case Current_Mode is
         when Parse_Plain =>
            Finish_Word;
            return R;
         when others =>
            Put_Line (Current_Error, "Syntax Error: String/Expansion not closed as expected");
            raise Program_Error;
            return E;
      end case;

   end Parse_Statement;


begin

   if Is_Interactive_Terminal
   then
      Put_Line ("Hello, Welcome to Zash, a Z-inspired Ada-founded scripting shell");
      Put_Line ("Type 'help' for a brief message to get started.");
      Option_Interactive := True;

   end if;

   loop
      Put_Prompt;

      declare
         use GNAT.OS_Lib;

         Current_Statement : Action_Full.Vector := Parse_Statement (Get_Line);

         Command_Status    : Boolean := True;

      begin

         Execute_Statement (Current_Statement);

      end;

   end loop;


   --
   --     Launch_Arguments (1) := new String'("--profile-directory=" & Profile);
   --     Launch_Arguments (2) := new String'("--new-window");
   --
   --     Launch_Arguments (3) := new String'("--app-id=" & App_ID);
   --
   --     for N in 1 .. Argument_Count loop
   --        Launch_Arguments (N+2) := new String'(Argument (N));
   --     end loop;
   --     -- Simply copy/convey all arguments to the new command launch.
   --
   --     --Put_Line (Launch_Name);
   --     -- DEBUG ACTION - remove this statement when the performance is sufficient.
   --
   --
   --     Spawn (Launch_Name, Launch_Arguments, Launch_Status);
   --     -- Launch the new process with conveyed arguments and capture general
   --     -- return status as Success or Failure.  A number may be used in the future.
   --
   --     if not Launch_Status
   --     then
   --        Set_Exit_Status (Failure);
   --     else
   --        Set_Exit_Status (Success);
   --     end if;
   --     --Give return status back to calling os/environment.

end Zash;
