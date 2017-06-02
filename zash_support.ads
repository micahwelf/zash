with Ada;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
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
with Terminal;
use GNAT;

--
package Zash_Support is

   package Calendar renames Ada.Calendar;
   package Containers renames Ada.Containers;
   package IO renames Ada.Text_IO;
   package OS renames GNAT.OS_Lib;
   package OS_Lib renames GNAT.OS_Lib;
   package IOs renames Ada.Text_IO.Text_Streams;




   use Ada.Calendar;
   use Ada.Command_Line;
   use Ada.Directories;
   use Ada.Containers;
   use Ada.Text_IO;
   use Ada.Text_IO.Text_Streams;


   Parse_Quotation_Mismatch : exception;
   Parse_Bracket_Mismatch : exception;
   Parse_Begin_End_Mismatch : exception;
   Parse_Early_End_Of_File  : exception;
   Command_Not_Found         : exception;


   Option_Interactive : Boolean := False;
   Option_Color : Boolean := True;
   Option_Fast : Boolean := False;


   -- Print general help message, or on the topic given.
   procedure Put_Help (Topic : String := "");


   -- A Statement is composed like an ARGV for any command or function.
   package Statement is new Containers.Indefinite_Vectors (Natural, String);
   function "=" (Left  : Statement.Vector;
                 Right : Statement.Vector)
                 return Boolean renames Statement."=";


   -- Words are full complete-character-value strings that can be used as
   -- command arguments or array components, etc.
   package Words is new Containers.Indefinite_Vectors (Positive, String);
   function "=" (Left  : Words.Vector;
                 Right : Words.Vector)
                 return Boolean renames Words."=";


   package List is new Containers.Vectors (Positive, Statement.Vector);
   function "=" (Left  : List.Vector;
                 Right : List.Vector)
                 return Boolean renames List."=";


   --  A String with expansion tags is expanded (See notes under Put_Prompt).
   function Expand_Prompt (Prompt : String)
                           return String;


   -- Expand a variable with Zsh features.
   function Expand_Variable (Name  : String;
                             Subscript : String := "";
                             Flags     : String := "";
                             Modifier  : String := "")
                             return String;


   --  Expand a reverse solidus JSON/SH/C escaped character.
   --  Defaults to an ordinary Reverse Solidus.
   function Expand_R_Solidus (Full_Sequence : String := "\\") return Character;


   procedure Put_Prompt
     (Prompt : String := "%f{magenta}%Y-%m-%d - %H:%M:%S");
   --
   --  Prompt string is the optional argument given to Expand_Prompt, which
   --  return value preceeds "=> " when prompt is given
   --
   --  GNU GNAT documentation explains Time_IO format, my own is prepended
   --  to explain my prompt-specific expansion that is added to Time_IO.
   --  brackets {} may be parenthesis () in actual use. The contents of
   --  the brackets are either the name of the color or variable.
   --
   --          %C   Current Directory
   --          %E{} Environment Variable
   --          %F{}
   --          %f{} foreground color
   --          %K{} background color
   --          %u   user
   --          %g   group
   --          %L   local host name -- GNAT.Sockets.Host_Name;
   --
   --  This is a string to describe date and time output format. The string is
   --  a set of standard character and special tag that are replaced by the
   --  corresponding values. It follows the GNU Date specification. Here are
   --  the recognized directives :
   --
   --          %    a literal %
   --          n    a newline
   --          t    a horizontal tab
   --
   --          Time fields:
   --
   --          %H   hour (00..23)
   --          %I   hour (01..12)
   --          %k   hour ( 0..23)
   --          %l   hour ( 1..12)
   --          %M   minute (00..59)
   --          %p   locale's AM or PM
   --          %r   time, 12-hour (hh:mm:ss [AP]M)
   --          %s   seconds  since 1970-01-01  00:00:00 UTC
   --                (a nonstandard extension)
   --          %S   second (00..59)
   --          %T   time, 24-hour (hh:mm:ss)
   --
   --          Date fields:
   --
   --          %a   locale's abbreviated weekday name (Sun..Sat)
   --          %A   locale's    full   weekday   name,    variable   length
   --                  (Sunday..Saturday)
   --  remove  %b   locale's abbreviated month name (Jan..Dec)
   --          %B   locale's    full    month    name,   variable    length
   --                  (January..December)
   --          %c   locale's date and time (Sat Nov 04 12:02:33 EST 1989)
   --          %d   day of month (01..31)
   --          %D   date (mm/dd/yy)
   --          %h   locale's abbreviated month name (Jan..Dec)
   --          %j   day of year (001..366)
   --          %m   month (01..12)
   --          %U   week number  of year with  Sunday as first day  of week
   --                  (00..53)
   --          %w   day of week (0..6) with 0 corresponding to Sunday
   --          %W   week number  of year with  Monday as first day  of week
   --                  (00..53)
   --          %x   locale's date representation (mm/dd/yy)
   --          %y   last two digits of year (00..99)
   --          %Y   year (1970...)
   --
   --          By default,  date pads numeric fields with zeroes.  GNU date
   --          recognizes the following nonstandard numeric modifiers:
   --
   --          -    (hyphen) do not pad the field
   --          _    (underscore) pad the field with spaces
   --
   --  Here are some GNAT extensions to the GNU Date specification:
   --
   --          %i   milliseconds (3 digits)
   --          %e   microseconds (6 digits)
   --          %o   nanoseconds  (9 digits)


   --  Move to inside Expand_Prompt for %s expansion.
   function Unix_Time return Time;

   --  Test
   function Test_Is_Terminal (Descriptor : OS_Lib.File_Descriptor := OS_Lib.Standin) return Boolean;

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


   type Control_Kind is (Control_With_If,
                         Control_With_Loop,
                         Control_With_Case,
                         Control_With_TryAlways,
                         Control_With_For,
                         Control_With_Foreach,
                         Control_With_Select);

   type Control_Statement (Of_Kind : Control_Kind) is record
      Kind : Control_Kind := Of_Kind;
      Condition : Words.Vector;
      Action    : List.Vector;
   end record;

   procedure Execute_Statement (Argv : Statement.Vector);

   -- Note to Self:
   --  Use Free (String_List'Access) instead of Free(String_List)

   -- When parsing any string, this mode is how it will be interpreted.
   type Parse_Mode is (-- a reasonable substitute for NULL
                       Parse_Undefined,

                       -- The default parsing mode -- white space separates arguments.
                       Parse_Plain,

                       -- Ada style quotes - two double-quote in the string creates a double quote instead of two strings.
                       Parse_Quote,

                       -- JSON/SH-like expansion.
                       Parse_R_Solidus,

                       -- Shell-like $ expansion with a few differing/added functionalities.
                       Parse_Expn,

                       -- Zash-style globbing is slightly different * starts a glob phrase like an operater or function.
                       Parse_Glob,

                       -- JSON data buffer.
                       Parse_Json)
     with Default_Value => Parse_Plain;

   --  Context
   type Context_Type is (-- a reasonable substitute for NULL
                         Context_Toplevel,

                         -- "If" test statement. removed with single statement or list/loop.
                         Context_If,

                         -- Special "[[ ... ]]" test.
                         Context_Condition,

                         -- "Then" list
                         Context_Then,

                         -- Curly bracket list:  { <list> }
                         Context_List,

                         -- Ada style loop:  loop  <statements>;  end [loop];
                         Context_Loop,

                         -- Zash-style globbing is slightly different * starts a glob phrase like an operater or function.
                         Context_Do,

                         -- JSON data buffer.
                         Context_Control_Subshell)
     with Default_Value => Context_Toplevel;

   --  Parse_Statement is the low-level master of the language. It takes
   -- raw input from script or terminal and converts it into a Statement
   -- that Zash understands internally.
   --
   --  Also, it is here that raw string and escape handling is done.
   -- A statement is parsed word by word, and when a word is finalized,
   -- expansions should either complete, or be completed. Functionality
   -- of expansions is thus limited, but very fast.
   function Parse_Statement (S : String)
                             return Statement.Vector;









end;
