pragma Ada_2012;
with GNAT.Regpat;
private with Ada.Strings.Unbounded;
private with Ada.Strings.Unbounded.Text_IO;
private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Indefinite_Ordered_Maps;

package Regex is

   subtype Expression is String;

   --type String_Access is access all String;

   type Constant_Reference_String
     (Source : not null access constant String) is private
     with Implicit_Dereference => Source;

   subtype Regex_Flags is GNAT.Regpat.Regexp_Flags;

   subtype Match_Location is GNAT.Regpat.Match_Location;

   type Reference_Location
     (Location : not null access Match_Location) is private
     with Implicit_Dereference => Location;

   type Constant_Reference_Location
     (Location : not null access constant Match_Location) is private
     with Implicit_Dereference => Location;

   -- By Default, programs are limited to 4 kibibytes, but that can
   -- automatically be increased, once I figure out how to detect
   -- that it is insufficient. This is not very memory efficient,
   -- but it means that compiling and and running Regular Expressions
   -- is faster.
   --
   -- The Agent is a the regular expression program, the holder of
   -- the results, and optionally can be linked via an access to
   -- a string variable. This allows very long strings to be
   -- searched more efficiently.
   type Agent is tagged private
     with
       Constant_Indexing => Get_Constant_Reference_String,
       Variable_Indexing => Get_Reference_Location;

   -- Flags change the regular expression program at Compile time to:
   -- (1) be case insensitive,
   -- (2) treat multiple lines (separated by Newlines) as one or as special,
   -- (3) search all possible non-overlapping matches in one go,
   -- (4) search all possible, possibly-overlapping matches in one go, and
   -- (5) cache the matching strings so no reference to the original string is necessary.
   No_Flags         : constant Regex_Flags;
   Case_Insensitive : constant Regex_Flags;
   Single_Line      : constant Regex_Flags;
   Multiple_Lines   : constant Regex_Flags;
   Global           : constant Regex_Flags;
   Cache            : constant Regex_Flags;
   Overlap          : constant Regex_Flags;

   procedure Compile (Agent      : in out Regex.Agent;
                      Expression : in Regex.Expression;
                      Flags      : Regex_Flags := No_Flags;
                      Source     : access String := null);

   function Sub_Count (Agent : in Regex.Agent) return Natural;

   procedure Match (Agent : in out Regex.Agent);

   function Match (Agent : in out Regex.Agent) return Boolean;

   procedure Match (Agent  : in out Regex.Agent;
                    Source : in String);

   function Match (Agent  : in out Regex.Agent;
                   Source : in String) return Boolean;

   function Get (Agent    : in out Regex.Agent;
                 SubIndex : Natural := 0) return String;

   function Get (Agent    : in out Regex.Agent;
                 Index    : Natural;
                 SubIndex : Natural) return String;

   function Count (Agent : in Regex.Agent) return Natural;

   procedure Clear (Agent : in out Regex.Agent);

   function Quote (Source : in String) return String;

   procedure Dump (Agent : in Regex.Agent);

   function Get_Reference_Location (Agent    : in out Regex.Agent;
                                    SubIndex : Natural) return Reference_Location;

   function Get_Constant_Reference_String (Agent    : in out Regex.Agent;
                                           SubIndex : Natural) return Constant_Reference_String;

   function Get_Constant_Reference_String (Agent    : in out Regex.Agent;
                                           Source   : in String;
                                           SubIndex : Natural := 0) return Constant_Reference_String;

   function Get_Constant_Reference_String (Agent    : in out Regex.Agent;
                                           Index    : Natural;
                                           SubIndex : Natural) return Constant_Reference_String;

   function Get_Constant_Reference_String (Agent    : in out Regex.Agent;
                                           Source   : in String;
                                           Index    : Natural;
                                           SubIndex : Natural) return Constant_Reference_String;

private
   use Gnat.Regpat;
   subtype Matcher is GNAT.Regpat.Pattern_Matcher (4096);

   type Constant_Reference_String
     (Source : not null access constant String) is record null; end record;
   type Reference_Location
     (Location : not null access Match_Location) is record null; end record;
   type Constant_Reference_Location
     (Location : not null access constant Match_Location) is record null; end record;

   No_Flags         : constant Regex_Flags := 0;
   Case_Insensitive : constant Regex_Flags := 1;
   Single_Line      : constant Regex_Flags := 2;
   Multiple_Lines   : constant Regex_Flags := 4;
   Global           : constant Regex_Flags := 8;
   Cache            : constant Regex_Flags := 16;
   Overlap          : constant Regex_Flags := 32;

   Empty_String : String := "";
   type Source_Access is access all String;
   type Cache_Index is record
      Index    : Positive;
      Location : Match_Location;
   end record;
   function "=" (Left, Right : Cache_Index) return Boolean;
   function "<" (Left, Right : Cache_Index) return Boolean;
   function ">" (Left, Right : Cache_Index) return Boolean;
   package Location_V is new Ada.Containers.Indefinite_Vectors (Positive,
                                                                Match_Array);
   package String_M is new Ada.Containers.Indefinite_Ordered_Maps (Cache_Index,
                                                                   String);

   type Agent is tagged record
      Matcher     : Regex.Matcher; -- The compiled regular expression program.
      Size        : GNAT.Regpat.Program_Size; -- The program size for fast running.
      Sub_Count   : Natural := 0; -- Count all sub-expressions (parentheses).
      Global      : Boolean := False; -- Flag to search for all possible matches in one go.
      Overlap     : Boolean := False; -- Flag to overlap beginning and ending of consecutive searches.
      Cache       : Boolean := False; -- Flag for whether to cache results.
      Complete    : Boolean := False; -- Flag for whether search reached end of source.
      Matches     : Location_V.Vector := Location_V.Empty_Vector;
      Cache_Store : String_M.Map := String_M.Empty_Map; -- Cache of results.
      Source      : Source_Access := null; -- Link to _large_ source string.
      Cursor      : Positive := 1; -- Index for getting one result at a time.
   end record;

end Regex;
