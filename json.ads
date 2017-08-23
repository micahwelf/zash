pragma Ada_2012;
with Ada.Containers;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.UTF_Encoding; use Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings; use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Characters.Wide_Wide_Latin_1; use Ada.Characters.Wide_Wide_Latin_1;
with Ada.Characters.Handling;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Ada.Streams;
with Ada.Streams.Stream_IO;


package JSON is

   subtype Json_String is UTF_8_String;
   subtype HTML_String is UTF_8_String;
   subtype CSS_String is UTF_8_String;
   package Latin renames Ada.Characters.Wide_Wide_Latin_1;

   package Error is
      Undefined                   : exception;
      Not_NUL                     : exception;
      Not_Boolean                 : exception;
      Not_Number                  : exception;
      Not_String                  : exception;
      Not_Vector                  : exception;
      Not_Object                  : exception;
      Not_HTML_Tag                : exception;
      Not_HTML_Attribute          : exception;
      Not_HTML_Content            : exception;
      Not_CSS_Selector            : exception;
      Not_CSS_Property            : exception;
      Not_CSS_Value               : exception;
      Not_Access                  : exception;
      Access_Not_Allowed          : exception;
      Syntax_Invalid_Literal      : exception;
      Syntax_Invalid_Value        : exception;
      Syntax_Invalid_Escape       : exception;
      Syntax_No_Object_Key        : exception;
      Syntax_No_Object_Value      : exception;
      Syntax_No_Element           : exception;
      Syntax_No_Element_Separator : exception;
      Syntax_No_Close_Object      : exception;
      Syntax_No_Close_Array       : exception;
      Syntax_No_Close_String      : exception;
      Syntax_Extra_Close_Object   : exception;
      Syntax_Extra_Close_Array    : exception;
      Syntax_Extra_Quotation      : exception;
   end Error;

   package Types is
      type Json_Type is (NUL, Boolean, Number, String, Vector, Object, Undefined);
      type HTML_Type is (Tag, Attribute, Content);
      type CSS_Type is (Selector, Property, Property_Value);
      function Get_Type (Str : Json_String) return Json_Type;
      function Get_Type (Str : HTML_String) return HTML_Type;
      function Get_Type (Str : CSS_String) return CSS_Type;

      subtype Path_Type is Json_Type range Number .. Object;
      use Ada.Strings.Wide_Wide_Unbounded;
      type Path_Element (Element_Type : Path_Type := Number) is record
         case Element_Type is
            when Number | Vector =>
               Number : Integer;
            when String | Object =>
               String : Unbounded_Wide_Wide_String;
         end case;
      end record;
      type Json_Path is array (Positive range <>) of Path_Element;

      -- Integers used with indexing
      function Image (Number : Integer) return Standard.String;
      function Image (Number : Integer) return Standard.Wide_Wide_String;
      function Value (Image : Standard.String) return Integer;
      function Value (Image : Standard.Wide_Wide_String) return Integer;

      -- Floats used with JSON Import/Export
      function Image (Number : Long_Long_Float) return Standard.String;
      function Image (Number : Long_Long_Float) return Standard.Wide_Wide_String;
      function Value (Image : Standard.String) return Long_Long_Float;
      function Value (Image : Standard.Wide_Wide_String) return Long_Long_Float;

      -- Strings used with JSON Import/Export
      function Image (Value : Wide_Wide_String) return Standard.String;
      function Image (Value : Wide_Wide_String) return Standard.Wide_Wide_String;
      function Value (Image : Standard.String) return Wide_Wide_String;
      function Value (Image : Standard.Wide_Wide_String) return Wide_Wide_String;

      function To_String
        (Item       : Standard.Wide_Wide_String;
         Substitute : Character := ' ') return Standard.String;
      function To_Wide_String
        (Item       : Standard.String) return Standard.Wide_Wide_String;

   private

      function Escape_Image (Value : in Wide_Wide_Character) return Wide_Wide_String;
      function Escape_Value (Image : in Wide_Wide_String) return Wide_Wide_Character;
   end Types;

   type Node is tagged private
     with
       Variable_Indexing => Get_Reference,
       Constant_Indexing => Get_Constant_Reference;

   type Reference (Actual : not null access Node) is limited private
     with
       Implicit_Dereference => Actual;
   type Constant_Reference (Actual : not null access constant Node) is limited private
     with
       Implicit_Dereference => Actual;

   -- These are modelled after JSON.Node to allow constructing HTML and CSS
   --type Element is private;
   --type Style is private;

   function Get_Type (Json : Node) return Types.Json_Type;

   function "=" (Left, Right : in Node) return Boolean;

   function "=" (Left : in Types.Json_Type; Right : in Node) return Boolean;
   function "=" (Left : in Node; Right : in Types.Json_Type) return Boolean;

   function "/" (Left : in Wide_Wide_String; Right : in Wide_Wide_String) return Types.Json_Path;
   function "/" (Left : in Wide_Wide_String; Right : in Integer) return Types.Json_Path;
   function "/" (Left : in Integer; Right : in Wide_Wide_String) return Types.Json_Path;
   function "/" (Left : in Integer; Right : in Integer) return Types.Json_Path;
   function "/" (Left : in Types.Json_Path; Right : in Integer) return Types.Json_Path;
   function "/" (Left : in Types.Json_Path; Right : in Wide_Wide_String) return Types.Json_Path;

   -- Import and Export are probably the biggest function/procedures because of
   -- the need to examine each of the JSON spec syntax elements...
   function Import (Source : in Json_String) return Node;
   function Export (Source : in Node) return Json_String;

   -- Convert (HTML_Type ) return Json_Type translate tags to JSON like this:
   -- <body attr="value" style="prop:value;">
   --  <span ...>Text of something.</span>
   -- </body>
   -- >>>>>>>>>>>>>>
   -- {
   --   tag: "body",
   --   attributes: { attr: "value", style: { "prop": "value" } },
   --   children: [
   --     {
   --       tag: "span",
   --       attributes: { ... },
   --       children: [
   --         tag: null,
   --         content: "Text of something."
   --       ]
   --     }   function Get_Reference (Object : in out Node; Key : Wide_Wide_String) return Reference;
   --   ]
   -- }


   procedure Set (Target : in out Node; Value : in Boolean);
   procedure SetS (Target : in out Node; Value : in String);
   procedure Set (Target : in out Node; Value : in Wide_Wide_String);
   procedure SetI (Target : in out Node; Value : in Integer);
   procedure Set (Target : in out Node; Value : in Long_Long_Integer'Base);
   procedure SetF (Target : in out Node; Value : in Float);
   procedure Set (Target : in out Node; Value : in Long_Long_Float'Base);

   function Get (Source : in out Node) return Boolean;
   function GetS (Source : in out Node) return String;
   function Get (Source : in out Node) return Wide_Wide_String;
   function GetI (Source : in out Node) return Integer;
   function Get (Source : in out Node) return Long_Long_Integer;
   function GetF (Source : in out Node) return Float;
   function Get (Source : in out Node) return Long_Long_Float;

   function Copy (Source : in Node) return Node;
   -- function Copy (Source : access Node) return Node;

   function Length (Json : in Node) return Natural;
   -- function Length (Json : access Node) return Natural;

   function Adapt (Input : in Json_String) return Wide_Wide_String;
   function Adapt (Input : in Wide_Wide_String) return Json_String;
   function Get_Reference (Object : in out Node; Key : Types.Json_Path) return Reference;
   function Get_Reference (Object : in out Node; Key : Wide_Wide_String) return Reference;
   function Get_Reference (Object : in out Node; Key : Positive) return Reference;
   function Get_Constant_Reference (Object : in out Node; Key : Types.Json_Path) return Constant_Reference;
   function Get_Constant_Reference (Object : in out Node; Key : Wide_Wide_String) return Constant_Reference;
   function Get_Constant_Reference (Object : in out Node; Key : Integer) return Constant_Reference;

   procedure Iterate (Node : in out JSON.Node; Process : not null access procedure (Each_Node : in out JSON.Node));

private

   type Reference (Actual : not null access Node) is limited record null; end record;
   type Constant_Reference (Actual : not null access constant Node) is limited record null; end record;

   procedure Node_Write (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                         Item   : in Node);

   procedure Node_Read (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                        Item   : out Node);

   function Node_Image (Item : in Node) return String;

   -- Recursively crawl through objects and arrays Free-ing any Node_Link
   -- connections to Node in the Storage Pool.
   procedure Destroy_Node_Tree (Target : in out Node);

   -- In order to create recursion (vectors or maps of Nodes within a Node)
   -- These structures are needed in order to treat the children Nodes like
   -- they were locally assigned variables rather than the access types
   -- that they are.
   type Node_Access is access all Node;
   package Node_Access_Backup is new Ada.Containers.Indefinite_Vectors (Natural, Node_Access);
   type Node_Link (Reference : Node_Access) is new Ada.Finalization.Controlled with record null;
      --History : Node_Access_Backup.Vector := Node_Access_Backup.Empty_Vector;
   end record;

   overriding procedure Initialize (Item : in out Node_Link);
   overriding procedure Adjust (Item : in out Node_Link);
   overriding procedure Finalize (Item : in out Node_Link);

   package Of_Array is new Ada.Containers.Indefinite_Vectors (Natural, Node_Link);
   package Of_Object is new Ada.Containers.Indefinite_Ordered_Maps (Wide_Wide_String, Node_Link);

   use Ada.Strings.Wide_Wide_Unbounded;

   type Node_Value (Of_Type : Types.Json_Type := Types.Undefined) is record
      case Of_Type is
         when Types.NUL =>
            null;
         when Types.Boolean =>
            Boolean   : aliased Standard.Boolean := False;
         when Types.Number =>
            Number    : aliased Long_Long_Float := 0.0;
         when Types.String =>
            String    : aliased Unbounded_Wide_Wide_String := Null_Unbounded_Wide_Wide_String;
         when Types.Vector =>
            Vector    : aliased Of_Array.Vector := Of_Array.Empty_Vector;
         when Types.Object =>
            Object    : aliased Of_Object.Map := Of_Object.Empty_Map;
         when Types.Undefined =>
            null;
      end case;
   end record;

   type Node_Stub (Disc : Node_Access) is new Ada.Finalization.Controlled with record
      Value : Node_Value;
   end record;

   overriding procedure Initialize (Item : in out Node_Stub);
   overriding procedure Adjust (Item : in out Node_Stub);
   overriding procedure Finalize (Item : in out Node_Stub);

   type Node is tagged record
      Node_Type  : aliased Types.Json_Type := Types.Undefined;
      Value      : Node_Value;
   end record
     with
       Read => Node_Read,
       Write => Node_Write;

   package Scanner is

      W_Space  : constant Wide_Wide_String
        := (Space, HT, LF, CR);

      Numeric : constant array (Positive range <>) of Wide_Character
        := ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'e', 'E', '-', '+', '.');

      type State is (Ready, Literal, String, Escape);

      type Context is record
         Format  : Types.Json_Type := Types.Undefined;
         Index   : Positive := 1;
      end record;

      type Node is record
         Format  : Types.Json_Type := Types.Undefined;
         First   : Natural := 0;
         Last    : Natural := 0;
      end record;

      package Of_Node is new Ada.Containers.Indefinite_Vectors (Positive, Node);
      use Of_Node;

      package Of_Context is new Ada.Containers.Indefinite_Vectors (Positive, Context);

      function Get_Type (Subject : Of_Context.Vector) return Types.Json_Type;

      function Pop (Subject : in out Of_Context.Vector) return Positive;

   end Scanner;

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

end JSON;






















-- Micah says "Hi!"
