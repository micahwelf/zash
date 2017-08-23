with Ada.Characters;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Wide_Latin_1;
with Ada.Wide_Characters;
with Ada.Wide_Characters.Unicode;
with Ada.Wide_Characters.Handling;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Wide_Text_IO;
with Ada.Wide_Text_IO.Text_Streams;
with JSON;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
package Shell is

   package Types is
      type Store_Type is (Normal, Wide);
      type Variable_Type is (Special, String, Bytes, Float, Integer, Json, Regex, Assoc_Array, Scalar_Array, Function_Source);
   end Types;

   subtype Byte is Standard.Character;
   subtype Bytes is Standard.String;

   type Node;
   type Access_Node is access all Shell.Node;
   type Access_JSON is access all JSON.Node;


   package Manager is
      package Strings_Local is new Ada.Containers.Indefinite_Ordered_Maps (Bytes, Bytes);
      package Wide_Strings_Local is new Ada.Containers.Indefinite_Ordered_Maps (Wide_String, Wide_String);
      function "=" (Left, Right : Strings_Local.Constant_Reference_Type)return Boolean;
      function "=" (Left, Right : Wide_Strings_Local.Constant_Reference_Type)return Boolean;

      package Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
      package Wide_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Wide_String);

      package Float_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Long_Long_Float);
      package Integer_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Long_Long_Integer);


      --subtype String_Map is Manager.Strings.Map;
      --subtype Wide_String_Map is Manager.Map_Wide_Strings.Map;

      package Strings_Visible is new Ada.Containers.Indefinite_Ordered_Maps (String, Strings_Local.Constant_Reference_Type);
      package Scoped_Wide_Links is new Ada.Containers.Indefinite_Ordered_Maps (String, Wide_Strings_Local.Constant_Reference_Type);

   end Manager;

   package Varables is

      type String is record
         String_Map      : Manager.Strings.Map;
         Wide_String_Map : Manager.Wide_Strings.Map;
      end record;


      type Wide_String_Reference_Type (String : not null access Wide_String) is private;
      --type String_Constant_Reference_Type (String : not null access constant Bytes) is private;
      --type Wide_String_Constant_Reference_Type (String : not null access constant Wide_String) is private;

   private
      type String_Reference_Type (String : not null access Standard.String) is record null; end record;
      type Wide_String_Reference_Type (String : not null access Wide_String) is record null; end record;

   end Varables;

   type Node is record
      Of_Type : Types.Variable_Type;
      Node    : Access_Node := null;

   end record;

   package IO is
      package Wide_IO renames Ada.Wide_Text_IO;
      package Text_IO renames Ada.Text_IO;
      function Read (File : Wide_IO.File_Type; Length : Integer := -1) return Wide_String;
      function Read (File : Text_IO.File_Type; Length : Integer := -1) return Wide_String;
      function Read (File : Text_IO.File_Type; Length : Integer := -1) return Bytes;
      procedure Write (File : Wide_Io.File_Type; String : Wide_String);
      procedure Write (File : Text_IO.File_Type; String : Standard.String);
   end IO;


end Shell;
