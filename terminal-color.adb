with Text_IO;
with Text_IO.Text_Streams;
with Ada.Streams.Stream_IO;

package body Terminal.Color is
   use Text_IO;
   use Text_IO.Text_Streams;

   procedure Apply (Color : Bytes := White) is
   begin
      for X of Color loop
         Character'Write
           (Stream (Standard_Output),
            Character'Val (X));

      end loop;

   end Apply;

   function Image (Color : Bytes := White) return String
   is
      Escape_Code : String (1 .. Color'Length);
   begin
      for X in Color'Range loop
         Escape_Code (X) :=
           Character'Val (Color (X));

      end loop;

      return Escape_Code;

   end Image;

   function "-" (Color : Bytes) return String
   is
      Escape_Code : String (1 .. Color'Length);
   begin
      for X in Color'Range loop
         Escape_Code (X) :=
           Character'Val (Color (X));

      end loop;

      return Escape_Code;

   end "-";

end Terminal.Color;
