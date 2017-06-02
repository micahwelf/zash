with Text_IO;
with Text_IO.Text_Streams;
with Ada.Streams;
with Ada.Streams.Stream_IO;

procedure Byte_Value is
   use Ada;
   use Text_IO;
   use Text_IO.Text_Streams;
   Input_End : Boolean := False;
   Buffer_C  : Character;
   function Image_Pos (C : Character) return String is
      N : Natural          := Natural (Character'Pos (C));
      S : String (1 .. 48) := (others => ' ');
      L : Natural          := 0;
      procedure Image_Natural
        (N : in     Natural;
         S : in out String;
         L : in out Natural)
      is
      begin
         null;
         if N >= 10
         then
            Image_Natural (N / 10, S, L);
            L     := L + 1;
            S (L) := Character'Val (48 + (N rem 10));
         else
            L     := L + 1;
            S (L) := Character'Val (48 + (N rem 10));
         end if;
      end Image_Natural;
   begin
      null;
      Image_Natural (N, S, L);
      return S (1 .. L);
   end Image_Pos;
begin
   loop
      Character'Read (Stream (Text_IO.Standard_Input), Buffer_C);
      String'Write (Stream (Standard_Output), Image_Pos (Buffer_C));
      exit when Text_IO.End_Of_File (Text_IO.Current_Input);
      String'Write (Stream (Standard_Output), " ");
   end loop;
end Byte_Value;
