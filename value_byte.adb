with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Characters.Handling;

procedure Value_Byte is
   use Ada;
   use Ada.Text_IO;
   use Ada.Text_IO.Text_Streams;

   Input_End : Boolean := False;
   Input_Not_Natural_Number : exception;

   -- Create a string of characters from a string of numerical values.
   function Image_Val (S : String) return String is
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
   loop
      String'Write (Stream (Current_Output), Image_Val (Text_IO.Get_Line));
      exit when Text_IO.End_Of_File (Text_IO.Current_Input);
   end loop;
end Value_Byte;
