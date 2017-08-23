package body Regex is

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Agent      : in out Regex.Agent;
      Expression : in Regex.Expression;
      Flags      : Regex_Flags := No_Flags;
      Source     : access String := null)
   is
      use GNAT.Regpat;
      Regpat_Flags : GNAT.Regpat.Regexp_Flags := Flags;
   begin

      if
        (Flags and Regex.Overlap) /= 0
      then
         Agent.Overlap := True;
         Regpat_Flags := Regpat_Flags - Regex.Overlap; end if;
      if
        (Flags and Regex.Cache) /= 0
      then
         Agent.Cache := True;
         Regpat_Flags := Regpat_Flags - Regex.Cache; end if;
      if
        (Flags and Regex.Global) /= 0
      then
         Agent.Global := True;
         Regpat_Flags := Regpat_Flags - Regex.Global;
      end if;
      GNAT.Regpat.Compile (Matcher         => Agent.Matcher,
                           Expression      => Expression,
                           Final_Code_Size => Agent.Size,
                           Flags           => Regpat_Flags);
      Agent.Sub_Count := Sub_Count (Agent => Agent);
      if Source /= null then
         Agent.Source := Regex.Source_Access (Source);
      end if;

   end Compile;

   ---------------
   -- Sub_Count --
   ---------------

   function Sub_Count (Agent : in Regex.Agent) return Natural is
      use GNAT.Regpat;
   begin
      return Natural (Paren_Count (Regexp => Agent.Matcher));
   end Sub_Count;

   -----------
   -- Match --
   -----------

   procedure Match (Agent : in out Regex.Agent) is
      use GNAT.Regpat;
      Each_Match : Match_Array (0 .. Agent.Sub_Count);
   begin

      if Agent.Source = null then Agent.Complete := True; end if;

      loop

         if not
           Agent.Complete
         then
            Match (Self       => Agent.Matcher,
                   Data       => Agent.Source.all,
                   Matches    => Each_Match,
                   Data_First => (if
                                    Agent.Matches.Is_Empty
                                  then
                                     Agent.Source'First
                                  else
                                    (if Agent.Overlap
                                     then Agent.Matches.Last_Element (0).Last + 1
                                     else Agent.Matches.Last_Element (0).First + 1)),
                   Data_Last  => Agent.Source'Last);

         end if;

         if
           Each_Match (0) = No_Match
         then
            Agent.Complete := True;
         else
            Agent.Matches.Append (Each_Match);
            if
              Agent.Cache
            then
               for Location of Agent.Matches.Last_Element
               loop
                  Agent.Cache_Store.Include
                    (Key      => (Agent.Matches.Last_Index, Location),
                     New_Item => Agent.Source (Location.First .. Location.Last));
               end loop;
            end if;
         end if;

         exit when (if Agent.Global then Agent.Complete else True);

      end loop;

   end Match;

   -----------
   -- Match --
   -----------

   function Match (Agent : in out Regex.Agent) return Boolean is
      use GNAT.Regpat;
      Each_Match : Match_Array (0 .. Agent.Sub_Count);
      Result     : Boolean := False;
   begin

      if Agent.Source = null then Agent.Complete := True; end if;

      loop

         if not
           Agent.Complete
         then
            Match (Self       => Agent.Matcher,
                   Data       => Agent.Source.all,
                   Matches    => Each_Match,
                   Data_First => (if
                                    Agent.Matches.Is_Empty
                                  then
                                     Agent.Source'First
                                  else
                                    (if Agent.Overlap
                                     then Agent.Matches.Last_Element (0).Last + 1
                                     else Agent.Matches.Last_Element (0).First + 1)),
                   Data_Last  => Agent.Source'Last);

         end if;

         if
           Each_Match (0) = No_Match
         then
            Agent.Complete := True;
         else
            Agent.Matches.Append (Each_Match);
            if
              Agent.Cache
            then
               for Location of Agent.Matches.Last_Element
               loop
                  Agent.Cache_Store.Include
                    (Key      => (Agent.Matches.Last_Index, Location),
                     New_Item => Agent.Source (Location.First .. Location.Last));
               end loop;
            end if;
         end if;

         exit when (if Agent.Global then Agent.Complete else True);

      end loop;

      return Result;

   end Match;

   -----------
   -- Match --
   -----------

   procedure Match
     (Agent  : in out Regex.Agent;
      Source : in String)
   is
      use GNAT.Regpat;
      Each_Match : Match_Array (0 .. Agent.Sub_Count);
   begin

      if Agent.Source = null then Agent.Complete := True; end if;

      loop

         if not
           Agent.Complete
         then
            Match (Self       => Agent.Matcher,
                   Data       => Agent.Source.all,
                   Matches    => Each_Match,
                   Data_First => (if
                                    Agent.Matches.Is_Empty
                                  then
                                     Agent.Source'First
                                  else
                                    (if Agent.Overlap
                                     then Agent.Matches.Last_Element (0).Last + 1
                                     else Agent.Matches.Last_Element (0).First + 1)),
                   Data_Last  => Agent.Source'Last);

         end if;

         if
           Each_Match (0) = No_Match
         then
            Agent.Complete := True;
         else
            Agent.Matches.Append (Each_Match);
            if
              Agent.Cache
            then
               for Location of Agent.Matches.Last_Element
               loop
                  Agent.Cache_Store.Include
                    (Key      => (Agent.Matches.Last_Index, Location),
                     New_Item => Agent.Source (Location.First .. Location.Last));
               end loop;
            end if;
         end if;

         exit when (if Agent.Global then Agent.Complete else True);

      end loop;

   end Match;

   -----------
   -- Match --
   -----------

   function Match
     (Agent  : in out Regex.Agent;
      Source : in String)
      return Boolean
   is
      use GNAT.Regpat;
      Each_Match : Match_Array (0 .. Agent.Sub_Count);
      Result     : Boolean := False;
   begin
      if Agent.Source = null then Agent.Complete := True; end if;

      loop

         if not
           Agent.Complete
         then
            Match (Self       => Agent.Matcher,
                   Data       => Agent.Source.all,
                   Matches    => Each_Match,
                   Data_First => (if
                                    Agent.Matches.Is_Empty
                                  then
                                     Agent.Source'First
                                  else
                                    (if Agent.Overlap
                                     then Agent.Matches.Last_Element (0).Last + 1
                                     else Agent.Matches.Last_Element (0).First + 1)),
                   Data_Last  => Agent.Source'Last);

         end if;

         if
           Each_Match (0) = No_Match
         then
            Agent.Complete := True;
         else
            Agent.Matches.Append (Each_Match);
            if
              Agent.Cache
            then
               for Location of Agent.Matches.Last_Element
               loop
                  Agent.Cache_Store.Include
                    (Key      => (Agent.Matches.Last_Index, Location),
                     New_Item => Agent.Source (Location.First .. Location.Last));
               end loop;
            end if;
         end if;

         exit when (if Agent.Global then Agent.Complete else True);

      end loop;

      return Result;
   end Match;

   ---------
   -- Get --
   ---------

   function Get
     (Agent    : in out Regex.Agent;
      SubIndex : Natural := 0)
      return String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get unimplemented");
      raise Program_Error with "Unimplemented function Get";
      return Get (Agent => Agent, SubIndex => SubIndex);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Agent    : in out Regex.Agent;
      Index    : Natural;
      SubIndex : Natural)
      return String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get unimplemented");
      raise Program_Error with "Unimplemented function Get";
      return Get (Agent => Agent, Index => Index, SubIndex => SubIndex);
   end Get;

   -----------
   -- Count --
   -----------

   function Count (Agent : in Regex.Agent) return Natural is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Count unimplemented");
      raise Program_Error with "Unimplemented function Count";
      return Count (Agent => Agent);
   end Count;

   -----------
   -- Clear --
   -----------

   procedure Clear (Agent : in out Regex.Agent) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Clear unimplemented");
      raise Program_Error with "Unimplemented procedure Clear";
   end Clear;

   -----------
   -- Quote --
   -----------

   function Quote (Source : in String) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Quote unimplemented");
      raise Program_Error with "Unimplemented function Quote";
      return Quote (Source => Source);
   end Quote;

   ----------
   -- Dump --
   ----------

   procedure Dump (Agent : in Regex.Agent) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Dump unimplemented");
      raise Program_Error with "Unimplemented procedure Dump";
   end Dump;

   ----------------------------
   -- Get_Reference_Location --
   ----------------------------

   function Get_Reference_Location
     (Agent    : in out Regex.Agent;
      SubIndex : Natural)
      return Reference_Location
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Reference_Location unimplemented");
      raise Program_Error with "Unimplemented function Get_Reference_Location";
      return Get_Reference_Location (Agent => Agent, SubIndex => SubIndex);
   end Get_Reference_Location;

   -----------------------------------
   -- Get_Constant_Reference_String --
   -----------------------------------

   function Get_Constant_Reference_String
     (Agent    : in out Regex.Agent;
      SubIndex : Natural)
      return Constant_Reference_String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Constant_Reference_String unimplemented");
      raise Program_Error with "Unimplemented function Get_Constant_Reference_String";
      return Get_Constant_Reference_String (Agent    => Agent,
                                            SubIndex => SubIndex);
   end Get_Constant_Reference_String;

   -----------------------------------
   -- Get_Constant_Reference_String --
   -----------------------------------

   function Get_Constant_Reference_String
     (Agent    : in out Regex.Agent;
      Source   : in String;
      SubIndex : Natural := 0)
      return Constant_Reference_String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Constant_Reference_String unimplemented");
      raise Program_Error with "Unimplemented function Get_Constant_Reference_String";
      return Get_Constant_Reference_String (Agent    => Agent, Source => Source,
                                            SubIndex => SubIndex);
   end Get_Constant_Reference_String;

   -----------------------------------
   -- Get_Constant_Reference_String --
   -----------------------------------

   function Get_Constant_Reference_String
     (Agent    : in out Regex.Agent;
      Index    : Natural;
      SubIndex : Natural)
      return Constant_Reference_String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Constant_Reference_String unimplemented");
      raise Program_Error with "Unimplemented function Get_Constant_Reference_String";
      return Get_Constant_Reference_String (Agent    => Agent, Index => Index,
                                            SubIndex => SubIndex);
   end Get_Constant_Reference_String;

   -----------------------------------
   -- Get_Constant_Reference_String --
   -----------------------------------

   function Get_Constant_Reference_String
     (Agent    : in out Regex.Agent;
      Source   : in String;
      Index    : Natural;
      SubIndex : Natural)
      return Constant_Reference_String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Constant_Reference_String unimplemented");
      raise Program_Error with "Unimplemented function Get_Constant_Reference_String";
      return Get_Constant_Reference_String (Agent => Agent, Source => Source,
                                            Index => Index, SubIndex => SubIndex);
   end Get_Constant_Reference_String;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Cache_Index) return Boolean is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """="" unimplemented");
      raise Program_Error with "Unimplemented function ""=""";
      return "=" (Left => Left, Right => Right);
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Cache_Index) return Boolean is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """<"" unimplemented");
      raise Program_Error with "Unimplemented function ""<""";
      return "<" (Left => Left, Right => Right);
   end "<";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Cache_Index) return Boolean is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """>"" unimplemented");
      raise Program_Error with "Unimplemented function "">""";
      return ">" (Left => Left, Right => Right);
   end ">";

end Regex;
