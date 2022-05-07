---------------------------------------------------------------------------
-- FILE    : data_storage.adb
-- SUBJECT : Body of an abstract data base interface package for Thumper.
-- AUTHOR  : (C) Copyright 2015 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------

-- TODO: Remove dependency on Ada.Text_IO. Use a real logger.
with Postgresql;
with Server_Logger;                       use Server_Logger;
with Ada.Text_IO;
with Hermes.OID;
with Ada.Calendar.Formatting;             
with Ada.Calendar.Time_Zones;   
with Cryptographic_Services;  
with Ada.Strings;                         use Ada.Strings;
with Ada.Strings.Fixed;                   use Ada.Strings.Fixed;
with Ada.Strings.Maps;                    use Ada.Strings.Maps;


package body Data_Storage is


   ----------------------
   -- Private Subprograms
   ----------------------

   -- Converts a Time type into a Generalized Time String.
   procedure Time_To_GeneralizedTime(t : in Time; gt : out String) is
      Text      : constant String                     := Ada.Calendar.Formatting.Image(t);
      tz        : constant Time_Zones.Time_Offset     := Time_Zones.UTC_Time_Offset (t);
      tzs       : constant String                     := Time_Zones.Time_Offset'Image(tz);
      gti       : Positive                            := gt'First; --gt iterator
      dash      : constant Character                  := '-';
      colon     : constant Character                  := ':';
      blank     : constant Character                  := ' ';
   begin
      for I in Text'Range loop
         if Text(I) /= dash and Text(I) /= colon and Text(I) /= blank then
            gt(gti) := Text(I);
            gti := gti + 1;
         end if; 
      end loop;
      -- Debug:Ada.Text_IO.Put_Line(tzs);
      gt(gt'First + 14) := tzs(2); 
      -- TODO: Time zones need to be formatted to match Generalized_Time, right now they are returning the second character in a string representing the UFC Time Offset
      -- Debug: Ada.Text_IO.Put_Line(gt);
   end Time_To_GeneralizedTime;

   -- Converts a String of 2 Hexadecimals into an Octet
   procedure String_To_Octet(Text : in String; Num : in out Natural; Component  : out Hermes.Octet) is  
       Value      : Integer;
       Temp_Value : Integer;
   begin
      if Text(Text'First) = 'A' then
         Temp_Value := 10;
      elsif Text(Text'First) = 'B' then
         Temp_Value := 11;
      elsif Text(Text'First) = 'C' then
         Temp_Value := 12;
      elsif Text(Text'First) = 'D' then
         Temp_Value := 13;
      elsif Text(Text'First) = 'E' then
         Temp_Value := 14;
      elsif Text(Text'First) = 'F' then
         Temp_Value := 15;
      elsif Text(Text'First) = '0' then
         Temp_Value := 0;
      elsif Text(Text'First) = '1' then
         Temp_Value := 1;
      elsif Text(Text'First) = '2' then
         Temp_Value := 2;
      elsif Text(Text'First) = '3' then
         Temp_Value := 3;
      elsif Text(Text'First) = '4' then
         Temp_Value := 4;
      elsif Text(Text'First) = '5' then
         Temp_Value := 5;
      elsif Text(Text'First) = '6' then
         Temp_Value := 6;
      elsif Text(Text'First) = '7' then
         Temp_Value := 7;
      elsif Text(Text'First) = '8' then
         Temp_Value := 8;
      else 
         Temp_Value := 9;
      end if;
      Value := Temp_Value * 16;
      if Text(Text'Last) = 'A' then
         Temp_Value := 10;
      elsif Text(Text'Last) = 'B' then
         Temp_Value := 11;
      elsif Text(Text'Last) = 'C' then
         Temp_Value := 12;
      elsif Text(Text'Last) = 'D' then
         Temp_Value := 13;
      elsif Text(Text'Last) = 'E' then
         Temp_Value := 14;
      elsif Text(Text'Last) = 'F' then
         Temp_Value := 15;
      elsif Text(Text'Last) = '0' then
         Temp_Value := 0;
      elsif Text(Text'Last) = '1' then
         Temp_Value := 1;
      elsif Text(Text'Last) = '2' then
         Temp_Value := 2;
      elsif Text(Text'Last) = '3' then
         Temp_Value := 3;
      elsif Text(Text'Last) = '4' then
         Temp_Value := 4;
      elsif Text(Text'Last) = '5' then
         Temp_Value := 5;
      elsif Text(Text'Last) = '6' then
         Temp_Value := 6;
      elsif Text(Text'Last) = '7' then
         Temp_Value := 7;
      elsif Text(Text'Last) = '8' then
         Temp_Value := 8;
      else 
         Temp_Value := 9;
      end if;  
      Value := Value + Temp_Value; 
      Component := Hermes.Octet(Value);
      Num := Num + 1;
      --Debug: Ada.Text_IO.Put_Line(Text);
   end String_To_Octet;
   

   -- Converts a String of 8 bit hexadecimals ('2e 5d ee 00 12') to an Octet_Array (Hermes.ads)
   procedure String_To_Octet_Array(Text : in String; Result : out Hermes.Octet_Array) is
      num       : Natural := 0;         -- Component_Array index value
      component : Hermes.Octet;
      alpha     : constant Character_Set  := To_Set(' ');
      iter      : Natural := 1;         --iterator
      lower     : Positive;             --lower index location for numbers
      upper     : Natural;              --upper index location for numbers
   begin
      while iter in Text'Range loop
         Find_Token (Source => Text, 
                     Set => alpha,
                     From => iter,
                     Test => Outside, 
                     First => lower, 
                     Last => upper);
      exit when upper = 0;
         -- DeBug: Ada.Text_IO.Put_Line(Integer'Image(lower));
         -- DeBug: Ada.Text_IO.Put_Line(Integer'Image(upper));
         String_To_Octet(Text(lower..upper), num, component);
         Result(num) := component; 
         iter := upper + 1;
      end loop;
   end String_To_Octet_Array;


   ----------------------
   -- Public Subprograms
   ----------------------

   procedure Initialize is
   begin
      -- TODO: Database connectivity information should come from a configuration file.
      Postgresql.Connect("localhost", 5432, "ThumperServer", "thumper", "rabbitsfoot");
      Write_Information("Connected to the Database.");
   end Initialize;


   procedure Shutdown is
   begin
      Postgresql.Disconnect;
      Write_Information("Disconnected From the Database.");
   end Shutdown;


   function Timestamp_Count return Count_Type is
   begin
      PostgreSQL.Execute_Query (Query => "SELECT COUNT(Timestamp_Count) FROM ThumperTable;");
      return Count_Type'Value(PostgreSQL.Get_Value(0,0));
   end Timestamp_Count;


   -- TODO: ThumperTable also stores IP addresses, but Timestamp_Store currently only stores Timestamps 
   procedure Timestamp_Store(Stamp : in Timestamp) is
      Iversion    : constant Positive                                := Stamp.Version;
      OIDpolicy   : constant Hermes.OID.Object_Identifier            := Stamp.Policy;
      Cpolicy     : Hermes.OID.Component_Array(1..Hermes.OID.Maximum_Component_Count);
      Npolicy     : Hermes.OID.Component_Count_Type;
      Spolicy     : String(1..128);         
      OIDalgo     : constant Hermes.OID.Object_Identifier            := Stamp.Hash_Algorithm;
      Calgo       : Hermes.OID.Component_Array(1..Hermes.OID.Maximum_Component_Count);
      Nalgo       : Hermes.OID.Component_Count_Type;
      Salgo       : String(1..128);        
      Ohash       : constant Hermes.Octet_Array                      := Stamp.Hashed_Message;
      Shash       : String(1..256);
      Serial      : constant Serial_Number_Type                      := Stamp.Serial_Number;
      GTime       : constant String(1..15)                           := Stamp.Generalized_Time;
   begin
      Hermes.OID.To_Separates(OIDpolicy, Cpolicy, Npolicy);
      Hermes.OID.OID_To_String (Cpolicy, Spolicy);
      Hermes.OID.To_Separates(OIDalgo, Calgo, Nalgo);
      Hermes.OID.OID_To_String (Calgo, Salgo);
      Shash := Cryptographic_Services.Octets_To_String(Ohash);
      -- Debug: Ada.Text_IO.Put_Line("Hashed_Message: " & Shash); 
      PostgreSQL.Execute_Query(Query => "INSERT INTO ThumperTable (Version, Policy, Hash_Algorithm, Hash_Message, Serial_Number, Generalized_Time) VALUES ( '" & Positive'Image(Iversion) & "', '" & Spolicy & "', '" & Salgo & "', " & Shash & ", " & Serial_Number_Type'Image(Serial) & ", '" & GTime & "');");
   end Timestamp_Store;


   function Timestamp_Retrieve(Serial_Number : Serial_Number_Type) return Timestamp_Array is
      Tuples : Count_Type;
   begin
      PostgreSQL.Execute_Query(Query => "SELECT Version, Policy, Hash_Algorithm, Hash_Message, Serial_Number, Generalized_Time FROM ThumperTable WHERE Serial_Number = " & Serial_Number_Type'Image(Serial_Number) & ";");
      Tuples := PostgreSQL.Number_Of_Tuples;
      declare
         Result : Timestamp_Array(1 .. Tuples); 
      begin
         if Tuples >= 1 then
            for I in 1 .. Tuples loop
            declare
               Iversion    : Positive;                                  -- Version
               Spolicy     : String(1..128);                            -- Policy                 
               Cpolicy     : Hermes.OID.Component_Array(1..Hermes.OID.Maximum_Component_Count);                
               OIDpolicy   : Hermes.OID.Object_Identifier;
               Stpolicy    : Hermes.OID.Status_Type;
               Salgo       : String(1..128);                            -- Hash_Algorithm
               Calgo       : Hermes.OID.Component_Array(1..Hermes.OID.Maximum_Component_Count);
               OIDalgo     : Hermes.OID.Object_Identifier;
               Stalgo      : Hermes.OID.Status_Type;
               Smess       : String(1..96);                             -- Hash_Message
               Omess       : Hermes.Octet_Array(1..Timestamp_Messages.Hash_Size);
               Snum        : Serial_Number_Type;                        -- Serial_Number
               Stime       : String(1..15);                             -- Generalized_Time   
            begin
               Iversion := Positive'Value(PostgreSQL.Get_Value ((I - 1),0));
                  Ada.Text_IO.Put_Line ("Version: " & Iversion'Image);
                  Ada.Text_IO.Put_Line("");
               Spolicy := PostgreSQL.Get_Value ((I - 1), 1);
                  Ada.Text_IO.Put_Line (Item => "Policy: " & Spolicy);
               Hermes.OID.String_To_Array (Spolicy, Cpolicy);
               Hermes.OID.To_Object_Identifier(Separates => Cpolicy, Result => OIDpolicy, Status => Stpolicy);
                  Ada.Text_IO.Put_Line (Item => "OID Status: " & Hermes.OID.Status_Type'Image(Stpolicy));
                  Ada.Text_IO.Put_Line("");
               Salgo := PostgreSQL.Get_Value ((I - 1), 2);
                  Ada.Text_IO.Put_Line (Item => "Hash Algorithm: " & Salgo);
               Hermes.OID.String_To_Array(Salgo, Calgo);
               Hermes.OID.To_Object_Identifier(Separates => Calgo, Result => OIDalgo, Status => Stalgo);
                  Ada.Text_IO.Put_Line ("OID Status: " & Hermes.OID.Status_Type'Image(Stalgo));
                  Ada.Text_IO.Put_Line("");
               Smess := PostgreSQL.Get_Value ((I - 1), 3);
                  Ada.Text_IO.Put_Line (Item => "Hashed Message: " & Smess);
                  Ada.Text_IO.Put_Line("");
               String_To_Octet_Array (Smess, Omess);
               Snum := Serial_Number_Type'Value(PostgreSQL.Get_Value ((I - 1), 4));
                  Ada.Text_IO.Put_Line (Item => "Serial Number: " & Serial_Number_Type'Image(Snum));
                  Ada.Text_IO.Put_Line("");
               Stime := PostgreSQL.Get_Value (Tuple_Number => (I - 1), Field_Number => 5);
                  Ada.Text_IO.Put_Line (Item => "Generalized Time: " & Stime);
                  Ada.Text_IO.Put_Line("");
               Result(I).Version          := Iversion;
               Result(I).Policy           := OIDpolicy;
               Result(I).Hash_Algorithm   := OIDalgo;
               Result(I).Hashed_Message   := Omess;  
               Result(I).Serial_Number    := Snum;
               Result(I).Generalized_Time := Stime;       
            end;
         end loop;
         else
            raise Program_Error with "Invalid Serial Number: not assigned.";
         end if;
         return Result;
      end;
   end Timestamp_Retrieve;


   function Timestamp_Retrieve(Start : Time; Stop : Time) return Timestamp_Array is
      Tuples   : Count_Type;
      StartS   : String(1..15);
      StopS    : String(1..15);
   begin
      Time_To_GeneralizedTime (Start, StartS);
      Time_To_GeneralizedTime (Stop, StopS);
      if StartS > StopS then
         raise Program_Error with "Invalid Times: start time comes after stop time.";
      end if;
      PostgreSQL.Execute_Query(Query => "SELECT Version, Policy, Hash_Algorithm, Hash_Message, Serial_Number, Generalized_Time FROM ThumperTable WHERE Generalized_Time BETWEEN '" & StartS & "' AND '" & StopS & "';");
      Tuples := PostgreSQL.Number_Of_Tuples;
      declare
         Result   : Timestamp_Array(1 .. Tuples);
      begin
         if Tuples = 0 then
            raise Program_Error with "No Timestamps between" & Ada.Calendar.Formatting.Image(Start) & " and " & Ada.Calendar.Formatting.Image(Stop) & ".";
         end if;
         for I in 1 .. Tuples loop
            declare
               Iversion    : Positive;                                  -- Version
               Spolicy     : String(1..128);                            -- Policy                 
               Cpolicy     : Hermes.OID.Component_Array(1..Hermes.OID.Maximum_Component_Count);                
               OIDpolicy   : Hermes.OID.Object_Identifier;
               Stpolicy    : Hermes.OID.Status_Type;
               Salgo       : String(1..128);                            -- Hash_Algorithm
               Calgo       : Hermes.OID.Component_Array(1..Hermes.OID.Maximum_Component_Count);
               OIDalgo     : Hermes.OID.Object_Identifier;
               Stalgo      : Hermes.OID.Status_Type;
               Smess       : String(1..96);                             -- Hash_Message
               Omess       : Hermes.Octet_Array(1..Timestamp_Messages.Hash_Size);
               Snum        : Serial_Number_Type;                        -- Serial_Number
               Stime       : String(1..15);                             -- Generalized_Time   
            begin
               Iversion := Positive'Value(PostgreSQL.Get_Value ((I - 1),0));
                  Ada.Text_IO.Put_Line ("Version: " & Iversion'Image);
                  Ada.Text_IO.Put_Line("");
               Spolicy := PostgreSQL.Get_Value ((I - 1), 1);
                  Ada.Text_IO.Put_Line (Item => "Policy: " & Spolicy);
               Hermes.OID.String_To_Array (Spolicy, Cpolicy);
               Hermes.OID.To_Object_Identifier(Separates => Cpolicy, Result => OIDpolicy, Status => Stpolicy);
                  Ada.Text_IO.Put_Line (Item => "OID Status: " & Hermes.OID.Status_Type'Image(Stpolicy));
                  Ada.Text_IO.Put_Line("");
               Salgo := PostgreSQL.Get_Value ((I - 1), 2);
                  Ada.Text_IO.Put_Line (Item => "Hash Algorithm: " & Salgo);
               Hermes.OID.String_To_Array(Salgo, Calgo);
               Hermes.OID.To_Object_Identifier(Separates => Calgo, Result => OIDalgo, Status => Stalgo);
                  Ada.Text_IO.Put_Line ("OID Status: " & Hermes.OID.Status_Type'Image(Stalgo));
                  Ada.Text_IO.Put_Line("");
               Smess := PostgreSQL.Get_Value ((I - 1), 3);
                  Ada.Text_IO.Put_Line (Item => "Hashed Message: " & Smess);
                  Ada.Text_IO.Put_Line("");
               String_To_Octet_Array (Smess, Omess);
               Snum := Serial_Number_Type'Value(PostgreSQL.Get_Value ((I - 1), 4));
                  Ada.Text_IO.Put_Line (Item => "Serial Number: " & Serial_Number_Type'Image(Snum));
                  Ada.Text_IO.Put_Line("");
               Stime := PostgreSQL.Get_Value (Tuple_Number => (I - 1), Field_Number => 5);
                  Ada.Text_IO.Put_Line (Item => "Generalized Time: " & Stime);
                  Ada.Text_IO.Put_Line("");
               Result(I).Version          := Iversion;
               Result(I).Policy           := OIDpolicy;
               Result(I).Hash_Algorithm   := OIDalgo;
               Result(I).Hashed_Message   := Omess; 
               Result(I).Serial_Number    := Snum;
               Result(I).Generalized_Time := Stime;       
            end;
         end loop;
      return Result;
      end;
   end Timestamp_Retrieve;

end Data_Storage;
