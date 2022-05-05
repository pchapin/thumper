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
with Hermes.DER.Decode;
--with Hermes.DER.Encode;
with Ada.Calendar.Formatting;             
with Ada.Calendar.Time_Zones;             


package body Data_Storage is


   ----------------------
   -- Private Subprograms
   ----------------------

   -- Converts a Time type into a Generalized Time String.
   procedure TimeToGeneralizedTime(t : in Time; gt : out String) is
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
   end TimeToGeneralizedTime;

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
      Ihash       : Integer;
      Stop        : Natural                                          := Ohash'Last;
      Sthash      : Hermes.DER.Status_Type;
      Serial      : constant Serial_Number_Type                      := Stamp.Serial_Number;
      GTime       : constant String(1..15)                           := Stamp.Generalized_Time;
   begin
      Hermes.OID.To_Separates(OIDpolicy, Cpolicy, Npolicy);
      Hermes.OID.OIDToString (Cpolicy, Spolicy);
      Hermes.OID.To_Separates(OIDalgo, Calgo, Nalgo);
      Hermes.OID.OIDToString (Calgo, Salgo);
      Hermes.DER.Decode.Get_Integer_Value (Message => Ohash, Start => Ohash'First, Stop => Stop, Value => Ihash, Status => Sthash);
      -- Debug: Ada.Text_IO.Put_Line("OID Status: " & Hermes.DER.Status_Type'Image(Sthash)); 
      PostgreSQL.Execute_Query(Query => "INSERT INTO ThumperTable (Version, Policy, Hash_Algorithm, Hash_Message, Serial_Number, Generalized_Time) VALUES ( '" & Positive'Image(Iversion) & "', '" & Spolicy & "', '" & Salgo & "', " & Integer'Image(Ihash) & ", " & Serial_Number_Type'Image(Serial) & ", '" & GTime & "');");
   end Timestamp_Store;


   --TODO: Work on Hashed_Message retrieval 
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
               Smess       : Integer;                                   -- Hash_Message
               --Omess       : Hermes.Octet_Array(1..Timestamp_Messages.Hash_Size);
               Snum        : Serial_Number_Type;                        -- Serial_Number
               Stime       : String(1..15);                             -- Generalized_Time   
            begin
               Iversion := Positive'Value(PostgreSQL.Get_Value ((I - 1),0));
                  Ada.Text_IO.Put_Line ("Version: " & Iversion'Image);
                  Ada.Text_IO.Put_Line("");
               Spolicy := PostgreSQL.Get_Value ((I - 1), 1);
                  Ada.Text_IO.Put_Line (Item => "Policy: " & Spolicy);
               Hermes.OID.StringToArray (Spolicy, Cpolicy);
               Hermes.OID.To_Object_Identifier(Separates => Cpolicy, Result => OIDpolicy, Status => Stpolicy);
                  Ada.Text_IO.Put_Line (Item => "OID Status: " & Hermes.OID.Status_Type'Image(Stpolicy));
                  Ada.Text_IO.Put_Line("");
               Salgo := PostgreSQL.Get_Value ((I - 1), 2);
                  Ada.Text_IO.Put_Line (Item => "Hash Algorithm: " & Salgo);
               Hermes.OID.StringToArray(Salgo, Calgo);
               Hermes.OID.To_Object_Identifier(Separates => Calgo, Result => OIDalgo, Status => Stalgo);
                  Ada.Text_IO.Put_Line ("OID Status: " & Hermes.OID.Status_Type'Image(Stalgo));
                  Ada.Text_IO.Put_Line("");
               Smess := Integer'Value(PostgreSQL.Get_Value ((I - 1), 3));
                  Ada.Text_IO.Put_Line (Item => "Hashed Message: " & Integer'Image(Smess));
                  Ada.Text_IO.Put_Line("");
               --Omess := Hermes.DER.Encode.Put_Integer_Value(Smess); TODO: length check fail
               Snum := Serial_Number_Type'Value(PostgreSQL.Get_Value ((I - 1), 4));
                  Ada.Text_IO.Put_Line (Item => "Serial Number: " & Serial_Number_Type'Image(Snum));
                  Ada.Text_IO.Put_Line("");
               Stime := PostgreSQL.Get_Value (Tuple_Number => (I - 1), Field_Number => 5);
                  Ada.Text_IO.Put_Line (Item => "Generalized Time: " & Stime);
                  Ada.Text_IO.Put_Line("");
               Result(I).Version          := Iversion;
               Result(I).Policy           := OIDpolicy;
               Result(I).Hash_Algorithm   := OIDalgo;
               --Result(I).Hashed_Message   := Omess;  
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


   --TODO: Work on Hashed_Message retrieval 
   function Timestamp_Retrieve(Start : Time; Stop : Time) return Timestamp_Array is
      Tuples   : Count_Type;
      StartS   : String(1..15);
      StopS    : String(1..15);
   begin
      TimeToGeneralizedTime (Start, StartS);
      TimeToGeneralizedTime (Stop, StopS);
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
               Smess       : Integer;                                   -- Hash_Message
               --Omess       : Hermes.Octet_Array(1..Timestamp_Messages.Hash_Size);
               Snum        : Serial_Number_Type;                        -- Serial_Number
               Stime       : String(1..15);                             -- Generalized_Time   
            begin
               Iversion := Positive'Value(PostgreSQL.Get_Value ((I - 1),0));
                  Ada.Text_IO.Put_Line ("Version: " & Iversion'Image);
                  Ada.Text_IO.Put_Line("");
               Spolicy := PostgreSQL.Get_Value ((I - 1), 1);
                  Ada.Text_IO.Put_Line (Item => "Policy: " & Spolicy);
               Hermes.OID.StringToArray (Spolicy, Cpolicy);
               Hermes.OID.To_Object_Identifier(Separates => Cpolicy, Result => OIDpolicy, Status => Stpolicy);
                  Ada.Text_IO.Put_Line (Item => "OID Status: " & Hermes.OID.Status_Type'Image(Stpolicy));
                  Ada.Text_IO.Put_Line("");
               Salgo := PostgreSQL.Get_Value ((I - 1), 2);
                  Ada.Text_IO.Put_Line (Item => "Hash Algorithm: " & Salgo);
               Hermes.OID.StringToArray(Salgo, Calgo);
               Hermes.OID.To_Object_Identifier(Separates => Calgo, Result => OIDalgo, Status => Stalgo);
                  Ada.Text_IO.Put_Line ("OID Status: " & Hermes.OID.Status_Type'Image(Stalgo));
                  Ada.Text_IO.Put_Line("");
               Smess := Integer'Value(PostgreSQL.Get_Value ((I - 1), 3));
                  Ada.Text_IO.Put_Line (Item => "Hashed Message: " & Integer'Image(Smess));
                  Ada.Text_IO.Put_Line("");
               -- Omess := Hermes.DER.Encode.Put_Integer_Value(Smess); TODO: length check fail
               Snum := Serial_Number_Type'Value(PostgreSQL.Get_Value ((I - 1), 4));
                  Ada.Text_IO.Put_Line (Item => "Serial Number: " & Serial_Number_Type'Image(Snum));
                  Ada.Text_IO.Put_Line("");
               Stime := PostgreSQL.Get_Value (Tuple_Number => (I - 1), Field_Number => 5);
                  Ada.Text_IO.Put_Line (Item => "Generalized Time: " & Stime);
                  Ada.Text_IO.Put_Line("");
               Result(I).Version          := Iversion;
               Result(I).Policy           := OIDpolicy;
               Result(I).Hash_Algorithm   := OIDalgo;
               --Result(I).Hashed_Message   := Omess; 
               Result(I).Serial_Number    := Snum;
               Result(I).Generalized_Time := Stime;       
            end;
         end loop;
      return Result;
      end;
   end Timestamp_Retrieve;

end Data_Storage;
