---------------------------------------------------------------------------
-- FILE    : data_storage.adb
-- SUBJECT : Body of an abstract data base interface package for Thumper.
-- AUTHOR  : (C) Copyright 2022 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <pchapin@vtc.edu>
---------------------------------------------------------------------------

-- TODO: Remove dependency on Ada.Text_IO. Use a real logger.
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Text_IO;

with Hermes.OID;
with Postgresql;
with Server_Logger;

use Server_Logger;

package body Data_Storage is

   ----------------------
   -- Private Subprograms
   ----------------------

   -- Converts a Time type into a Generalized Time String.
   procedure Time_To_GeneralizedTime(T : in Time; GT : out String) is
      Text        : constant String                 := Ada.Calendar.Formatting.Image(T);
      Time_Zone   : constant Time_Zones.Time_Offset := Time_Zones.UTC_Time_Offset(T);
      Time_Zone_String : constant String            := Time_Zones.Time_Offset'Image(Time_Zone);
      GT_Iterator : Positive                        := GT'First;
   begin
      for I in Text'Range loop
         if Text(I) /= '-' and Text(I) /= ':' and Text(I) /= ' ' then
            GT(GT_Iterator) := Text(I);
            GT_Iterator := GT_Iterator + 1;
         end if;
      end loop;
      GT(GT'First + 14) := Time_Zone_String(2);
      -- TODO: Time zones need to be formatted to match Generalized_Time, right now they are
      -- returning the second character in a string representing the UFC Time Offset
   end Time_To_GeneralizedTime;

   ----------------------
   -- Public Subprograms
   ----------------------

   procedure Initialize is
   begin
      -- TODO: Database connectivity information should come from a configuration file.
      Postgresql.Connect
        (Server_Name => "localhost",
         Port        => 5432,
         Database    => "ThumperDB",
         User        => "thumper",
         Password    => "rabbitsfoot");
      Write_Information("Connected to the database.");
   end Initialize;


   procedure Shutdown is
   begin
      Postgresql.Disconnect;
      Write_Information("Disconnected from the database.");
   end Shutdown;


   function Timestamp_Count return Count_Type is
   begin
      PostgreSQL.Execute_Query(Query => "SELECT COUNT(*) FROM timestamp;");
      return Count_Type'Value(PostgreSQL.Get_Value(0,0));
   end Timestamp_Count;


   -- TODO: Table timestamp also stores IP addresses, but Timestamp_Store currently only stores Timestamps
   procedure Timestamp_Store(Stamp : in Timestamp) is
      Policy_Components      : Hermes.OID.Component_Array(1 .. Hermes.OID.Maximum_Component_Count);
      Policy_Component_Count : Hermes.OID.Component_Count_Type;
      Policy_String          : String(1 .. 128);

      Hash_Algorithm_Components : Hermes.OID.Component_Array(1..Hermes.OID.Maximum_Component_Count);
      Hash_Algorithm_Component_Count : Hermes.OID.Component_Count_Type;
      Hash_Algorithm_String     : String(1 .. 128);

      Hash_String : String(1 .. 95);
   begin
      Hermes.OID.To_Separates
        (Stamp.Policy, Policy_Components, Policy_Component_Count);
      Hermes.OID.OID_To_String
        (Policy_Components(1 .. Policy_Component_Count), Policy_String);

      Hermes.OID.To_Separates
        (Stamp.Hash_Algorithm, Hash_Algorithm_Components, Hash_Algorithm_Component_Count);
      Hermes.OID.OID_To_String
        (Hash_Algorithm_Components(1 .. Hash_Algorithm_Component_Count), Hash_Algorithm_String);

      Hash_String := Hermes.Octets_To_String(Stamp.Hashed_Message);

      -- TODO: Protect against SQL injection? Is it a concern here?
      PostgreSQL.Execute_Query
        (Query =>
           "INSERT INTO timestamp " &
             "(version, policy, hash_algorithm, hash, serial_number, generalized_time) " &
           "VALUES ('" &
           Positive'Image(Stamp.Version) & "', '" &
           Policy_String                 & "', '" &
           Hash_Algorithm_String         & "', '" &
           Hash_String                   & "', " &
           Serial_Number_Type'Image(Stamp.Serial_Number) & ", '" &
           Stamp.Generalized_Time        & "');");
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
               Hermes.OID.String_To_OID(Spolicy, Cpolicy);
               Hermes.OID.To_Object_Identifier(Separates => Cpolicy, Result => OIDpolicy, Status => Stpolicy);
                  Ada.Text_IO.Put_Line (Item => "OID Status: " & Hermes.OID.Status_Type'Image(Stpolicy));
                  Ada.Text_IO.Put_Line("");
               Salgo := PostgreSQL.Get_Value ((I - 1), 2);
                  Ada.Text_IO.Put_Line (Item => "Hash Algorithm: " & Salgo);
               Hermes.OID.String_To_OID(Salgo, Calgo);
               Hermes.OID.To_Object_Identifier(Separates => Calgo, Result => OIDalgo, Status => Stalgo);
                  Ada.Text_IO.Put_Line ("OID Status: " & Hermes.OID.Status_Type'Image(Stalgo));
                  Ada.Text_IO.Put_Line("");
               Smess := PostgreSQL.Get_Value ((I - 1), 3);
                  Ada.Text_IO.Put_Line (Item => "Hashed Message: " & Smess);
                  Ada.Text_IO.Put_Line("");
               Hermes.String_To_Octet_Array (Smess, Omess);
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
               Hermes.OID.String_To_OID(Spolicy, Cpolicy);
               Hermes.OID.To_Object_Identifier(Separates => Cpolicy, Result => OIDpolicy, Status => Stpolicy);
                  Ada.Text_IO.Put_Line (Item => "OID Status: " & Hermes.OID.Status_Type'Image(Stpolicy));
                  Ada.Text_IO.Put_Line("");
               Salgo := PostgreSQL.Get_Value ((I - 1), 2);
                  Ada.Text_IO.Put_Line (Item => "Hash Algorithm: " & Salgo);
               Hermes.OID.String_To_OID(Salgo, Calgo);
               Hermes.OID.To_Object_Identifier(Separates => Calgo, Result => OIDalgo, Status => Stalgo);
                  Ada.Text_IO.Put_Line ("OID Status: " & Hermes.OID.Status_Type'Image(Stalgo));
                  Ada.Text_IO.Put_Line("");
               Smess := PostgreSQL.Get_Value ((I - 1), 3);
                  Ada.Text_IO.Put_Line (Item => "Hashed Message: " & Smess);
                  Ada.Text_IO.Put_Line("");
               Hermes.String_To_Octet_Array (Smess, Omess);
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
