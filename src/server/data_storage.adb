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


   function Process_Tuples(Number_Of_Tuples : in Count_Type) return Timestamp_Array is
      Version              : Positive;
      Policy_String        : String(1 .. 128);
      Policy_Components    : Hermes.OID.Component_Array(1 .. Hermes.OID.Maximum_Component_Count);
      Policy_OID           : Hermes.OID.Object_Identifier;
      Policy_Status        : Hermes.OID.Status_Type;
      Algorithm_String     : String(1 .. 128);
      Algorithm_Components : Hermes.OID.Component_Array(1 .. Hermes.OID.Maximum_Component_Count);
      Algorithm_OID        : Hermes.OID.Object_Identifier;
      Algorithm_Status     : Hermes.OID.Status_Type;
      Hash_String          : String(1 .. 96);
      Hash_Octets          : Hermes.Octet_Array(1 .. Timestamp_Messages.Hash_Size);
      Serial_Number        : Serial_Number_Type;
      Time_String          : String(1 .. 15);

      Result : Timestamp_Array(1 .. Number_Of_Tuples);
   begin
      -- TODO: This code assumes the strings all have the same size, which isn't really true.
      for I in 1 .. Number_Of_Tuples loop
         Version := Positive'Value(PostgreSQL.Get_Value((I - 1), 0));

         -- TODO: Check the status value from To_Object_Identifier.
         Policy_String := PostgreSQL.Get_Value((I - 1), 1);
         Hermes.OID.String_To_OID(Policy_String, Policy_Components);
         Hermes.OID.To_Object_Identifier
           (Separates => Policy_Components,
            Result    => Policy_OID,
            Status    => Policy_Status);

         -- TODO: Check the status value from To_Object_Identifier.
         Algorithm_String := PostgreSQL.Get_Value((I - 1), 2);
         Hermes.OID.String_To_OID(Algorithm_String, Algorithm_Components);
         Hermes.OID.To_Object_Identifier
           (Separates => Algorithm_Components,
            Result    => Algorithm_OID,
            Status    => Algorithm_Status);

         Hash_String := PostgreSQL.Get_Value((I - 1), 3);
         Hermes.String_To_Octet_Array(Hash_String, Hash_Octets);

         Serial_Number := Serial_Number_Type'Value(PostgreSQL.Get_Value((I - 1), 4));
         Time_String := PostgreSQL.Get_Value((I - 1), 5);

         Result(I).Version          := Version;
         Result(I).Policy           := Policy_OID;
         Result(I).Hash_Algorithm   := Algorithm_OID;
         Result(I).Hashed_Message   := Hash_Octets;
         Result(I).Serial_Number    := Serial_Number;
         Result(I).Generalized_Time := Time_String;
      end loop;
      return Result;
   end Process_Tuples;


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
           Hash_String                   & "', "  &
           Serial_Number_Type'Image(Stamp.Serial_Number) & ", '" &
           Stamp.Generalized_Time        & "');");
   end Timestamp_Store;


   function Timestamp_Retrieve(Serial_Number : Serial_Number_Type) return Timestamp_Array is
   begin
      PostgreSQL.Execute_Query
        (Query => "SELECT version, "         &
                         "policy, "          &
                         "hash_algorithm, "  &
                         "hash, "            &
                         "serial_number, "   &
                         "generalized_time " &
                  "FROM  timestamp "         &
                  "WHERE serial_number = " & Serial_Number_Type'Image(Serial_Number) & ";");
      return Process_Tuples(PostgreSQL.Number_Of_Tuples);
   end Timestamp_Retrieve;


   function Timestamp_Retrieve(Start : Time; Stop : Time) return Timestamp_Array is
      Start_Time_String : String(1 .. 15);
      Stop_Time_String  : String(1 .. 15);
   begin
      Time_To_GeneralizedTime(Start, Start_Time_String);
      Time_To_GeneralizedTime(Stop, Stop_Time_String);
      PostgreSQL.Execute_Query
        (Query => "SELECT version, "         &
                         "policy, "          &
                         "hash_algorithm, "  &
                         "hash, "            &
                         "serial_number, "   &
                         "generalized_time " &
                  "FROM  timestamp "         &
                  "WHERE generalized_time BETWEEN '" & Start_Time_String & "' AND '" & Stop_Time_String & "';");
      return Process_Tuples(PostgreSQL.Number_Of_Tuples);
   end Timestamp_Retrieve;

end Data_Storage;
