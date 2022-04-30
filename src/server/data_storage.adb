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
with Ada.Strings;                         use Ada.Strings;
with Ada.Strings.Fixed;                   use Ada.Strings.Fixed;
with Ada.Strings.Maps;                    use Ada.Strings.Maps;
with Ada.Calendar.Formatting;             use Ada.Calendar.Formatting;


package body Data_Storage is

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


   -- Converts a String to a Component_Type (Hermes.OID)
   procedure StringToComponent(Text : in String; Num : in out Natural; Component: out Hermes.OID.Component_Type) is  
   begin
      Component := Hermes.OID.Component_Type'Value(Text);
      Num := Num + 1;
      -- Debug: Ada.Text_IO.Put_Line(Text);
   end StringToComponent;
   

   -- Converts an OID String (eg "1.5.77.3.6.3.9") into a Component Array (Hermes.OID) 
   procedure StringToArray(Text : in String; Result : out Hermes.OID.Component_Array) is
      num       : Natural := 0;         -- Component_Array index value
      component : Hermes.OID.Component_Type;
      alpha     : constant Character_Set := To_Set(Singleton => '.');
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
         StringToComponent(Text(lower..upper), num, component);
         Result(num) := component; 
         iter := upper + 1;
      end loop;
   end StringToArray;


   -- Converts a Time type into a Generalized Time String
   procedure TimeToGeneralizedTime(t : in Time; gt : out String) is
      Text      : constant String := Image(t);
      gti       : Positive := gt'First;            --gt iterator
      dash      : constant Character := '-';
      colon     : constant Character := ':';
      blank     : constant Character := ' ';
   begin
      for iter in Text'Range loop
         if Text(iter) /= dash and Text(iter) /= colon and Text(iter) /= blank then
            gt(gti) := Text(iter);
            gti := gti + 1;
         end if; 
      end loop;
      gt(gt'First + 14) := '0';
      -- Debug: Ada.Text_IO.Put_Line(gt);
   end TimeToGeneralizedTime;


   -- OID Component Arrays to OID String (eg "2.6.3.7.23.122.6")
   procedure OIDToString(Comp : in Hermes.OID.Component_Array; Str : out String) is
      SIndex   : Natural := Str'First;
      len      : Natural;
      blank    : constant Character := ' ';
   begin
      for I in Comp'Range loop
         declare
            ct  : constant Hermes.OID.Component_Type := Comp(I); 
         begin
            len := 0;
            for K in ct'Image'Range loop
               len := len + 1;
            end loop;
         end;
         declare
            num : String(1..len);
         begin
            num := Comp(I)'Image;
            for J in num'Range loop
               if num(J) /= blank then
                  Str(SIndex) := num(J);
                  SIndex := SIndex + 1;
               end if;
            end loop;
         end; 
         if I = Comp'Last then
            for L in SIndex..Str'Last loop
               Str(L) := ' ';
            end loop;
            exit;
         end if;
         Str(SIndex) := '.';
         SIndex := SIndex + 1;
      end loop;
      Ada.Text_IO.Put_Line (Str);
   end OIDToString;


   function Timestamp_Count return Count_Type is
   begin
      PostgreSQL.Clear_Result;
      PostgreSQL.Execute_Query (Query => "SELECT COUNT(Timestamp_Count) FROM ThumperTable;");
      return Count_Type'Value(PostgreSQL.Get_Value(0,0));
   end Timestamp_Count;


   -- TODO: ThumperTable also stores IP addresses, Timestamp_Store only currently stores Policy, Hash_Algorithm, Serial_Number, and Generalized_Time 
   procedure Timestamp_Store(Stamp : in Timestamp) is
      OIDpolicy   : constant Hermes.OID.Object_Identifier   := Stamp.Policy;
      Cpolicy     : Hermes.OID.Component_Array(1..Hermes.OID.Maximum_Component_Count);
      Npolicy     : Hermes.OID.Component_Count_Type;
      Spolicy     : String(1..128);         
      OIDalgo     : constant Hermes.OID.Object_Identifier            := Stamp.Hash_Algorithm;
      Calgo       : Hermes.OID.Component_Array(1..Hermes.OID.Maximum_Component_Count);
      Nalgo       : Hermes.OID.Component_Count_Type;
      Salgo       : String(1..128);        
      -- TODO: Ohash       : Hermes.Octet_Array                      := Stamp.Hashed_Message;
      Serial      : constant Serial_Number_Type                      := Stamp.Serial_Number;
      GTime       : constant String(1..15)                           := Stamp.Generalized_Time;
   begin
      PostgreSQL.Clear_Result;
      Hermes.OID.To_Separates(OIDpolicy, Cpolicy, Npolicy);
      OIDToString (Cpolicy, Spolicy);
      Hermes.OID.To_Separates(OIDalgo, Calgo, Nalgo);
      OIDToString (Calgo, Salgo);
      -- TODO: Query still needs Hashed_Message 
      PostgreSQL.Execute_Query(Query => "INSERT INTO ThumperTable (Policy, Hash_Algorithm, Serial_Number, Generalized_Time) VALUES ( '" & Spolicy & "', '" & Salgo & "', " & Serial_Number_Type'Image(Serial) & ", '" & GTime & "');");
   end Timestamp_Store;


   function Timestamp_Retrieve(Serial_Number : Serial_Number_Type) return Timestamp_Array is
      Tuples         : Count_Type;
   begin
      PostgreSQL.Clear_Result;
      PostgreSQL.Execute_Query(Query => "SELECT Policy, Hash_Algorithm, Hash_Message, Serial_Number, Generalized_Time FROM ThumperTable WHERE Serial_Number = " & Serial_Number_Type'Image(Serial_Number) & ";");
      Tuples := PostgreSQL.Number_Of_Tuples;
      declare
         Result      : Timestamp_Array(1 .. Tuples);
         Spolicy     : String(1..128);                            -- Policy                 
         Cpolicy     : Hermes.OID.Component_Array(1..Hermes.OID.Maximum_Component_Count);                
         OIDpolicy   : Hermes.OID.Object_Identifier;
         Stpolicy    : Hermes.OID.Status_Type;
         Salgo       : String(1..128);                            -- Hash_Algorithm
         Calgo       : Hermes.OID.Component_Array(1..Hermes.OID.Maximum_Component_Count);
         OIDalgo     : Hermes.OID.Object_Identifier;
         Stalgo      : Hermes.OID.Status_Type;
         Smess       : String(1..256);                            -- Hash_Message
         Snum        : Serial_Number_Type;                        -- Serial_Number
         Stime       : String(1..15);                             -- Generalized_Time    
      begin
         if Tuples = 1 then
            Spolicy := PostgreSQL.Get_Value (0, 0);
                  Ada.Text_IO.Put_Line (Item => "Policy: " & Spolicy);
               StringToArray (Spolicy, Cpolicy);
               Hermes.OID.To_Object_Identifier(Separates => Cpolicy, Result => OIDpolicy, Status => Stpolicy);
                  Ada.Text_IO.Put_Line (Item => "OID Status: " & Hermes.OID.Status_Type'Image(Stpolicy));
                  Ada.Text_IO.Put_Line("");
            Salgo := PostgreSQL.Get_Value (0, 1);
                  Ada.Text_IO.Put_Line (Item => "Hash Algorithm: " & Salgo);
               StringToArray(Salgo, Calgo);
               Hermes.OID.To_Object_Identifier(Separates => Calgo, Result => OIDalgo, Status => Stalgo);
                  Ada.Text_IO.Put_Line ("OID Status: " & Hermes.OID.Status_Type'Image(Stalgo));
                  Ada.Text_IO.Put_Line("");
            Smess := PostgreSQL.Get_Value (0, 2);
                  Ada.Text_IO.Put_Line (Item => "Hashed Message: " & Smess);
                  Ada.Text_IO.Put_Line("");
            Snum := Serial_Number_Type'Value(PostgreSQL.Get_Value (0, 3));
                  Ada.Text_IO.Put_Line (Item => "Serial Number: " & Serial_Number_Type'Image(Snum));
                  Ada.Text_IO.Put_Line("");
            Stime := PostgreSQL.Get_Value (0, 4);
                  Ada.Text_IO.Put_Line (Item => "Generalized Time: " & Stime);
                  Ada.Text_IO.Put_Line("");
            Result(1).Policy           := OIDpolicy;
            Result(1).Hash_Algorithm   := OIDalgo;
         -- TODO: Result(1).Hashed_Message   := Smess;  need to convert string of bits into octects 
            Result(1).Serial_Number    := Snum;
            Result(1).Generalized_Time := Stime;
         else
            raise Program_Error with "Invalid Serial Number: not assigned or has duplicates.";
         end if;
         return Result;
      end;
   end Timestamp_Retrieve;


   function Timestamp_Retrieve(Start : Time; Stop : Time) return Timestamp_Array is
      Tuples   : Count_Type;
      StartS   : String(1..15);
      StopS    : String(1..15);
   begin
      PostgreSQL.Clear_Result;
      TimeToGeneralizedTime (Start, StartS);
      TimeToGeneralizedTime (Stop, StopS);
      PostgreSQL.Execute_Query(Query => "SELECT Policy, Hash_Algorithm, Hash_Message, Serial_Number, Generalized_Time FROM ThumperTable WHERE Generalized_Time BETWEEN '" & StartS & "' AND '" & StopS & "';");
      Tuples := PostgreSQL.Number_Of_Tuples;
      declare
         Result   : Timestamp_Array(1 .. Tuples);
      begin
         for I in 1 .. Tuples loop
            declare
               Spolicy     : String(1..128);                            -- Policy                 
               Cpolicy     : Hermes.OID.Component_Array(1..Hermes.OID.Maximum_Component_Count);                
               OIDpolicy   : Hermes.OID.Object_Identifier;
               Stpolicy    : Hermes.OID.Status_Type;
               Salgo       : String(1..128);                            -- Hash_Algorithm
               Calgo       : Hermes.OID.Component_Array(1..Hermes.OID.Maximum_Component_Count);
               OIDalgo     : Hermes.OID.Object_Identifier;
               Stalgo      : Hermes.OID.Status_Type;
               Smess       : String(1..256);                            -- Hash_Message
               Snum        : Serial_Number_Type;                        -- Serial_Number
               Stime       : String(1..15);                             -- Generalized_Time   
            begin
               Spolicy := PostgreSQL.Get_Value ((I - 1), 0);
                  Ada.Text_IO.Put_Line (Item => "Policy: " & Spolicy);
               StringToArray (Spolicy, Cpolicy);
               Hermes.OID.To_Object_Identifier(Separates => Cpolicy, Result => OIDpolicy, Status => Stpolicy);
                  Ada.Text_IO.Put_Line (Item => "OID Status: " & Hermes.OID.Status_Type'Image(Stpolicy));
                  Ada.Text_IO.Put_Line("");
               Salgo := PostgreSQL.Get_Value ((I - 1), 1);
                  Ada.Text_IO.Put_Line (Item => "Hash Algorithm: " & Salgo);
               StringToArray(Salgo, Calgo);
               Hermes.OID.To_Object_Identifier(Separates => Calgo, Result => OIDalgo, Status => Stalgo);
                  Ada.Text_IO.Put_Line ("OID Status: " & Hermes.OID.Status_Type'Image(Stalgo));
                  Ada.Text_IO.Put_Line("");
               Smess := PostgreSQL.Get_Value ((I - 1), 2);
                  Ada.Text_IO.Put_Line (Item => "Hashed Message: " & Smess);
                  Ada.Text_IO.Put_Line("");
               Snum := Serial_Number_Type'Value(PostgreSQL.Get_Value ((I - 1), 3));
                  Ada.Text_IO.Put_Line (Item => "Serial Number: " & Serial_Number_Type'Image(Snum));
                  Ada.Text_IO.Put_Line("");
               Stime := PostgreSQL.Get_Value (Tuple_Number => (I - 1), Field_Number => 4);
                  Ada.Text_IO.Put_Line (Item => "Generalized Time: " & Stime);
                  Ada.Text_IO.Put_Line("");
               Result(I).Policy           := OIDpolicy;
               Result(I).Hash_Algorithm   := OIDalgo;
               -- TODO: Result(1).Hashed_Message   := Smess;  need to convert string of bits into octects 
               Result(I).Serial_Number    := Snum;
               Result(I).Generalized_Time := Stime;       
            end;
         end loop;
      return Result;
      end;
   end Timestamp_Retrieve;

end Data_Storage;
