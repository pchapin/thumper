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
with Server_Logger;
with Ada.Text_IO;
--with Hermes;

use Server_Logger;


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


   function Timestamp_Count return Count_Type is
   begin
      PostgreSQL.Clear_Result;
      PostgreSQL.Execute_Query (Query => "SELECT COUNT(Timestamp_Count) FROM thumper_table;");
      return Count_Type'Value(PostgreSQL.Get_Value(0,0));
   end Timestamp_Count;


   procedure Timestamp_Store(Stamp : in Timestamp) is
   begin
      raise Program_Error with "Data_Storage.Timestamp_Store not implemented";
   end Timestamp_Store;


   function Timestamp_Retrieve(Serial_Number : Serial_Number_Type) return Timestamp_Array is
      Tuples    : Count_Type;
   begin
      PostgreSQL.Clear_Result;
      PostgreSQL.Execute_Query (Query => "SELECT Policy, Hash_Algorithm, Hash_Message, Serial_Number, Generalized_Time FROM thumper_table WHERE Serial_Number = " & Serial_Number_Type'Image(Serial_Number) & ";");
      --PostgreSQL.Execute_Query (Query => "SELECT COUNT(Timestamp_Count), Policy, Hash_Algorithm, Hash_Message, Serial_Number, Generalized_Time FROM thumper_table WHERE Serial_Number = " & Serial_Number_Type'Image(Serial_Number) & "GROUP BY Policy, Hash_Algorithm, Hash_Message, Serial_Number, Generalized_Time;");
      Tuples := PostgreSQL.Number_Of_Tuples;

      declare
         Result   : Timestamp_Array(1 .. Tuples);
         Spolicy  : String(1..128);                -- Policy
         Salgo    : String(1..128);                -- Hash_Algorithm
         Smess    : String(1..256);                -- Hash_Message
         Snum     : Serial_Number_Type;            -- Serial_Number
         Stime    : String(1..15);                 -- Generalized_Time
         
      begin
         if Tuples = 1 then

            Spolicy := PostgreSQL.Get_Value (0, 0);
               Ada.Text_IO.Put_Line (Item => "Policy: " & Spolicy);
               Ada.Text_IO.Put_Line("");
            Salgo := PostgreSQL.Get_Value (0, 1);
               Ada.Text_IO.Put_Line (Item => "Hash Algorithm: " & Salgo);
            Smess := PostgreSQL.Get_Value (0, 2);
               Ada.Text_IO.Put_Line (Item => "Hashed Message: " & Smess);
               Ada.Text_IO.Put_Line("");
            Snum := Serial_Number_Type'Value(PostgreSQL.Get_Value (0, 3));
               Ada.Text_IO.Put_Line (Item => "Serial Number: " & Serial_Number_Type'Image(Snum));
               Ada.Text_IO.Put_Line("");
            Stime := PostgreSQL.Get_Value (0, 4);
               Ada.Text_IO.Put_Line (Item => "Generalized Time: " & Stime);
               Ada.Text_IO.Put_Line("");

         --Result(1).Policy           := Spolicy;
         --Result(1).Hash_Algorithm   := Salgo;
         --Result(1).Hashed_Message   := Smess;
            Result(1).Serial_Number    := Snum;
            Result(1).Generalized_Time := Stime;
         else
            raise Program_Error with "Invalid Serial Number: not assigned or has duplicates.";
         end if;
         return Result;
      end;
   end Timestamp_Retrieve;


   function Timestamp_Retrieve(Start : Time; Stop : Time) return Timestamp_Array is
      pragma Unreferenced(Start, Stop);   -- For now to cut down on warnings.
      Result : Timestamp_Array(1 .. PostgreSQL.Number_Of_Tuples);
      Tuples : Count_Type;
   begin
      --PostgreSQL.Execute_Query (Query => "SELECT Policy, Hash_Algorithm, Hash_Message, Serial_Number, Generalized_Time FROM thumper_table WHERE Generalized_Time >= " & Time'Image(Start) & " AND Generalized_Time =< " & Time'Image(Stop) & ";");
      
      Tuples := PostgreSQL.Number_Of_Tuples;
      
      --for I in 0 .. Tuples loop
         --   declare
         --      stime : String(1..15); 
         --   begin
         --      stime := PostgreSQL.Get_Value (I, 4);
         --      Result(I).Generalized_Time := stime;
         --   end;
      --end loop;
      return Result;
   end Timestamp_Retrieve;

end Data_Storage;
