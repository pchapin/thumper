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
      PostgreSQL.Execute_Query (Query => "SELECT COUNT(Timestamp_Count) FROM thumper_table;");
      return Count_Type'Value(PostgreSQL.Get_Value(0,0));
   end Timestamp_Count;


   procedure Timestamp_Store(Stamp : in Timestamp) is
   begin
      raise Program_Error with "Data_Storage.Timestamp_Store not implemented";
   end Timestamp_Store;


   function Timestamp_Retrieve(Serial_Number : Serial_Number_Type) return Timestamp_Array is
      --pragma Unreferenced(Serial_Number);   -- For now to cut down on warnings.
   begin
      PostgreSQL.Execute_Query (Query => "SELECT Policy, Hash_Algorithm, Hash_Message, Serial_Number, Generalized_Time FROM thumper_table WHERE Serial_Number = " & Serial_Number_Type'Image(Serial_Number) & ";");
      declare
         Result : Timestamp_Array(1 .. PostgreSQL.Number_Of_Tuples);
      begin
         for I in Result'Range loop
            declare
               stime : String := PostgreSQL.Get_Value (I, 7);
            begin
               Result(I).Generalized_Time := stime;
            end;
         end loop;
         return Result;
      end;
   end Timestamp_Retrieve;


   function Timestamp_Retrieve(Start : Time; Stop : Time) return Timestamp_Array is
      pragma Unreferenced(Start, Stop);   -- For now to cut down on warnings.
      Dummy : Timestamp_Array(1 .. 0);
   begin
      --PostgreSQL.Execute_Query (Query => "SELECT Policy, Hash_Algorithm, Hash_Message, Serial_Number, Generalized_Time FROM thumper_table WHERE Generalized_Time >= " & Time'Image(Start) & " AND Generalized_Time =< " & Time'Image(Stop) & ";");

      return Dummy;
   end Timestamp_Retrieve;

end Data_Storage;
