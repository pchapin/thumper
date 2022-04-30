---------------------------------------------------------------------------
-- FILE    : thumper_server.adb
-- SUBJECT : Main procedure of the Thumper server.
-- AUTHOR  : (C) Copyright 2015 by Peter Chapin
--
-- This procedure initializes various global items and then calls the SPARK procedure
-- Service_Clients.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Cryptographic_Services;
with Data_Storage;
with Network.Addresses;
with Network.Socket;
--with Remote_Access;
with Server_SPARK_Boundary;
with Thumper_Switches;

--with PostgreSQL;
with Ada.Calendar;      use Ada.Calendar;
with Ada.Calendar.Formatting;    use Ada.Calendar.Formatting; 
with Hermes.OID;
with Timestamp_Messages;

use Ada.Exceptions;
use Ada.Strings.Unbounded;
use Thumper_Switches;

procedure Thumper_Server is
   use type Cryptographic_Services.Status_Type;

   Command_Line_Okay : Boolean;
   Error_Message : Unbounded_String;
   Crypto_Status : Cryptographic_Services.Status_Type;


   ray : Data_Storage.Timestamp_Array(1 .. 1);
   ray2 : Data_Storage.Timestamp_Array(1..2);
   start : Time;
   stop : Time := Clock;
   ts   : Timestamp_Messages.Timestamp;


begin
   -- Be sure the command line makes sense.
   Thumper_Switches.Validate(Thumper_Switches.Server, Command_Line_Okay, Error_Message);
   if not Command_Line_Okay then
      Ada.Text_IO.Put_Line("*** Command line error: " & To_String(Error_Message));
      return;
   end if;

   -- Be sure the key is available.
   Cryptographic_Services.Initialize_Key(Crypto_Status);
   if Crypto_Status /= Cryptographic_Services.Success then
      Ada.Text_IO.Put_Line("*** Unable to intialize the cryptographic key");
      return;
   end if;

   -- Initialize the data storage. This procedure raises an exception if it fails.
   -- TODO: Handle the exception raised (or maybe change the procedure to return a status code).
   Data_Storage.Initialize;




   --Test for Data_Storage.Timestamp_Count
   Ada.Text_IO.Put_Line("Timestamp Count: " & Integer'Image(Data_Storage.Timestamp_Count));
   Ada.Text_IO.Put_Line("");

   --Test for Data_Storage.Timestamp_Retrieve
   Ada.Text_IO.Put_Line("Timestamp Retrieve for Serial Number: ");
   ray := Data_Storage.Timestamp_Retrieve(811368890754717258);

   start := Time_Of (Year => 2017, Month => 06, Day => 24, Hour => 03, Minute => 30, Second => 59, Sub_Second => 0.5, Leap_Second => False, Time_Zone => 0);
   Ada.Text_IO.Put_Line("Time Stop: " & Image(stop));
   Ada.Text_IO.Put_Line("Time Start: " & Image(start));

   Ada.Text_IO.Put_Line("");

   Ada.Text_IO.Put_Line("Timestamp Retrieve for Start - Stop time: ");
   ray2 := Data_Storage.Timestamp_Retrieve(Start => start, Stop => stop);

   Ada.Text_IO.Put_Line("");

   ts.Version := 1;
   ts.Policy := ray(1).Policy;
   ts.Hash_Algorithm := ray(1).Hash_Algorithm;
   ts.Serial_Number := 14980002230;
   ts.Generalized_Time := "201604291152302";

   Data_Storage.Timestamp_Store(ts);



   -- Initialize the remote access. This procedure raises an exception if it fails.
   -- TODO: Handle the exception raised (or maybe change the procedure to return a status code).
   --Remote_Access.Initialize;

   -- Set up the socket. This initializes the network streams (both input and output).
   Network.Socket.Create_And_Bind_Socket(Network.Addresses.Port_Type'Value(Get_Switch(Port)));

   -- Service_Clients never returns.
   -- TODO: Come up with a good way to cleanly shut the server down.
   Server_SPARK_Boundary.Service_Clients;

exception
   when Ex : Network.Socket.Network_Error =>
      Ada.Text_IO.Put_Line("*** Unable to initialize network: " & Exception_Message(Ex));
      --Remote_Access.Shutdown;
      Data_Storage.Shutdown;

end Thumper_Server;
