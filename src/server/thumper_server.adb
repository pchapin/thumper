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
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Exceptions;
with Ada.Text_IO;

with Cryptographic_Services;
with Data_Storage;
with Network.Socket;
with Remote_Access;
with SPARK_Boundary;

use Ada.Exceptions;

procedure Thumper_Server is
   use type Cryptographic_Services.Status_Type;

   Crypto_Status : Cryptographic_Services.Status_Type;
begin
   -- Initialize the data storage. This procedure raises an exception if it fails.
   -- TODO: Handle the exception raised (or maybe change the procedure to return a status code).
   Data_Storage.Initialize;

   -- Initialize the remote access. This procedure raises an exception if it fails.
   -- TODO: Handle the exception raised (or maybe change the procedure to return a status code).
   Remote_Access.Initialize;

   -- Be sure the key is available.
   Cryptographic_Services.Initialize_Key(Crypto_Status);
   if Crypto_Status /= Cryptographic_Services.Success then
      Ada.Text_IO.Put_Line("*** Unable to intialize the cryptographic key");
   else
      -- Set up the socket. This initializes the network streams (both input and output).
      Network.Socket.Create_And_Bind_Socket(4318);

      -- Service_Clients never returns.
      -- TODO: Come up with a good way to cleanly shut the server down.
      SPARK_Boundary.Service_Clients;
   end if;

exception
   when Ex : Network.Socket.Network_Error =>
      Ada.Text_IO.Put_Line("*** Unable to initialize network: " & Exception_Message(Ex));
end Thumper_Server;
