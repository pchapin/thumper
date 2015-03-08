---------------------------------------------------------------------------
-- FILE    : thumper_server.adb
-- SUBJECT : Main procedure of the Thumper server.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- This procedure initializes various global items and then calls the SPARK procedure
-- Service_Clients. In the future this main procedure may have other (non-SPARK) duties
-- such as setting up remote access via AWS or a GUI interface.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Exceptions;
with Ada.Text_IO;

with Cryptographic_Services;
with Network.Socket;
with SPARK_Boundary;

use Ada.Exceptions;

procedure Thumper_Server is
   use type Cryptographic_Services.Status_Type;

   Crypto_Status : Cryptographic_Services.Status_Type;
begin
   -- Be sure the key is available. This initializes Cryptographic_Services.Key
   Cryptographic_Services.Initialize(Crypto_Status);
   if Crypto_Status /= Cryptographic_Services.Success then
      Ada.Text_IO.Put_Line("*** Unable to intialize the cryptographic library: missing key?");
   else
      -- Set up the socket. This initializes the network streams (both input and output).
      Network.Socket.Create_And_Bind_Socket(4318);
      SPARK_Boundary.Service_Clients;
   end if;

exception
   when Ex : Network.Socket.Network_Error =>
      Ada.Text_IO.Put_Line("*** Unable to initialize network: " & Exception_Message(Ex));
end Thumper_Server;
