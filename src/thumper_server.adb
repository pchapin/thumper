---------------------------------------------------------------------------
-- FILE    : thumper_server.adb
-- SUBJECT : Main procedure of the Thumper server.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Exceptions;
with Ada.Text_IO;

with Cryptographic_Services;
with Messages;
with Network.Addresses;
with Network.Socket;
with Serial_Generator;
with Timestamp_Maker;

use Ada.Exceptions;

use type Cryptographic_Services.Status_Type;
use type Network.Addresses.Status_Type;

procedure Thumper_Server is

   procedure Service_Clients is
      Client_Address   : Network.Addresses.UDPv4;

      Network_Request  : Messages.Network_Message;
      Request_Message  : Messages.Message;
      Request_Count    : Messages.Count_Type;

      Network_Response : Messages.Network_Message;
      Response_Message : Messages.Message;
      Response_Count   : Messages.Count_Type;
   begin
      -- Service clients infinitely (or maybe I need a way to cleanly shut the server down?).
      loop
         begin
            Network.Socket.Receive(Network_Request, Request_Count, Client_Address);
            Ada.Text_IO.Put_Line("Handling a message from a client...");
            Request_Message := Messages.From_Network(Network_Request);
            Timestamp_Maker.Create_Timestamp
              (Request_Message, Request_Count, Response_Message, Response_Count);
            Network_Response := Messages.To_Network(Response_Message);
            Network.Socket.Send(Client_Address, Network_Response, Response_Count);
         exception
            when Ex : Network.Socket.Network_Error =>
               -- Should these errors be logged?
               Ada.Text_IO.Put_Line("*** Network Error: " & Exception_Message(Ex));
            when Ex : others =>
               -- Note: Create_Timestamp should never raise an exception.
               -- Should these errors be logged?
               Ada.Text_IO.Put_Line
                 ("*** Unexpected: " & Exception_Name(Ex) & ": " & Exception_Message(Ex));
         end;
      end loop;
   end Service_Clients;

   Crypto_Status  : Cryptographic_Services.Status_Type;
begin
   -- Be sure the key is available.
   Cryptographic_Services.Initialize(Crypto_Status);
   if Crypto_Status /= Cryptographic_Services.Success then
      Ada.Text_IO.Put_Line("*** Unable to intialize the cryptographic library!");
   else
      -- Create the socket. The port should be 318, but a value above 1024 allows for easier
      -- testing by non-root users.
      Network.Socket.Create_And_Bind_Socket(318);
      Service_Clients;
   end if;
exception
   when Ex : Network.Socket.Network_Error =>
      Ada.Text_IO.Put_Line("*** Network Error: " & Exception_Message(Ex));
end Thumper_Server;
