---------------------------------------------------------------------------
-- FILE    : thumper_server.adb
-- SUBJECT : Main procedure of the Thumper server.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Text_IO;
with Cryptographic_Services;
with Messages;
with Network.Addresses;
with Network.Socket;
with Serial_Generator;
with Timestamp_Maker;

use type Cryptographic_Services.Status_Type;
use type Network.Addresses.Status_Type;
use type Network.Socket.Status_Type;
use type Serial_Generator.Status_Type;
use type Timestamp_Maker.Status_Type;

procedure Thumper_Server is

   procedure Service_Clients is
      Client_Address   : Network.Addresses.UDPv4;
      Request_Message  : Messages.Message;
      Request_Count    : Messages.Count_Type;
      Response_Message : Messages.Message;
      Response_Count   : Messages.Count_Type;

      Network_Status   : Network.Socket.Status_Type;
      Timestamp_Status : Timestamp_Maker.Status_Type;
   begin
      -- Service clients infinitely (or maybe I need a way to cleanly shut the server down?).
      loop
         Network.Socket.Receive(Request_Message, Request_Count, Client_Address, Network_Status);

         -- Ignore bad receives (Should we log them? Right now it's easy to get in an infinite loop here)
         if Network_Status /= Network.Socket.Success then
            Ada.Text_IO.Put_Line("Receive from socket failed!");
         else
            Ada.Text_IO.Put_Line("Handling a message from a client...");
            Timestamp_Maker.Create_Timestamp
              (Request_Message, Request_Count, Response_Message, Response_Count, Timestamp_Status);

            -- Ignore bad time-stamp creation operations (should we log them?)
            if Timestamp_Status = Timestamp_Maker.Success then
               Network.Socket.Send(Client_Address, Response_Message, Response_Count);
            end if;
         end if;
      end loop;
   end Service_Clients;

   Serial_Status  : Serial_Generator.Status_Type;
   Crypto_Status  : Cryptographic_Services.Status_Type;
   Network_Status : Network.Socket.Status_Type;
begin

   -- Be sure the serial generator is working.
   Serial_Generator.Initialize(Serial_Status);
   if Serial_Status /= Serial_Generator.Success then
      Ada.Text_IO.Put_Line("Unable to intialize the serial generator! (no serial number file?)");
   else
      -- Be sure the key is available.
      Cryptographic_Services.Initialize(Crypto_Status);
      if Crypto_Status /= Cryptographic_Services.Success then
         Ada.Text_IO.Put_Line("Unable to intialize the cryptographic library! (no private key?)");
      else
         -- Create the socket. The port should be 318, but a value above 1024 allows for easier testing by non-root users.
         Network.Socket.Create_And_Bind_Socket(318, Network_Status);
         if Network_Status /= Network.Socket.Success then
            Ada.Text_IO.Put_Line("Unable to create the server socket. Aborting!");
         else
            Service_Clients;
         end if;
      end if;
   end if;
end Thumper_Server;
