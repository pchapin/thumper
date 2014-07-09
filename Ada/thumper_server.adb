---------------------------------------------------------------------------
-- FILE    : thumper_server.adb
-- SUBJECT : Main procedure of the Thumper server.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Cryptographic_Services;
with Messages;
with Network.Addresses;
with Network.Socket;
with Serial_Generator;
with Timestamp_Maker;
with Wrapper_IO;

use type Cryptographic_Services.Status_Type;
use type Network.Addresses.Status_Type;
use type Network.Socket.Status_Type;
use type Serial_Generator.Status_Type;
use type Timestamp_Maker.Status_Type;

procedure Thumper_Server
  with
    Global => (In_Out => (Wrapper_IO.IO_Subsystem, Network.Socket.State, Network.Socket.Network_Stack),
               Output => (Cryptographic_Services.Key, Serial_Generator.Number))
is

   procedure Service_Clients
     with
       Global => (Input  => (Cryptographic_Services.Key, Serial_Generator.Number, Network.Socket.State),
                  In_Out => (Wrapper_IO.IO_Subsystem, Network.Socket.Network_Stack))
   is
      Client_Address   : Network.Addresses.UDPv4;

      Network_Request  : Messages.Network_Message;
      Request_Message  : Messages.Message;
      Request_Count    : Messages.Count_Type;

      Network_Response : Messages.Network_Message;
      Response_Message : Messages.Message;
      Response_Count   : Messages.Count_Type;

      Network_Status   : Network.Socket.Status_Type;
      Timestamp_Status : Timestamp_Maker.Status_Type;
   begin
      -- Service clients infinitely (or maybe I need a way to cleanly shut the server down?).
      loop
         Network.Socket.Receive(Network_Request, Request_Count, Client_Address, Network_Status);

         -- Ignore bad receives (Should we log them? Right now it's easy to get in an infinite loop here)
         -- TODO: What happens if Standard_Output enters an error state? Right now the preconditions on Put_Line might fail.
         if Network_Status /= Network.Socket.Success then
            Wrapper_IO.Put_Line("Receive from socket failed!");
         else
            Wrapper_IO.Put_Line("Handling a message from a client...");
            Request_Message := Messages.From_Network(Network_Request);
            Timestamp_Maker.Create_Timestamp(Request_Message, Request_Count, Response_Message, Response_Count, Timestamp_Status);

            -- Ignore bad time-stamp creation operations (should we log them?)
            if Timestamp_Status = Timestamp_Maker.Success then
               Network_Response := Messages.To_Network(Response_Message);
               Network.Socket.Send(Client_Address, Network_Response, Response_Count);
            end if;
         end if;
      end loop;
   end Service_Clients;

   Serial_Status  : Serial_Generator.Status_Type;
   Crypto_Status  : Cryptographic_Services.Status_Type;
   Network_Status : Network.Socket.Status_Type;
begin

   -- Do low level "from scratch" initializations.
   Serial_Generator.Initialize(Serial_Status);
   Cryptographic_Services.Initialize(Crypto_Status);

   -- Check initialization results.
   if Serial_Status /= Serial_Generator.Success then
      Wrapper_IO.Put_Line("Unable to intialize the serial generator! (no serial number file?)");
   else
      -- Be sure the key is available.
      if Crypto_Status /= Cryptographic_Services.Success then
         Wrapper_IO.Put_Line("Unable to intialize the cryptographic library! (no private key?)");
      else
         -- Create the socket. The port should be 318, but a value above 1024 allows for easier testing by non-root users.
         Network.Socket.Create_And_Bind_Socket(318, Network_Status);
         if Network_Status /= Network.Socket.Success then
            Wrapper_IO.Put_Line("Unable to create the server socket. Aborting!");
         else
            Service_Clients;
         end if;
      end if;
   end if;
end Thumper_Server;
