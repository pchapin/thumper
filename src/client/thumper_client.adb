---------------------------------------------------------------------------
-- FILE    : thumper_client.adb
-- SUBJECT : Main procedure of the Thumper client.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Text_IO;
with Messages;
with Network.Addresses;
with Network.Socket;
with Network.Socket.Writer;

use Network.Socket;

procedure Thumper_Client is
   use type Network.Addresses.Status_Type;
   use type Writer.Status_Type;

   procedure Make_Request is
      Local_Host      : Network.Addresses.IPv4;
      Request_Message : Messages.Network_Message;
      Network_Status  : Network.Socket.Writer.Status_Type;
      Address_Status  : Network.Addresses.Status_Type;
   begin
      Request_Message := (Data => (others => 0), Size => 0);
      Network.Addresses.To_IPv4_Address("127.0.0.1", Local_Host, Address_Status);
      if Address_Status /= Network.Addresses.Success then
         Ada.Text_IO.Put_Line("Failed to convert target address to binary form!");
      else
         Request_Message.Data(Messages.Index_Type'First) := Character'Pos('X');
         Writer.Send
           (Request_Message, Network.Addresses.To_UDPv4_Address(Local_Host, 4318), Network_Status);
         if Network_Status /= Writer.Success then
            Ada.Text_IO.Put_Line("Failed to send request message!");
         end if;
      end if;
   end Make_Request;

begin
   Network.Socket.Create_Socket;
   Make_Request;
end Thumper_Client;
