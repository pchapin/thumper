---------------------------------------------------------------------------
-- FILE    : service_clients.adb
-- SUBJECT : Main loop that reads time stamp requests and issues time stamp responses.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Note that the SPARK tools (currently) think this is a possible main procedure and thus
-- produce somewhat odd and misleading messages about it. My attempt to justify some of the
-- messages using pragma Annotate is not working as expected.
--
-- The unusual data and flow dependency contracts arise because Writer.Output_Message_Stream
-- is not written on every path (think about the case when Receive fails every time). Thus
-- technically Writer.Output_Message_Stream needs to have mode In_Out, etc.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Text_IO;

with Cryptographic_Services;
with Messages;
with Network.Addresses;
with Network.Socket.Reader;
with Network.Socket.Writer;
with Serial_Generator;
with Timestamp_Maker;

use Network.Socket;

procedure Service_Clients
  with
    Global => (Input  => (Reader.Input_Message_Stream, Cryptographic_Services.Key),
               In_Out => (Writer.Output_Message_Stream, Serial_Generator.State)),
    Depends =>
      (Writer.Output_Message_Stream =>+
         (Reader.Input_Message_Stream, Cryptographic_Services.Key, Serial_Generator.State),
       Serial_Generator.State =>+ Reader.Input_Message_Stream)
is
   -- These annotations don't appear to work; I'm not sure why not.
   pragma Annotate(GNATprove, False_Positive,
      """Cryptographic_Services.Key"" might not be initialized",
      "Initialized in main program by Cryptographic_Services.Initialize");
   pragma Annotate(GNATprove, False_Positive,
      """Writer.Output_Message_Stream"" might not be initialized",
      "Initialized in main program Network.Socket.Create_And_Bind_Socket");

   use type Reader.Status_Type;
   use type Writer.Status_Type;

   Client_Address   : Network.Addresses.UDPv4;

   Network_Request  : Messages.Network_Message;  -- Low level request.
   Request_Message  : Messages.Message;          -- Converted to Hermes.Octets.
   Read_Status      : Reader.Status_Type;

   Response_Message : Messages.Message;          -- High level request.
   Network_Response : Messages.Network_Message;  -- Converted to Network.Octets.
   Write_Status     : Writer.Status_Type;
begin
   -- In the future the program should probably have a proper way to log failure messages.
   pragma Warnings(Off, "no Global contract available for ""Put_Line""");

   -- Service clients infinitely (or maybe I need a way to cleanly shut the server down?).
   loop
      Reader.Receive(Message => Network_Request, From => Client_Address, Status => Read_Status);
      if Read_Status /= Reader.Success then
         Ada.Text_IO.Put_Line("*** Failure reading request message!");
      else
         Request_Message := Messages.From_Network(Network_Request);
         Timestamp_Maker.Create_Timestamp(Request_Message, Response_Message);
         Network_Response := Messages.To_Network(Response_Message);

         Writer.Send(Message => Network_Response, To => Client_Address, Status => Write_Status);
         if Write_Status /= Writer.Success then
            Ada.Text_IO.Put_Line("*** Failure sending reply message!");
         end if;
      end if;
   end loop;
end Service_Clients;

