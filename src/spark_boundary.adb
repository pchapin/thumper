pragma SPARK_Mode(On);

with Ada.Text_IO;

with Messages;
with Network.Addresses;
with Timestamp_Maker;

package body SPARK_Boundary is

   procedure Service_Clients is
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

      -- Service clients infinitely.
      -- TODO: Come up with a way to cleanly shut the server down.
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

end SPARK_Boundary;
