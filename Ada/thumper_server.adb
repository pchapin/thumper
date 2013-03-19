---------------------------------------------------------------------------
-- FILE    : thumper_server.adb
-- SUBJECT : Main procedure of the Thumper server.
-- AUTHOR  : (C) Copyright 2013 by Peter Chapin and John McCormick
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Cryptographic_Services;
with Messages;
with Network.Addresses;
with Network.Server_Socket;
with Serial_Generator;
with SPARK_IO;
with Timestamp_Maker;

use type Cryptographic_Services.Status_Type;
use type Network.Addresses.Status_Type;
use type Network.Server_Socket.Status_Type;
use type Serial_Generator.Status_Type;
use type Timestamp_Maker.Status_Type;

--# inherit Cryptographic_Services,
--#         Messages,
--#         Network,
--#         Network.Addresses,
--#         Network.Server_Socket,
--#         Serial_Generator,
--#         SPARK_IO,
--#         Timestamp_Maker;
--# main_program;
procedure Thumper_Server
--# global in     Network.Server_Socket.Input, Cryptographic_Services.Key;
--#           out Network.Server_Socket.Output, Serial_Generator.Current_Number;
--#        in out Network.Server_Socket.State, SPARK_IO.State, SPARK_IO.Inputs, SPARK_IO.Outputs;
--# derives Network.Server_Socket.State     from Cryptographic_Services.Key, Network.Server_Socket.State &
--#         Network.Server_Socket.Output    from Cryptographic_Services.Key, Network.Server_Socket.State, Network.Server_Socket.Input &
--#         Serial_Generator.Current_Number from SPARK_IO.State, SPARK_IO.Inputs &
--#         SPARK_IO.State                  from SPARK_IO.State &
--#         SPARK_IO.Inputs                 from SPARK_IO.Inputs &
--#         SPARK_IO.Outputs                from Cryptographic_Services.Key, Network.Server_Socket.State, Network.Server_Socket.Input, SPARK_IO.Outputs;
is
   Serial_Status    : Serial_Generator.Status_Type;
   Crypto_Status    : Cryptographic_Services.Status_Type;
   Network_Status   : Network.Server_Socket.Status_Type;
   Client_Address   : Network.Addresses.UDPv4;
   Request_Message  : Messages.Message;
   Request_Count    : Messages.Count_Type;
   Response_Message : Messages.Message;
   Response_Count   : Messages.Count_Type;
   Timestamp_Status : Timestamp_Maker.Status_Type;
begin

   -- Be sure the serial generator is working.
   Serial_Generator.Initialize(Serial_Status);
   if Serial_Status /= Serial_Generator.Success then
      SPARK_IO.Put_Line
        (File => SPARK_IO.Standard_Output,
         Item => "Unable to intialize the serial generator (no serial number file?)",
         Stop => 0);
   else
      -- Be sure the key is available.
      Cryptographic_Services.Validate_Key(Crypto_Status);
      if Crypto_Status /= Cryptographic_Services.Success then
         SPARK_IO.Put_Line
           (File => SPARK_IO.Standard_Output,
            Item => "Unable to intialize the cryptographic library (no private key?)",
            Stop => 0);
      else
         -- Create the socket. The port should be 318, but a value above 1024 allows for easier testing by non-root users.
         Network.Server_Socket.Create_Socket(1318, Network_Status);
         if Network_Status /= Network.Server_Socket.Success then
            SPARK_IO.Put_Line
              (File => SPARK_IO.Standard_Output,
               Item => "Unable to create the server socket. Aborting!",
               Stop => 0);
         else
            -- Service clients infinitely (or maybe I need a way to cleanly shut the server down?).
            loop
               Network.Server_Socket.Receive(Request_Message, Request_Count, Client_Address, Network_Status);

               -- Ignore bad receives (should we log them?)
               if Network_Status = Network.Server_Socket.Success then
                  SPARK_IO.Put_Line
                    (File => SPARK_IO.Standard_Output,
                     Item => "Handling a message from a client...",
                     Stop => 0);
                  Timestamp_Maker.Create_Timestamp
                    (Request_Message, Request_Count, Response_Message, Response_Count, Timestamp_Status);

                  -- Ignore bad time-stamp creation operations (should we log them?)
                  if Timestamp_Status = Timestamp_Maker.Success then
                     Network.Server_Socket.Send(Client_Address, Response_Message, Response_Count);
                  end if;
               end if;
            end loop;
         end if;
      end if;
   end if;
end Thumper_Server;
