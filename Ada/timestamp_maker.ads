---------------------------------------------------------------------------
-- FILE    : timestamp_maker.adb
-- SUBJECT : Specification of a package that encapsulates the work of creating a time stamp.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with ASN1.BER;
with Cryptographic_Services;
with Messages;
with Network;
with Serial_Generator;

use type ASN1.BER.Status_Type;
use type Network.Octet;

package Timestamp_Maker is

   type Status_Type is (Success, Bad_Request);

   procedure Create_Timestamp
     (Request_Message  : in  Messages.Message;
      Request_Count    : in  Messages.Count_Type;
      Response_Message : out Messages.Message;
      Response_Count   : out Messages.Count_Type;
      Status           : out Status_Type)
   with
     Global  => (Input            => (Cryptographic_Services.Key, Serial_Generator.Number)),
     Depends => (Response_Message => (Request_Message, Request_Count, Serial_Generator.Number, Cryptographic_Services.Key),
                 Response_Count   => (Request_Message, Request_Count, Serial_Generator.Number),
                 Status           => (Request_Message, Request_Count));

end Timestamp_Maker;
