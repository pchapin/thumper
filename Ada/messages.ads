---------------------------------------------------------------------------
-- FILE    : messages.ads
-- SUBJECT : Specification of a package that defines the basic message type exchanged.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Hermes;
with Network;

package Messages is

   subtype Index_Type is Positive range 1 .. 512;
   subtype Count_Type is Natural  range 0 .. Index_Type'Last;

   -- Type Network_Message represents the raw data sent/received on the network.
   subtype Network_Message is Network.Octet_Array(Index_Type);

   -- Type Message represents the ASN.1 data. In theory it is more abstract than the raw data.
   subtype Message is Hermes.Octet_Array(Index_Type);

   -- Currently these functions don't actually do any processing but in general (or eventually) they might.
   function From_Network(Low_Level : Network_Message) return Message;
   function To_Network(High_Level : Message) return Network_Message;

end Messages;
