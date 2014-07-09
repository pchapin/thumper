---------------------------------------------------------------------------
-- FILE    : messages.adb
-- SUBJECT : Body of a package that defines the basic message type exchanged.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Messages is

   function From_Network(Low_Level : Network_Message) return Message is
      Result : Message;
   begin
      for I in Low_Level'Range loop
         Result(I) := ASN1.Octet(Low_Level(I));
      end loop;
      return Result;
   end From_Network;

   function To_Network(High_Level : Message) return Network_Message is
      Result : Network_Message;
   begin
      for I in High_Level'Range loop
         Result(I) := Network.Octet(High_Level(I));
      end loop;
      return Result;
   end To_Network;

end Messages;
