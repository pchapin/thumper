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
      High_Level : Message := (Data => (others => 0), Size => Low_Level.Size);
   begin
      for I in Index_Type'First .. Low_Level.Size loop
         High_Level.Data(I) := Hermes.Octet(Low_Level.Data(I));
      end loop;
      return High_Level;
   end From_Network;


   function To_Network(High_Level : Message) return Network_Message is
      Low_Level : Network_Message := (Data => (others => 0), Size => High_Level.Size);
   begin
      for I in Index_Type'First .. High_Level.Size loop
         Low_Level.Data(I) := Network.Octet(High_Level.Data(I));
      end loop;
      return Low_Level;
   end To_Network;

end Messages;
