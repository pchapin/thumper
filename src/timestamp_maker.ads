---------------------------------------------------------------------------
-- FILE    : timestamp_maker.adb
-- SUBJECT : Specification of a package that encapsulates the work of creating a time stamp.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Cryptographic_Services;
with Messages;
with Serial_Generator;

package Timestamp_Maker is

   procedure Create_Timestamp
     (Request_Message : in Messages.Message; Response_Message : out Messages.Message)
     with
       Global  => (Input => Cryptographic_Services.Key,
                   In_Out => Serial_Generator.State),
       Depends => (Response_Message =>
                     (Request_Message, Cryptographic_Services.Key, Serial_Generator.State),
                   Serial_Generator.State =>+ null);

end Timestamp_Maker;
