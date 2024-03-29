---------------------------------------------------------------------------
-- FILE    : server_timestamp_maker.ads
-- SUBJECT : Specification of a package that encapsulates the work of creating a time stamp.
-- AUTHOR  : (C) Copyright 2015 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Messages;
with Serial_Generator;

package Server_Timestamp_Maker is

   procedure Create_Timestamp
     (Request_Message : in Messages.Message; Response_Message : out Messages.Message)
     with
       Global  => (In_Out => Serial_Generator.State),
       Depends => (Response_Message =>
                     (Request_Message, Serial_Generator.State),
                   Serial_Generator.State =>+ null);

end Server_Timestamp_Maker;
