---------------------------------------------------------------------------
-- FILE    : timestamp_maker.adb
-- SUBJECT : Specification of a package that encapsulates the work of creating a time stamp.
-- AUTHOR  : (C) Copyright 2013 by Peter Chapin and John McCormick
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Cryptographic_Services;
with Messages;
with Serial_Generator;

--# inherit Cryptographic_Services, Messages, Serial_Generator;
package Timestamp_Maker is

   type Status_Type is (Success, Bad_Request);

   procedure Create_Timestamp
     (Request_Message  : in  Messages.Message;
      Request_Count    : in  Messages.Count_Type;
      Response_Message : out Messages.Message;
      Response_Count   : out Messages.Count_Type;
      Status           : out Status_Type);
   --# global in Cryptographic_Services.Key, Serial_Generator.Current_Number;
   --# derives Response_Message from Request_Message, Request_Count, Serial_Generator.Current_Number, Cryptographic_Services.Key &
   --#         Response_Count   from Request_Message, Request_Count, Serial_Generator.Current_Number &
   --#         Status           from Request_Message, Request_Count;

end Timestamp_Maker;
