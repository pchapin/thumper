---------------------------------------------------------------------------
-- FILE    : timestamp_maker.adb
-- SUBJECT : Body of a package that encapsulates the work of creating a time stamp.
-- AUTHOR  : (C) Copyright 2013 by Peter Chapin and John McCormick
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package body Timestamp_Maker is

   function Valid_Request(Request_Message : in Messages.Message; Request_Count : in Messages.Count_Type) return Boolean is
   begin
      return False;
   end Valid_Request;


   procedure Create_Timestamp
     (Request_Message  : in  Messages.Message;
      Request_Count    : in  Messages.Count_Type;
      Response_Message : out Messages.Message;
      Response_Count   : out Messages.Count_Type;
      Status           : out Status_Type) is
   begin
      Response_Message := Messages.Message'(others => 0);
      Response_Count := 0;
      Status := Success;

      if not Valid_Request(Request_Message, Request_Count) then
         Status := Bad_Request;
      end if;
   end Create_Timestamp;


end Timestamp_Maker;
