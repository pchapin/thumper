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

   type Imprint is
      record
         X : Integer; -- TODO: Replace with appropriate content.
      end record;

   -- Extracts the message imprint from the request. The meaning of the parameters are as follows.
   --   Request_Message : The full message array.
   --   Request_Count   : The number of bytes in Request_Message that are meaningful.
   --   Index           : The first position in Request_Message where the imprint starts.
   --   Message_Imprint : The imprint obtained from the message.
   --   Stop            : The last position of the imprint.
   --   Message_Status  : Indicates the success or failure of imprint extraction.
   --
   procedure Get_Message_Imprint
     (Request_Message : in  Messages.Message;
      Request_Count   : in  Messages.Count_Type;
      Index           : in  Messages.Index_Type;
      Message_Imprint : out Imprint;
      Stop            : out Messages.Index_Type;
      Imprint_Status  : out Status_Type) is
   begin
      null;
   end Get_Message_Imprint;


   function Valid_Request(Request_Message : in Messages.Message; Request_Count : in Messages.Count_Type) return Boolean is
   begin
      -- This is Homework #8:
      -- Check that the first octet in Request_Message is 16#30# (a sequence).
      -- Get the sequence length. For now, treat indefinite lengths as errors.
      -- Compare sequence length to Request_Count. Return False if request incomplete or extra junk present.
      -- Get the first sequence item (version) as an Integer. Check that it is version 1.
      -- Get the message imprint from Request_Message and verify that doing so was successful.
      return True;
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
