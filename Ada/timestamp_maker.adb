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
   --   Stop            : The last position of the imprint.
   --   Message_Imprint : The imprint obtained from the message.
   --   Message_Status  : Indicates the success or failure of imprint extraction.
   --
   procedure Get_Message_Imprint
     (Request_Message : in  Messages.Message;
      Request_Count   : in  Messages.Count_Type;
      Index           : in  Messages.Index_Type;
      Stop            : out Messages.Index_Type;
      Message_Imprint : out Imprint;
      Imprint_Status  : out Status_Type) is
   begin
      null;
   end Get_Message_Imprint;


   function Valid_Request(Request_Message : in Messages.Message; Request_Count : in Messages.Count_Type) return Boolean is
      Result          : Boolean := True;
      Length_Stop     : Messages.Index_Type;
      Length          : Natural;
      Version_Stop    : Messages.Index_Type;
      Version         : Integer;
      Imprint_Stop    : Messages.Index_Type;
      Message_Imprint : Imprint;
      Decode_Status   : BER.Status_Type;
      Imprint_Status  : Status_Type;
   begin
      -- TODO: All these complex nested conditionals are nasty. Come up with a better way to handle this.

      if Request_Count <= 2 then
         -- The message is too short. It can't possibly make sense. This check ensures certain array accesses below will work.
         Result := False;
      elsif Request_Message(Request_Message'First) /= 16#30# then
         -- The message is not a sequence.
         Result := False;
      else
         -- Get the length of the sequence.
         BER.Get_Length_Value(Request_Message, Request_Message'First + 1, Length_Stop, Length, Decode_Status);
         if Decode_Status /= BER.Success then
            -- Can't decode the sequence length.
            Result := False;
         elsif Length_Stop + Length /= Request_Message'Last then
            -- Message has the wrong length.
            Result := False;
         else
            -- Get the version number.
            BER.Get_Integer_Value(Request_Message, Length_Stop + 1, Version_Stop, Version, Decode_Status);
            if Decode_Status /= BER.Success then
               -- Can't decode the version.
               Result := False;
            elsif Version /= 1 then
               -- Bad version.
               Result := False;
            else
               -- Get the message imprint.
               Get_Message_Imprint
                 (Request_Message, Request_Count, Version_Stop + 1, Imprint_Stop, Message_Imprint, Imprint_Status);
               if Imprint_Status /= Success then
                  -- Can't decode the imprint.
                  Result := False;
               else
                  -- TODO: Finish me!
                  null;
               end if;
            end if;
         end if;
      end if;
      return Result;
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
