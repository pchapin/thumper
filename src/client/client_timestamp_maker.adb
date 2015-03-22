pragma SPARK_Mode(On);

package body Client_Timestamp_Maker is

   procedure Create_Timestamp
     (Hash           : in  Hermes.Octet_Array;
      Timestamp      : out Hermes.Octet_Array;
      Timestamp_Size : out Natural) is
   begin
      -- Encode a request message.
      -- Send request to server.
      -- Receive response from server.
      -- Decode response and verify correctness.
      raise Program_Error with "Timestamp_Maker.Create_Timestamp not implemented";
   end Create_Timestamp;


   function Verify_Timestamp
     (Hash : Hermes.Octet_Array; Timestamp : Hermes.Octet_Array) return Boolean is
   begin
      -- Decode time stamp and verify correctness.
      raise Program_Error with "Timestamp_Maker.Verify_Timestamp not implemented";
      return False;
   end Verify_Timestamp;


end Client_Timestamp_Maker;
