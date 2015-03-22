pragma SPARK_Mode(On);

package body Timestamp_Messages.Server is

   function Put_Timestamp_Value(Stamp : Timestamp) return Hermes.Octet_Array is
   begin
      raise Program_Error with "Timestamp_Messages.Put_Timestamp_Value not implemented";
      return Hermes.Octet_Array'(1 => 0);
   end Put_Timestamp_Value;


   procedure Get_Request_Value
     (Message : in  Hermes.Octet_Array;
      Start   : in  Natural;
      Stop    : out Natural;
      Req     : out Request;
      Status  : out Hermes.DER.Status_Type) is
   begin
      raise Program_Error with "Timestamp_Messages.Server.Get_Request_Value not implemented";
   end Get_Request_Value;


   function Put_Response_Value(Resp : Response) return Hermes.Octet_Array is
      Dummy : Hermes.Octet_Array(1 .. 0);
   begin
      raise Program_Error with "Timestamp_Messages.Server.Put_Response_Value not implemented";
      return Dummy;
   end Put_Response_Value;


end Timestamp_Messages.Server;
