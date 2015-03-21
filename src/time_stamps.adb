pragma SPARK_Mode(On);

package body Time_Stamps is

   procedure Get_Time_Stamp_Value
     (Message : in  Hermes.Octet_Array;
      Start   : in  Natural;
      Stop    : out Natural;
      Stamp   : out Time_Stamp;
      Status  : out Hermes.DER.Status_Type) is
   begin
      raise Program_Error with "Time_Stamps.Get_Time_Stamp_Value not implemented";
   end Get_Time_Stamp_Value;


   function Put_Time_Stamp_Value(Stamp : Time_Stamp) return Hermes.Octet_Array is
   begin
      raise Program_Error with "Time_Stamps.Put_Time_Stamp_Value not implemented";
      return Hermes.Octet_Array'(1 => 0);
   end Put_Time_Stamp_Value;


end Time_Stamps;
