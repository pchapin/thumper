pragma SPARK_Mode(On);

with Hermes;

package Client_Timestamp_Maker is

   -- TODO: Add SPARK aspects and return a status indication.
   procedure Create_Timestamp
     (Hash           : in  Hermes.Octet_Array;
      Timestamp      : out Hermes.Octet_Array;
      Timestamp_Size : out Natural);


   function Verify_Timestamp
     (Hash : Hermes.Octet_Array; Timestamp : Hermes.Octet_Array) return Boolean;

end Client_Timestamp_Maker;
