pragma SPARK_Mode(On);

with Cryptographic_Services;
with Network.Socket.Reader;
with Network.Socket.Writer;
with Serial_Generator;

use Network.Socket;

package SPARK_Boundary is

   procedure Service_Clients
     with
       Global => (Input  => (Reader.Input_Message_Stream, Cryptographic_Services.Key),
                  In_Out => (Writer.Output_Message_Stream, Serial_Generator.State)),
       Depends =>
         (Writer.Output_Message_Stream =>+
            (Reader.Input_Message_Stream, Cryptographic_Services.Key, Serial_Generator.State),
          Serial_Generator.State =>+ Reader.Input_Message_Stream);

end SPARK_Boundary;
