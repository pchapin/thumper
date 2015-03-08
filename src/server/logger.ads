pragma SPARK_Mode(On);

package Logger
  with
     Abstract_State => (Log_Stream with External => (Async_Readers, Effective_Writes)),
     Initializes => Log_Stream
is

   procedure Write_Error(Message : in String)
     with Global => (Output => Log_Stream);

end Logger;
