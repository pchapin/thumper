pragma SPARK_Mode(Off);

with Ada.Text_IO;

package body Logger is

   procedure Write_Error(Message : in String) is
   begin
      Ada.Text_IO.Put_Line("*** ERROR: " & Message);
   end Write_Error;

end Logger;
