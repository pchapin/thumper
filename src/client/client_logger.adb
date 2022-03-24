---------------------------------------------------------------------------
-- FILE    : client_logger.adb
-- SUBJECT : Body of a log management package.
-- AUTHOR  : (C) Copyright 2022 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(Off);

with Ada.Text_IO;

package body Client_Logger is

   procedure Write_Information(Message : in String) is
   begin
      -- TODO: Display the messages in a message box and potentially also log it to a file.
      Ada.Text_IO.Put_Line("*** INF: " & Message);
   end Write_Error;


   procedure Write_Warning(Message : in String) is
   begin
      -- TODO: Display the messages in a message box and potentially also log it to a file.
      Ada.Text_IO.Put_Line("*** WRN: " & Message);
   end Write_Error;


   procedure Write_Error(Message : in String) is
   begin
      -- TODO: Display the messages in a message box and potentially also log it to a file.
      Ada.Text_IO.Put_Line("*** ERR: " & Message);
   end Write_Error;

end Client_Logger;
