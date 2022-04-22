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
use Ada.Text_IO;

package body Client_Logger is

   procedure Log_In_File(Message : in String) is
      Any_File : File_Type;

      File_Name : constant String := "clientlogfile.log";

   begin
      Open(Any_File, Append_File, File_Name);
      Put_Line(Any_File, Message);
      Close(Any_File);

   end Log_In_File;

   procedure Write_Information(Message : in String) is
   begin
      Put_Line("*** INF: " & Message);
      Log_In_File("*** INF: " & Message);
   end Write_Information;


   procedure Write_Warning(Message : in String) is
   begin
      Put_Line("*** WRN: " & Message);
      Log_In_File("*** WRN: " & Message);
   end Write_Warning;


   procedure Write_Error(Message : in String) is
   begin
      Put_Line("*** ERR: " & Message);
      Log_In_File("*** ERR: " & Message);
   end Write_Error;

end Client_Logger;
