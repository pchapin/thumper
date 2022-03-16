---------------------------------------------------------------------------
-- FILE    : server_logger.adb
-- SUBJECT : Body of a log management package.
-- AUTHOR  : (C) Copyright 2022 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(Off);

with Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Formatting;

use Ada.Calendar;
use Ada.Calendar.Formatting;

package body Server_Logger is

   function Format_Timestamp return String is
      Now : Time := Clock;
      Result : String := Image(Now);
   begin
      return Result;
   end Format_Timestamp;

   procedure Write_Error(Message : in String) is
   begin
      Ada.Text_IO.Put_Line(Format_Timestamp & "*** ERROR: " & Message);
   end Write_Error;

   procedure Write_Information(Message : in String) is
   begin
      Ada.Text_IO.Put_Line(Format_Timestamp & "*** INFO: " & Message);
   end Write_Information;

   procedure Write_Warning(Message : in String) is
   begin
      Ada.Text_IO.Put_Line(Format_Timestamp & "*** WARNING: " & Message);
   end Write_Warning;

end Server_Logger;
