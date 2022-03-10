---------------------------------------------------------------------------
-- FILE    : logger.adb
-- SUBJECT : Body of a log management package.
-- AUTHOR  : (C) Copyright 2015 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(Off);

with Ada.Text_IO;
with Ada.Calendar;

package body Logger is

   procedure Write_Error(Message : in String) is
   begin
      Ada.Text_IO.Put_Line("*** ERROR: " & Message);
   end Write_Error;

   procedure Write_Information(Message : in String) is
   begin
      Ada.Text_IO.Put_Line("*** INFORMATION: " & Message);
   end Write_Information;

   procedure Write_Warning(Message : in String) is
   begin
      Ada.Text_IO.Put_Line("*** WARNING: " & Message);
   end Write_Warning;

   function Format_Timestamp return Ada.Calendar.Time is

   begin

   end Format_Timestamp;

end Logger;
