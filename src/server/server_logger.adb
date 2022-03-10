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

package body Server_Logger is


   function Format_Timestamp return Ada.Calendar.Time is
      Now : Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      return Now;
   end Format_Timestamp;

   procedure Write_Error(Message : in String) is
      Time : Ada.Calendar.Time;
   begin
      Time := Format_Timestamp;
      Ada.Text_IO.Put_Line("*** ERROR: " & Ada.Calendar.Formatting.Image(Time) & Message);
   end Write_Error;

   procedure Write_Information(Message : in String) is
      Time : Ada.Calendar.Time;
   begin
      Time := Format_Timestamp;
      Ada.Text_IO.Put_Line("*** INFORMATION: " & Ada.Calendar.Formatting.Image(Time) & Message);
   end Write_Information;

   procedure Write_Warning(Message : in String) is
      Time : Ada.Calendar.Time;
   begin
      Time := Format_Timestamp;
      Ada.Text_IO.Put_Line("*** WARNING: " & Ada.Calendar.Formatting.Image(Time) & Message);
   end Write_Warning;


end Logger;
