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


   function Format_Timestamp return String is
      Now : Constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Time : String := "0000-00-00 00:00:00";
   begin
      Time := Ada.Calendar.Formatting.Image(Now);
      return Time;
   end Format_Timestamp;

   procedure Write_Error(Message : in String) is
      Time : String := "0000-00-00 00:00:00";
   begin
      Time := Format_Timestamp;
      Ada.Text_IO.Put_Line(Time & " *** ERROR: " & Message);
   end Write_Error;

   procedure Write_Information(Message : in String) is
      Time : String := "0000-00-00 00:00:00";
   begin
      Time := Format_Timestamp;
      Ada.Text_IO.Put_Line(Time & " *** INFORMATION: " & Message);
   end Write_Information;

   procedure Write_Warning(Message : in String) is
      Time : String := "0000-00-00 00:00:00";
   begin
      Time := Format_Timestamp;
      Ada.Text_IO.Put_Line(Time & " *** WARNING: " & Message);
   end Write_Warning;


end Server_Logger;
