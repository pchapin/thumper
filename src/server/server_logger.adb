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

package body Server_Logger is

   function Format_Timestamp return String is
      Time_Of_Day : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Year_Time : constant String := Integer'Image(Ada.Calendar.Year(Time_Of_Day));
      Month_Time : constant String := Integer'Image(Ada.Calendar.Month(Time_Of_Day));
      Day_Time : constant String := Integer'Image(Ada.Calendar.Day(Time_Of_Day));
      Seconds_Time : constant Integer := Integer(Ada.Calendar.Seconds(Time_Of_Day));
      Seconds_Hour : constant Integer := Integer(Seconds_Time / 3600);
      Seconds_Minute : constant Integer := (Seconds_Time - (Seconds_Hour * 3600)) / 60;
      Seconds_Second : constant Integer := Seconds_Time - (Seconds_Hour * 3600) - (Seconds_Minute * 60);
      Hour_String : constant String := Integer'Image(Seconds_Hour);
      Minute_String : constant String := Integer'Image(Seconds_Minute);
      Second_String : constant String := Integer'Image(Seconds_Second);
      Full_Seconds_String : constant String := Hour_String(2 .. Hour_String'Last) & ":" & Minute_String(2 .. Minute_String'Last) & ":" & Second_String(2 .. Second_String'Last);
   begin
      return Year_Time(2 .. Year_Time'Last) & "-" & Month_Time(2 .. Month_Time'Last) & "-" & Day_Time(2 .. Day_Time'Last) & " " & Full_Seconds_String;
   end Format_Timestamp;

   procedure Write_Error(Message : in String) is
   begin
      Ada.Text_IO.Put_Line(Format_Timestamp & " *** ERROR: " & Message);
   end Write_Error;

   procedure Write_Information(Message : in String) is
   begin
      Ada.Text_IO.Put_Line(Format_Timestamp & " *** INFORMATION: " & Message);
   end Write_Information;

   procedure Write_Warning(Message : in String) is
   begin
      Ada.Text_IO.Put_Line(Format_Timestamp & " *** WARNING: " & Message);
   end Write_Warning;




end Server_Logger;
