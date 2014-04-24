---------------------------------------------------------------------------
-- FILE    : serial_generator.adb
-- SUBJECT : Body of a package to abstract the serial number generator.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(Off);

with Ada.Text_IO;

package body Serial_Generator is

   package Serial_Number_IO is new Ada.Text_IO.Modular_IO(Serial_Generator.Serial_Number_Type);

   Current_Number : Serial_Number_Type;

   procedure Initialize(Status : out Status_Type) is
      Number_File : Ada.Text_IO.File_Type;
   begin
      Current_Number := 0;
      Status := Success;
      Ada.Text_IO.Open(Number_File, Ada.Text_IO.In_File, "serial-number.txt");
      Serial_Number_IO.Get(Number_File, Current_Number);
      Ada.Text_IO.Close(Number_File);
   exception
      when others =>
         Status := Bad_Number;
   end Initialize;


   procedure Advance(Status : out Status_Type) is
      Number_File : Ada.Text_IO.File_Type;
   begin
      Status := Bad_Update;
      if Current_Number /= Serial_Number_Type'Last then
         Status := Success;
         Current_Number := Current_Number + 1;
         Ada.Text_IO.Open(Number_File, Ada.Text_IO.Out_File, "serial-number.txt");
         Serial_Number_IO.Put(Number_File, Current_Number);
         Ada.Text_IO.Close(Number_File);
      end if;
   exception
      when others =>
         Status := Bad_Update;
   end Advance;


   function Get return Serial_Number_Type is
   begin
      return Current_Number;
   end Get;

end Serial_Generator;
