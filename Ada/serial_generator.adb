---------------------------------------------------------------------------
-- FILE    : serial_generator.adb
-- SUBJECT : Body of a package to abstract the serial number generator.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Integer_Text_IO;

package body Serial_Generator
with
  Refined_State => (Number => Current_Number)
is

   Current_Number : Serial_Number_Type;

   procedure Initialize(Status : out Status_Type)
   with
     Refined_Global => (Output => Current_Number),
     Refined_Depends => ((Current_Number, Status) => null)
   is
      Number_File : Ada.Text_IO.File_Type;
      Raw_Number  : Integer;  -- TODO: We should really read Serial_Number_Type values from the file.
   begin
      Current_Number := 0;
      Status := Bad_Number;
      Ada.Text_IO.Open(Number_File, Ada.Text_IO.In_File, "serial-number.txt");
      Ada.Integer_Text_IO.Get(Number_File, Raw_Number);
      if Raw_Number >= 0 then
         Current_Number := Serial_Number_Type(Raw_Number);
         Status := Success;
      end if;
      Ada.Text_IO.Close(Number_File);
   end Initialize;


   procedure Advance(Status : out Status_Type)
   with
     Refined_Global => (In_Out => Current_Number),
     Refined_Depends => ((Current_Number, Status) => Current_Number)
   is
      Number_File : Ada.Text_IO.File_Type;
      Raw_Number  : Integer;  -- TODO: We should really write Serial_Number_Type values to the file.
   begin
      Status := Bad_Update;
      if Current_Number /= Serial_Number_Type'Last then
         Current_Number := Current_Number + 1;
         Ada.Text_IO.Open(Number_File, Ada.Text_IO.Out_File, "serial-number.txt");
         if Current_Number <= Serial_Number_Type(Integer'Last) then
            Raw_Number := Integer(Current_Number);
            Ada.Integer_Text_IO.Put(Number_File, Raw_Number);
            Ada.Text_IO.Close(Number_File);
            Status := Success;
         end if;
      end if;
   end Advance;


   function Get return Serial_Number_Type
   with
     Refined_Global => (Input => Current_Number)
   is
   begin
      return Current_Number;
   end Get;

end Serial_Generator;
