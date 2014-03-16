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
-- The body of the generic package SPARK.Text_IO.Modular_IO is not in SPARK. This means that, currently, it can only be
-- instantiated in a unit with SPARK mode off. This limitation may be lifted in the future.

with SPARK.Text_IO;
with SPARK.Text_IO.Modular_IO;

use type SPARK.Text_IO.File_Status;

package body Serial_Generator is
   package Serial_Number_IO is new SPARK.Text_IO.Modular_IO(Serial_Number_Type);

   Current_Number : Serial_Number_Type;

   procedure Initialize(Status : out Status_Type) is
      Number_File : SPARK.Text_IO.File_Type;
      Result      : Serial_Number_IO.Mod_Result;
   begin
      Current_Number := 0;
      Status := Bad_Number;
      SPARK.Text_IO.Open(Number_File, SPARK.Text_IO.In_File, "serial-number.txt");
      Serial_Number_IO.Get(Number_File, Result);
      if Result.Status = SPARK.Text_IO.Success then
         Current_Number := Result.Item;
         Status := Success;
      end if;
      SPARK.Text_IO.Close(Number_File);
   end Initialize;


   procedure Advance(Status : out Status_Type) is
      Number_File : SPARK.Text_IO.File_Type;
   begin
      Status := Bad_Update;
      if Current_Number /= Serial_Number_Type'Last then
         Current_Number := Current_Number + 1;
         SPARK.Text_IO.Open(Number_File, SPARK.Text_IO.Out_File, "serial-number.txt");
         if SPARK.Text_IO.Status(Number_File) = SPARK.Text_IO.Success then
            -- TODO: Deal with the possibility of a failed Put.
            Serial_Number_IO.Put(Number_File, Current_Number);
            SPARK.Text_IO.Close(Number_File);
            Status := Success;
         end if;
      end if;
   end Advance;


   function Get return Serial_Number_Type is
   begin
      return Current_Number;
   end Get;

end Serial_Generator;
