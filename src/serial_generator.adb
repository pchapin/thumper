---------------------------------------------------------------------------
-- FILE    : serial_generator.adb
-- SUBJECT : Body of a package to abstract the serial number generator.
-- AUTHOR  : (C) Copyright 2013 by Peter Chapin and John McCormick
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package body Serial_Generator is

   Current_Number : Serial_Number_Type;

   procedure Initialize(Status : out Status_Type) is
      Number_File        : SPARK_IO.File_Type;
      Number_File_Status : SPARK_IO.File_Status;
      Raw_Number         : Integer;  -- TODO: We should really read Serial_Number_Type values from the file.
      Read_Success       : Boolean;
   begin
      Current_Number := 0;
      Status := Bad_Number;
      SPARK_IO.Open(Number_File, SPARK_IO.In_File, "serial-number.txt", "", Number_File_Status);
      if Number_File_Status = SPARK_IO.Ok then
         SPARK_IO.Get_Integer(Number_File, Raw_Number, 0, Read_Success);
         if Read_Success and then Raw_Number >= 0 then
            Current_Number := Serial_Number_Type(Raw_Number);
            Status := Success;
         end if;
         --# accept flow_message, 10, Number_File, "Close sets Number_File to an invalid value";
         --# accept flow_message, 10, Number_File_Status, "Failure when closing an input file has no bad effects";
         SPARK_IO.Close(Number_File, Number_File_Status);
         --# end accept;
         --# end accept;
      end if;
   end Initialize;


   procedure Advance(Status : out Status_Type) is
      Number_File        : SPARK_IO.File_Type;
      Number_File_Status : SPARK_IO.File_Status;
      Raw_Number         : Integer;  -- TODO: We should really write Serial_Number_Type values to the file.
   begin
      Status := Bad_Update;
      if Current_Number /= Serial_Number_Type'Last then
         Current_Number := Current_Number + 1;
         SPARK_IO.Open(Number_File, SPARK_IO.Out_File, "serial-number.txt", "", Number_File_Status);
         if Number_File_Status = SPARK_IO.Ok and then Current_Number <= Serial_Number_Type(Integer'Last) then
            Raw_Number := Integer(Current_Number);
            SPARK_IO.Put_Integer(Number_File, Raw_Number, 0, 10);
            --# accept flow_message, 10, Number_File, "Close sets Number_File to an invalid value";
            SPARK_IO.Close(Number_File, Number_File_Status);
            --# end accept;
            if Number_File_Status = SPARK_IO.Ok then
               Status := Success;
            end if;
         end if;
      end if;
   end Advance;


   function Get return Serial_Number_Type is
   begin
      return Current_Number;
   end Get;

end Serial_Generator;
