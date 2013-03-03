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
   begin
      Status := Bad_Number;
   end Initialize;


   procedure Advance is
   begin
      null;
   end Advance;


   function Get return Serial_Number_Type is
   begin
      return Current_Number;
   end Get;

end Serial_Generator;
