---------------------------------------------------------------------------
-- FILE    : serial_generator.ads
-- SUBJECT : Specification of a package to abstract the serial number generator.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Serial numbers are required to be unique over the lifetime of the system's deployment.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package Serial_Generator
with
  Abstract_State => Number
is
   type Serial_Number_Type is mod 2**64;
   type Status_Type is (Success, Bad_Number, Bad_Update);

   -- Initializes the current number. Fails with Bad_Number if an appropriate value couldn't be
   -- read from the file system.
   procedure Initialize(Status : out Status_Type)
   with
     Global => (Output => Number),
     Depends => ((Number, Status) => null);

   -- Computes (and saves) the next serial number. Fails with Bad_Update if a new serial number
   -- couldn't be saved or if the serial number is already at its maximum value and can't be
   -- advanced.
   procedure Advance(Status : out Status_Type)
   with
     Global => (In_Out => Number),
     Depends => ((Number, Status) => Number);

   -- Returns the current serial number.
   function Get return Serial_Number_Type
   with
     Global => (Input => Number);

end Serial_Generator;
