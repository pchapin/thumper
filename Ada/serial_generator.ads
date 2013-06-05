---------------------------------------------------------------------------
-- FILE    : serial_generator.ads
-- SUBJECT : Specification of a package to abstract the serial number generator.
-- AUTHOR  : (C) Copyright 2013 by Peter Chapin and John McCormick
--
-- Serial numbers are required to be unique over the lifetime of the system's deployment.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Text_IO;

package Serial_Generator
--# own Current_Number;
is
   type Serial_Number_Type is mod 2**64;
   type Status_Type is (Success, Bad_Number, Bad_Update);

   -- Initializes the current number. Fails with Bad_Number if an appropriate value couldn't be read from the file system.
   procedure Initialize(Status : out Status_Type);
   --# global out Current_Number; in out SPARK_IO.State, SPARK_IO.Inputs;
   --# derives Current_Number  from SPARK_IO.State, SPARK_IO.Inputs &
   --#         Status          from SPARK_IO.State, SPARK_IO.Inputs &
   --#         SPARK_IO.State  from SPARK_IO.State &
   --#         SPARK_IO.Inputs from SPARK_IO.State, SPARK_IO.Inputs;

   -- Computes (and saves) the next serial number. Fails with Bad_Update if a new serial number couldn't be calculated.
   procedure Advance(Status : out Status_Type);
   --# global in out Current_Number; in out SPARK_IO.State, SPARK_IO.Outputs;
   --# derives Current_Number   from Current_Number &
   --#         Status           from Current_Number, SPARK_IO.State &
   --#         SPARK_IO.State   from Current_Number, SPARK_IO.State &
   --#         SPARK_IO.Outputs from Current_Number, SPARK_IO.State, SPARK_IO.Outputs;

   -- Returns the current serial number.
   function Get return Serial_Number_Type;
   --# global in Current_Number;

end Serial_Generator;
