---------------------------------------------------------------------------
-- FILE    : serial_generator.ads
-- SUBJECT : Specification of a package to abstract the serial number generator.
-- AUTHOR  : (C) Copyright 2022 by Peter Chapin
--
-- Serial numbers are required to be unique over the lifetime of the system's deployment.
-- This is accomplished by generating random 64-bit numbers on the assumption that it is
-- very improbably that a number will get reused. The RNG is seeded using the time of
-- system startup, so when the system reboots it will produce a different sequence with high
-- probability. Note that the RNG uses is not secure; the serial numbers it produces are
-- predictable if the system boot time is known or can be guessed.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Calendar;

package Serial_Generator
  with
     Abstract_State => State,
     Initializes => (State => Ada.Calendar.Clock_Time)
is
   type Serial_Number_Type is mod 2**64;

   -- Computes the next serial number and updates the internal state to prepare for a following
   -- call to Next. This procedure can't fail. However, the numbers it generates cycle with a
   -- period of 2**64.
   --
   procedure Next(Number : out Serial_Number_Type)
     with
        Global => (In_Out => State),
        Depends => ((State, Number) => State);

end Serial_Generator;
