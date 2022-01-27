---------------------------------------------------------------------------
-- FILE    : thumper_suite.ads
-- SUBJECT : The main test suite of the Thumper unit test program.
-- AUTHOR  : (C) Copyright 2015 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
with AUnit.Test_Suites;

package Thumper_Suite is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;
end Thumper_Suite;
