---------------------------------------------------------------------------
-- FILE    : hermes_suite.ads
-- SUBJECT : The main test suite of the ASN.1 unit test program.
-- AUTHOR  : (C) Copyright 2015 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
with AUnit.Test_Suites;

package Hermes_Suite is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;
end Hermes_Suite;
