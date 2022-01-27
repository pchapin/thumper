---------------------------------------------------------------------------
-- FILE    : hermes_test.adb
-- SUBJECT : Main procedure of the Hermes unit test program.
-- AUTHOR  : (C) Copyright 2015 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
with AUnit.Run;
with AUnit.Reporter.Text;

with Hermes_Suite;

procedure Hermes_Test is
   procedure Run is new AUnit.Run.Test_Runner(Hermes_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run(Reporter);
end Hermes_Test;
