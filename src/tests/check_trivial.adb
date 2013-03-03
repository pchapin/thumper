---------------------------------------------------------------------------
-- FILE    : check_trivial.adb
-- SUBJECT : Package containing tests of nothing in particular.
-- AUTHOR  : (C) Copyright 2012 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with AUnit.Assertions; use AUnit.Assertions;

package body Check_Trivial is

   procedure Test_Nothing(T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      -- This is arbitrary code. Use Assert to verify that an expected condition is actually true.
      -- Test moves between strings of the same length.
      Assert(0 = 0, "Zero is not zero!");
      Assert(true = true, "True is the same as false!");
   end Test_Nothing;


   procedure Register_Tests(T : in out Trivial_Test) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine(T, Test_Nothing'Access, "Nothing Test");
   end Register_Tests;


   function Name(T : Trivial_Test) return AUnit.Message_String is
   begin
      return AUnit.Format("Trivial");
   end Name;

end Check_Trivial;
