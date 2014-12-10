---------------------------------------------------------------------------
-- FILE    : check_ber.ads
-- SUBJECT : Package containing tests of the ASN.1 Basic Encoding Rules.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

package Check_BER is

   type BER_Test is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T : in out BER_Test);
   function Name(T : BER_Test) return AUnit.Message_String;

end Check_BER;
