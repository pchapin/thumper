---------------------------------------------------------------------------
-- FILE    : check_der.ads
-- SUBJECT : Package containing tests of the ASN.1 Distinguished Encoding Rules.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

package Check_DER is

   type DER_Test is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T : in out DER_Test);
   function Name(T : DER_Test) return AUnit.Message_String;

end Check_DER;
