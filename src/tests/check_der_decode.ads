---------------------------------------------------------------------------
-- FILE    : check_der_decode.ads
-- SUBJECT : Package containing tests of ASN.1 DER decoding.
-- AUTHOR  : (C) Copyright 2015 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

package Check_DER_Decode is

   type DER_Decode_Test is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T : in out DER_Decode_Test);
   function Name(T : DER_Decode_Test) return AUnit.Message_String;

end Check_DER_Decode;
