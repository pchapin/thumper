---------------------------------------------------------------------------
-- FILE    : cryptographic_services.ads
-- SUBJECT : Specification of a package to abstract the crypto needed by Thumper.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Hermes;

package Cryptographic_Services
with
  Abstract_State => Key
is
   type Status_Type is (Success, Bad_Key);

   procedure Initialize(Status : out Status_Type)
     with
        Global => (Output => Key),
        Depends => ((Key, Status) => null);

   -- Computes the signature of Data using a constant private key that is internal to this
   -- package, returning the result in Signature.
   --
   function Make_Signature(Data : in  Hermes.Octet_Array) return Hermes.Octet_Array
     with Global => (Input => Key);

end Cryptographic_Services;
