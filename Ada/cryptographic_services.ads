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

with Network;

package Cryptographic_Services
with
  Abstract_State => Key
is
   type Status_Type is (Success, Bad_Key);

   subtype Signature_Index_Type is Natural range 1 .. 20;
   subtype Signature_Type is Network.Octet_Array(Signature_Index_Type);

   procedure Initialize(Status : out Status_Type)
   with
     Global => (Output => Key),
     Depends => ((Key, Status) => null);

   -- Computes the signature of Data using a constant private key that is internal to this
   -- package, putting the result in Signature. Fails with Bad_Key if the key is invalid.
   --
   procedure Make_Signature
     (Data      : in  Network.Octet_Array;
      Signature : out Signature_Type;
      Status    : out Status_Type)
   with
     Global => (Input => Key),
     Depends => (Signature => (Data, Key), Status => Key);

end Cryptographic_Services;
