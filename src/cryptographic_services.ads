---------------------------------------------------------------------------
-- FILE    : cryptographic_services.ads
-- SUBJECT : Specification of a package to abstract the crypto needed by Thumper.
-- AUTHOR  : (C) Copyright 2022 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Interfaces.C;
with Hermes;

package Cryptographic_Services is

   type Status_Type is (Success, Bad_Key);

   -- Hash context (state).
   type SHA256_CTX is private;

   -- An array to hold the hash value.
   subtype SHA256_Hash_Type is Hermes.Octet_Array(0 .. 31);

   -- Prepares the internal hash for use.
   procedure Initialize_Hash(Context: out SHA256_CTX)
     with
       Global => null,
       Depends => (Context => null);

   -- Updates the hash using the given data. This procedure can be called repeatedly to hash
   -- a large amount of data. The size of the Data array can be anything (even zero) and need
   -- not be the same size between calls.
   procedure Update_Hash(Context: in out SHA256_CTX; Data : in Hermes.Octet_Array)
     with
       Global => null,
       Depends => (Context =>+ Data);

   -- Finalizes the hash computation and returns the result.
   procedure Finalize_Hash(Context: in out SHA256_CTX; Hash_Value : out SHA256_Hash_Type)
     with
       Global => null,
       Depends => ((Context, Hash_Value) => Context);

   -- Reads the server's private key from file system.
   procedure Initialize_Key(Status : out Status_Type)
     with
       Global => null,
       Depends => (Status => null);

   -- Computes the signature of Data using a constant private key that is internal to this
   -- package, returning the result.
   function Make_Signature(Data : in  Hermes.Octet_Array) return Hermes.Octet_Array
     with Global => null;

private

   type SHA_LONG is new Interfaces.C.unsigned;
   type SHA256_CTX_Array is array(SHA_LONG range <>) of Natural;

   -- Type definition for the structure used to create the hash.
   type SHA256_CTX is
      record
         H      : SHA256_CTX_Array(1 .. 8);
         Nl     : SHA_LONG;
         Nh     : SHA_LONG;
         Data   : SHA256_CTX_Array(1 .. 16);
         Num    : Interfaces.C.unsigned;
         Md_Len : Interfaces.C.unsigned;
      end record
     with Convention => C;

end Cryptographic_Services;
