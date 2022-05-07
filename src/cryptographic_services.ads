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

   procedure Update_Transform(Context : in out SHA256_CTX)
     with
       Global => null,
       Depends => (Context => Context);

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
   type Word is mod 2**32;
   type State_H is array(Natural range 0 .. 7) of Word;
   subtype Array_Index_Type is Natural range 0 .. 63;

   -- Type definition for the structure used to hold the hashing context.
   type SHA256_CTX is
      record
         H           : State_H;
         Data_Length : Natural;
         Bit_Length  : Natural;
         Data        : Hermes.Octet_Array(Array_Index_Type);
      end record;

end Cryptographic_Services;
