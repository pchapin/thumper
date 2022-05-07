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

   K : constant array (Array_Index_Type) of Word :=
     (16#428A_2F98#, 16#7137_4491#, 16#B5C0_FBCF#, 16#E9B5_DBA5#, 16#3956_C25B#, 16#59F1_11F1#,
      16#923F_82A4#, 16#AB1C_5ED5#, 16#D807_AA98#, 16#1283_5B01#, 16#2431_85BE#, 16#550C_7DC3#,
      16#72BE_5D74#, 16#80DE_B1FE#, 16#9BDC_06A7#, 16#C19B_F174#, 16#E49B_69C1#, 16#EFBE_4786#,
      16#0FC1_9DC6#, 16#240C_A1CC#, 16#2DE9_2C6F#, 16#4A74_84AA#, 16#5CB0_A9DC#, 16#76F9_88DA#,
      16#983E_5152#, 16#A831_C66D#, 16#B003_27C8#, 16#BF59_7FC7#, 16#C6E0_0BF3#, 16#D5A7_9147#,
      16#06CA_6351#, 16#1429_2967#, 16#27B7_0A85#, 16#2E1B_2138#, 16#4D2C_6DFC#, 16#5338_0D13#,
      16#650A_7354#, 16#766A_0ABB#, 16#81C2_C92E#, 16#9272_2C85#, 16#A2BF_E8A1#, 16#A81A_664B#,
      16#C24B_8B70#, 16#C76C_51A3#, 16#D192_E819#, 16#D699_0624#, 16#F40E_3585#, 16#106A_A070#,
      16#19A4_C116#, 16#1E37_6C08#, 16#2748_774C#, 16#34B0_BCB5#, 16#391C_0CB3#, 16#4ED8_AA4A#,
      16#5B9C_CA4F#, 16#682E_6FF3#, 16#748F_82EE#, 16#78A5_636F#, 16#84C8_7814#, 16#8CC7_0208#,
      16#90BE_FFFA#, 16#A450_6CEB#, 16#BEF9_A3F7#, 16#C671_78F2#);

   -- Type definition for the structure used to hold the hashing context.
   type SHA256_CTX is
      record
         H           : State_H;
         Data_Length : Natural;
         Bit_Length  : Natural;
         Data        : Hermes.Octet_Array(Array_Index_Type);
      end record;

end Cryptographic_Services;
