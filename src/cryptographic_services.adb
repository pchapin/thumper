---------------------------------------------------------------------------
-- FILE    : cryptographic_services.adb
-- SUBJECT : Body of a package to abstract the crypto needed by Thumper.
-- AUTHOR  : (C) Copyright 2015 by Peter Chapin
--
-- The implementation of this package is not SPARK.
--
-- Ultimately this package should be implemented by calling into an appropriate cryptographic
-- library such as OpenSSL.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(Off);

package body Cryptographic_Services is
   use type Hermes.Octet;

   procedure Initialize_Key(Status : out Status_Type) is
   begin
      -- TODO: Read the key from the file system.
      -- This procedure does not raise an exception so the main program can make progress.
      Status  := Success;
   end Initialize_Key;


   function Make_Signature(Data : in  Hermes.Octet_Array) return Hermes.Octet_Array is
      Signature : Hermes.Octet_Array(1 .. 20) := (others => 0);
   begin
      raise Program_Error with "Cryptographic_Services.Make_Signature not implemented";
      return Signature;
   end Make_Signature;


   procedure Initialize_Hash is
   begin
      raise Program_Error with "Cryptographic_Services.Intialize_Hash not implemented";
   end Initialize_Hash;


   procedure Update_Hash(Data : in Hermes.Octet_Array) is
   begin
      raise Program_Error with "Cryptographic_Services.Update_Hash not implemented";
   end Update_Hash;


   function Finalize_Hash return Hermes.Octet_Array is
      Final_Hash : Hermes.Octet_Array(1 .. 20) := (others => 0);
   begin
      raise Program_Error with "Cryptographic_Services.Finalize_Hash not implemented";
      return Final_Hash;
   end Finalize_Hash;


end Cryptographic_Services;
