---------------------------------------------------------------------------
-- FILE    : cryptographic_services.adb
-- SUBJECT : Body of a package to abstract the crypto needed by Thumper.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- The implementation of this package is not SPARK.
--
-- Ultimately this package should be implemented by calling into an appropriate cryptographic
-- library.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(Off);

package body Cryptographic_Services is
   use type Hermes.Octet;

   procedure Initialize(Status : out Status_Type) is
   begin
      -- TODO: Read the key from the file system.
      Status  := Success;
   end Initialize;


   function Make_Signature(Data : in  Hermes.Octet_Array) return Hermes.Octet_Array is
      Signature : Hermes.Octet_Array(1 .. 20) := (others => 0);
   begin
      -- TODO: Call into OpenSSL to do the dirty deed.
      return Signature;
   end Make_Signature;

end Cryptographic_Services;
