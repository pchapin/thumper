---------------------------------------------------------------------------
-- FILE    : cryptographic_services.adb
-- SUBJECT : Body of a package to abstract the crypto needed by Thumper.
-- AUTHOR  : (C) Copyright 2013 by Peter Chapin and John McCormick
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package body Cryptographic_Services is

   Not_Implemented : exception;


   procedure Validate_Key(Status : out Status_Type) is
   begin
      -- TODO: Read the key from the file system.
      Status := Bad_Key;
      raise Not_Implemented;
   end Validate_Key;


   -- Computes the signature of Data using Key, putting the result in Signature. The number of octets used are written to
   -- Octet_Count. Fails with Insufficient_Space if the Signature array is not large enough to hold the signature.
   --
   procedure Make_Signature
     (Data        : in Network.Octet_Array;
      Signature   : out Network.Octet_Array;
      Octet_Count : out Natural;
      Status      : out Status_Type) is
   begin
      raise Not_Implemented;
   end Make_Signature;


end Cryptographic_Services;
