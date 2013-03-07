---------------------------------------------------------------------------
-- FILE    : cryptographic_services.adb
-- SUBJECT : Body of a package to abstract the crypto needed by Thumper.
-- AUTHOR  : (C) Copyright 2013 by Peter Chapin and John McCormick
--
-- The implementation of this package is not SPARK.
--
-- Ultimately this package should be implemented by calling into an appropriate cryptographic library. For now it
-- uses a fake key and creates fake digital signatures. Notice that because this package is not SPARK, the Examiner
-- can't check the correctness of the annotations in the specification. It's up to the programmer to get them right.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package body Cryptographic_Services is


   procedure Validate_Key(Status : out Status_Type) is
   begin
      -- TODO: Read the key from the file system.
      Status := Success;
   end Validate_Key;


   procedure Make_Signature
     (Data        : in  Network.Octet_Array;
      Signature   : out Network.Octet_Array;
      Octet_Count : out Natural;
      Status      : out Status_Type) is
   begin
      -- Create a fake signature of 20 bytes (all zeros).
      if Signature'Length < 20 then
         Octet_Count := 0;
         Status := Insufficient_Space;
      else
         for I in 1 .. 20 loop
            Signature(Signature'First + (I - 1)) := 0;
         end loop;
         Octet_Count := 20;
         Status := Success;
      end if;
   end Make_Signature;


end Cryptographic_Services;
