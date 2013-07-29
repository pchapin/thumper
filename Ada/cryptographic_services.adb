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

package body Cryptographic_Services
  with Refined_State => (Key => Raw_Key)
is

   Raw_Key : Integer;

   procedure Initialize(Status : out Status_Type)
   with
     Refined_Global => (Output => Raw_Key),
     Refined_Depends => ((Raw_Key, Status) => null)
   is
   begin
      -- TODO: Read the key from the file system.
      Raw_Key := 0;
      Status  := Success;
   end Initialize;


   procedure Make_Signature
     (Data        : in  Network.Octet_Array;
      Signature   : out Network.Octet_Array;
      Octet_Count : out Natural;
      Status      : out Status_Type)
   with
     Refined_Global => (Input => Raw_Key),
     Refined_Depends => (Signature => (Data, Raw_Key), (Octet_Count, Status) => Data)
   is
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
