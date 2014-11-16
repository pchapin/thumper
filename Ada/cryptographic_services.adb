---------------------------------------------------------------------------
-- FILE    : cryptographic_services.adb
-- SUBJECT : Body of a package to abstract the crypto needed by Thumper.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- The implementation of this package is not SPARK.
--
-- Ultimately this package should be implemented by calling into an appropriate cryptographic
-- library. For now it uses a fake key and creates fake digital signatures.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Cryptographic_Services
  with Refined_State => (Key => Raw_Key)
is
   use type Network.Octet;

   Raw_Key : Network.Octet := 0;

   procedure Initialize(Status : out Status_Type)
   with
     Refined_Global => (Output => Raw_Key),
     Refined_Depends => ((Raw_Key, Status) => null)
   is
   begin
      -- TODO: Read the key from the file system.
      Raw_Key := 42;
      Status  := Success;
   end Initialize;


   procedure Make_Signature
     (Data        : in  Network.Octet_Array;
      Signature   : out Signature_Type;
      Status      : out Status_Type)
   with
     Refined_Global => (Input => Raw_Key),
     Refined_Depends => (Signature => (Data, Raw_Key), Status => Raw_Key)
   is
      I : Natural;
      J : Signature_Index_Type;
   begin
      -- Create a fake signature of 20 bytes.
      if Raw_Key = 0 then
         Signature := (others => 0);
         Status := Bad_Key;
      else
         Signature := (others => Raw_Key);
         I := Data'First;
         J := Signature_Index_Type'First;
         while I <= Data'Last loop
            Signature(J) := Signature(J) + Data(I);
            I := I + 1;
            if J < Signature_Index_Type'Last then
               J := J + 1;
            else
               J := Signature_Index_Type'First;
            end if;
         end loop;
         Status := Success;
      end if;
   end Make_Signature;

end Cryptographic_Services;
