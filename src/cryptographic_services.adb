---------------------------------------------------------------------------
-- FILE    : cryptographic_services.adb
-- SUBJECT : Body of a package to abstract the crypto needed by Thumper.
-- AUTHORS  : (C) Copyright 2022 by Peter Chapin, Nicole Hurley, and Nathan Brown
--
-- The implementation of this package is not SPARK.
--
-- Ultimately this package should be implemented by calling into an appropriate SPARK crypto
-- library. Right now it makes use of the cryptographic operations in OpenSSL.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Cryptographic_Services is

   -- Imported C Functions From OpenSSL
   ------------------------------------

   -- Note that these OpenSSL subprograms are technically deprecated. However, instead of
   -- replacing them with the newer OpenSSL subprograms, it would be nicer to implement these
   -- operations in SPARK directly.

   -- Imported hash initialization function.
   procedure SHA256_Init(Context : out SHA256_CTX)
     with
       Global => null,
       Import,
       Convention => C,
       External_Name => "SHA256_Init";

   -- Imported hash updating function.
   procedure SHA256_Update
     (Context : in out SHA256_CTX; Data : in Hermes.Octet_Array; Len : in Interfaces.C.size_t)
     with
       Global => null,
       Import,
       Convention => C,
       External_Name => "SHA256_Update";

   -- Imported hash finalization function.
   procedure SHA256_Final(Md : out Hermes.Octet_Array; Context : in out SHA256_CTX)
     with
       Global => null,
       Import,
       Convention => C,
       External_Name => "SHA256_Final";

   -- Visible Subprograms
   ----------------------

   -- Uses the imported C function SHA256_Init() to initialize the hashing procedure.
   procedure Initialize_Hash(Context : out SHA256_CTX) is
   begin
      SHA256_Init(Context);
   end Initialize_Hash;


   -- Uses the imported C function SHA256_Update() to continue the hashing procedure.
   procedure Update_Hash(Context : in out SHA256_CTX; Data : in Hermes.Octet_Array) is
   begin
      SHA256_Update(Context, Data, Interfaces.C.size_t(Data'Length));
   end Update_Hash;


   -- Uses the imported C function SHA256_Final() to finalize the hashing procedure.
   procedure Finalize_Hash(Context : in out SHA256_CTX; Hash_Value : out SHA256_Hash_Type) is
   begin
      SHA256_Final(Hash_Value, Context);
   end Finalize_Hash;


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

end Cryptographic_Services;
