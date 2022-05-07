---------------------------------------------------------------------------
-- FILE    : cryptographic_services.adb
-- SUBJECT : Body of a package to abstract the crypto needed by Thumper.
-- AUTHORS  : (C) Copyright 2022 by Peter Chapin, Nicole Hurley, and Nathan Brown
--
-- The implementation of this package is not SPARK.
--
-- Ultimately this package should be implemented by calling into an appropriate SPARK crypto
-- library. Right now it makes use of the cryptographic operations in OpenSSL for digital
-- signatures.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Interfaces;

package body Cryptographic_Services is

   type Word_Array_Type is array (Array_Index_Type) of Word
     with Pack;

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

   function Ch(X : in Word; Y : in Word; Z : in Word) return Word is
     ((X and Y) or (Z and not X))
     with Inline;


   function Maj(X : in Word; Y : in Word; Z : in Word) return Word is
     ((X and Y) or (X and Z) or (Y and Z))
     with Inline;


   -- Wrapper functions to do basic shifts and rotates.
   -- TODO: Declare these as intrinsics.

   function Rotate_Right(Value : Word; Amount_To_Move : Natural) return Word is
     (Word(Interfaces.Rotate_Right(Interfaces.Unsigned_32(Value), Amount_To_Move)))
     with Inline;


   function Shift_Right(Value : in Word; Amount : in Natural) return Word is
     (Word(Interfaces.Shift_Right(Interfaces.Unsigned_32(Value), Amount)))
     with Inline;


   function Shift_Left(Value : in Word; Amount : in Natural) return Word is
     (Word(Interfaces.Shift_Left(Interfaces.Unsigned_32(Value), Amount)))
     with Inline;


   -- Functions from FIPS 180-4 Section 4.1.2

   function Sigma_Zero_256(Value : in Word) return Word is
     (Rotate_Right(Value, 2) xor Rotate_Right(Value, 13) xor Rotate_Right(Value, 22))
     with Inline;


   function Sigma_One_256(Value : in Word) return Word is
     (Rotate_Right(Value, 6) xor Rotate_Right(Value, 11) xor Rotate_Right(Value, 25))
     with Inline;

   -- We need to find a better name for these two functions.
   -- Math equation has a weird o with a line coming out of the top right going right.
   function O_Zero_256(Value : in Word) return Word is
     (Rotate_Right(Value, 7) xor Rotate_Right(Value, 18) xor Shift_Right(Value, 3))
     with Inline;


   function O_One_256(Value : in Word) return Word is
     (Rotate_Right(Value, 17) xor Rotate_Right(Value, 19) xor Shift_Right(Value, 10))
     with Inline;


   -- Visible Subprograms
   ----------------------

   procedure Initialize_Hash(Context : out SHA256_CTX) is
   begin
      Context.Bit_Length  := 0;
      Context.Data_Length := 0;
      Context.Data := (others => 0);
      Context.H :=
        (16#6A09_E667#, 16#BB67_AE85#, 16#3C6E_F372#, 16#A54F_F53A#,
         16#510E_527F#, 16#9B05_688C#, 16#1F83_D9AB#, 16#5BE0_CD19#);
   end Initialize_Hash;


   -- Takes the Data and transforms it into Context.H
   procedure Update_Transform(Context : in out SHA256_CTX) is
      Data       : constant Hermes.Octet_Array := Context.Data;
      Word_Array : Word_Array_Type := (others => 0);
      A, B, C, D, E, F, G, H : Word;
      T1 : Word;
      T2 : Word;
      J  : Natural := 0;
   begin
      for I in Array_Index_Type loop
         if (I < 16) then
            -- 16 * 4 = 60 which means J will be equal to or less than 60 always.
            pragma Assert(J <= 60);
            Word_Array(I) :=
              (Shift_Left(Word(Data(Data'First + J + 0)), 24)) or
              (Shift_Left(Word(Data(Data'First + J + 1)), 16)) or
              (Shift_Left(Word(Data(Data'First + J + 2)),  8)) or
              (Word(Data(Data'First + J + 3)));
            J := J + 4;
         else
            Word_Array(I) :=
              O_One_256 (Word_Array(I -  2)) + Word_Array(I -  7) +
              O_Zero_256(Word_Array(I - 15)) + Word_Array(I - 16);
         end if;
      end loop;

      A := Context.H(0);
      B := Context.H(1);
      C := Context.H(2);
      D := Context.H(3);
      E := Context.H(4);
      F := Context.H(5);
      G := Context.H(6);
      H := Context.H(7);

      for Q in Array_Index_Type loop
         T1 := H + Sigma_One_256(E) + Ch(E, F, G) + K(Q) + Word_Array(Q);
         T2 := Sigma_Zero_256(A) + Maj(A, B, C);
         H := G;
         G := F;
         F := E;
         E := D + T1;
         D := C;
         C := B;
         B := A;
         A := T1 + T2;
      end loop;

      Context.H(0) := Context.H(0) + A;
      Context.H(1) := Context.H(1) + B;
      Context.H(2) := Context.H(2) + C;
      Context.H(3) := Context.H(3) + D;
      Context.H(4) := Context.H(4) + E;
      Context.H(5) := Context.H(5) + F;
      Context.H(6) := Context.H(6) + G;
      Context.H(7) := Context.H(7) + H;
   end Update_Transform;


   --Takes a Context and an unbounded array of Data and hashes it or stores it into Context.Data.
   procedure Update_Hash(Context : in out SHA256_CTX; Data : in Hermes.Octet_Array) is
   begin
      --Make sure data is within range
      for I in 0 .. Data'Length - 1 loop
         -- Bit_Length will always be a multiple of 512 in this function.
         pragma Assert(Context.Bit_Length mod 512 = 0);

         -- Context.Data_Length has to go to 64 to trigger the if statement but then it gets reset.
         pragma Assert(Context.Data_Length < 64);

         -- Takes the Data Variable and puts it into Context.Data
         Context.Data(Context.Data_Length) := Data(Data'First + Integer(I));
         Context.Data_Length := Context.Data_Length + 1;

         -- Every 64 bits of data, transform that data into Context.H
         if Context.Data_Length = 64 then
            Update_Transform(Context);

            -- Add the data that just got transform to the Bit_Length, 64 octets equals 512 bits.
            Context.Bit_Length := Context.Bit_Length + 512;

            -- Reset Data_Length
            Context.Data_Length := 0;
         end if;
      end loop;
   end Update_Hash;


   procedure Finalize_Hash(Context : in out SHA256_CTX; Hash_Value : out SHA256_Hash_Type) is
      I : Natural;
   begin
      -- Context.Data_Length should never be more than 63
      pragma Assert(Context.Data_Length < 64);

      I := Context.Data_Length;
      -- If data length is less than 56 then the 0x80 bit and the 64 bit length will fit in the same block.
      if Context.Data_Length < 56 then
         Context.Data(I) := 16#80#;
         I := I + 1;

         --Pad with zeros until it hits the spot where the length bits will go.
         while I < 56 loop
            Context.Data(I) := 16#00#;
            I := I + 1;
         end loop;

      else
         -- If not then we need to pad the current block and make a new block for the length.
         Context.Data(I) := 16#80#;
         I := I + 1;

         -- Pad the rest of the current block with zeros
         while I < 64 loop
            Context.Data(I) := 16#00#;
            I := I + 1;
         end loop;

         -- Hash the current block into Context.H.
         -- Since the length bits won't fit we need to make another block
         Update_Transform(Context);

         --Make sure Context.Data is empty
         Context.Data(0 .. 55) := (others => 0);
      end if;

      Context.Bit_Length := Context.Bit_Length + (Context.Data_Length * 8);

      -- Get the binary of Context.Bit_Length and mask it so it can fit into an octet.
      Context.Data(63) := Hermes.Octet(Word(Context.Bit_Length) and Word(16#FF#));
      Context.Data(62) := Hermes.Octet(Shift_Right(Word(Context.Bit_Length),  8) and Word(16#FF#));
      Context.Data(61) := Hermes.Octet(Shift_Right(Word(Context.Bit_Length), 16) and Word(16#FF#));
      Context.Data(60) := Hermes.Octet(Shift_Right(Word(Context.Bit_Length), 24) and Word(16#FF#));
      Context.Data(59) := Hermes.Octet(Shift_Right(Word(Context.Bit_Length), 32) and Word(16#FF#));
      Context.Data(58) := Hermes.Octet(Shift_Right(Word(Context.Bit_Length), 40) and Word(16#FF#));
      Context.Data(57) := Hermes.Octet(Shift_Right(Word(Context.Bit_Length), 48) and Word(16#FF#));
      Context.Data(56) := Hermes.Octet(Shift_Right(Word(Context.Bit_Length), 56) and Word(16#FF#));

      -- Hash the rest of the message (It now has been padded and the length has been appended)
      Update_Transform(Context);

      -- Since this implementation uses little endian byte ordering and SHA uses big endian,
      -- reverse all the bytes when copying the final state to the output hash.
      for U in 0 .. 3 loop
         Hash_Value(U + 0)  := Hermes.Octet(Shift_Right(Context.H(0), 24 - U * 8) and 16#000000FF#);
         Hash_Value(U + 4)  := Hermes.Octet(Shift_Right(Context.H(1), 24 - U * 8) and 16#000000FF#);
         Hash_Value(U + 8)  := Hermes.Octet(Shift_Right(Context.H(2), 24 - U * 8) and 16#000000FF#);
         Hash_Value(U + 12) := Hermes.Octet(Shift_Right(Context.H(3), 24 - U * 8) and 16#000000FF#);
         Hash_Value(U + 16) := Hermes.Octet(Shift_Right(Context.H(4), 24 - U * 8) and 16#000000FF#);
         Hash_Value(U + 20) := Hermes.Octet(Shift_Right(Context.H(5), 24 - U * 8) and 16#000000FF#);
         Hash_Value(U + 24) := Hermes.Octet(Shift_Right(Context.H(6), 24 - U * 8) and 16#000000FF#);
         Hash_Value(U + 28) := Hermes.Octet(Shift_Right(Context.H(7), 24 - U * 8) and 16#000000FF#);
      end loop;
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
