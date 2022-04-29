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

   -- Visible Subprograms
   ----------------------

   function Ch (x : in Word; y : in Word; z : in Word) return Word is
   begin
      return (x AND y) OR (z AND NOT x);
   end Ch;

   function Maj (x, y, z : in Word) return Word is
   begin
      return (x AND y) OR (x AND z) OR (y AND z);
   end Maj;

   subtype Shift_Range is Integer range 0..1000000;

   --Wrapper function to rotate bits to the right
   function Rotate_Right(Value : Word; Amount_To_Move : Shift_Range) return Word is
   begin
      return Word(Interfaces.Rotate_Right(Interfaces.Unsigned_32(Value), Amount_To_Move));
   end Rotate_Right;

   --Wrapper function to rotate bits to the left
   --function Rotate_Left(Value : Word; Amount_To_Move : Shift_Range) return Word is
   --begin
      --return Word(Interfaces.Rotate_Left(Interfaces.Unsigned_32(Value), Amount_To_Move));
   --end Rotate_Left;

   function ShiftR(Value : in Word; Amount : in Shift_Range) return Word is
   begin
      return Word(Interfaces.Shift_Right(Interfaces.Unsigned_32(Value), Amount));
   end ShiftR;

   function ShiftL(Value : in Word; Amount : in Shift_Range) return Word is
   begin
      return Word(Interfaces.Shift_Left(Interfaces.Unsigned_32(Value), Amount));
   end ShiftL;

   --Functions from FIPS 180-4 Section 4.1.2

   function Sigma_Zero_256(W_Arr : in Word) return Word is
   begin
      return Rotate_Right(W_Arr,2) XOR Rotate_Right(W_Arr,13) XOR Rotate_Right(W_Arr,22);
   end Sigma_Zero_256;

   function Sigma_One_256(W_Arr : in Word) return Word is
   begin
      return Rotate_Right(W_Arr,6) XOR Rotate_Right(W_Arr,11) XOR Rotate_Right(W_Arr,25);
   end Sigma_One_256;

   --Need to find a better name for these two functions
   --Math equation has a weird o with a line coming out of the top right going right
   function O_Zero_256(W_Arr : in Word) return Word is
   begin
      return Rotate_Right(W_Arr,7) XOR Rotate_Right(W_Arr,18) XOR ShiftR(W_Arr,3);
   end O_Zero_256;

   function O_One_256(W_Arr : in Word) return Word is
   begin
      return Rotate_Right(W_Arr,17) XOR Rotate_Right(W_Arr,19) XOR ShiftR(W_Arr,10);
   end O_One_256;

   -- Uses the imported C function SHA256_Init() to initialize the hashing procedure.
   procedure Initialize_Hash(Context : out SHA256_CTX) is
   begin
      Context.BitLen := 0;
      Context.DataLen := 0;
      Context.H := (16#6A09_E667#, 16#BB67_AE85#, 16#3C6E_F372#, 16#A54F_F53A#,
                    16#510E_527F#, 16#9B05_688C#, 16#1F83_D9AB#, 16#5BE0_CD19#);
   end Initialize_Hash;

   --Takes the Data and transforms it into Context.H
   procedure Update_Transform(Context : in out SHA256_CTX) is
      --Intailize with data
      Data : constant Hermes.Octet_Array := Context.Data;
      W_Arr   : Word64 := (others => 0);
      a : Word; b : Word; c : Word; d : Word; e : Word; f : Word;
      g : Word; h : Word;
      T1 : Word;
      T2 : Word;
      i : Natural := 0;
      j : Natural := 0;
   begin
      while(i < 64) loop
         if (i < 16) then
            --16 * 4 = 60 which means j will be equal to or less than 60 always
            pragma Assume (j <= 60);
            W_Arr(i) := (ShiftL(Word(Data(Data'First + Integer(j))), 24)) or (ShiftL(Word(Data(Data'First + Integer(j) + 1)), 16)) or
              (ShiftL(Word(Data(Data'First + Integer(j) + 2)), 8)) or (Word(Data(Data'First + Integer(j) + 3)));
            j := j + 4;
         else
            W_Arr(i) := O_One_256(W_Arr(i - 2)) + W_Arr(i - 7) + O_Zero_256(W_Arr(i - 15)) + W_Arr(i - 16);
         end if;
         i := i + 1;
      end loop;


      a := Context.H(0);
      b := Context.H(1);
      c := Context.H(2);
      d := Context.H(3);
      e := Context.H(4);
      f := Context.H(5);
      g := Context.H(6);
      h := Context.H(7);

      for Q in 0 .. 63 loop
         T1 := h + Sigma_One_256(e) + Ch(e,f,g) + K(Array64(Q)) + W_Arr(Q);
         T2 := Sigma_Zero_256(a) + Maj(a,b,c);
         h := g;
         g := f;
         f := e;
         e := d + T1;
         d := c;
         c := b;
         b := a;
         a := T1 + T2;
      end loop;

      Context.H(0) := Context.H(0) + a;
      Context.H(1) := Context.H(1) + b;
      Context.H(2) := Context.H(2) + c;
      Context.H(3) := Context.H(3) + d;
      Context.H(4) := Context.H(4) + e;
      Context.H(5) := Context.H(5) + f;
      Context.H(6) := Context.H(6) + g;
      Context.H(7) := Context.H(7) + h;
   end Update_Transform;

   --Takes a Context and an unbounded array of Data and hashes it or stores it into Context.Data if it will fit
   procedure Update_Hash(Context : in out SHA256_CTX; Data : in Hermes.Octet_Array) is
      i : Word range 0 .. Data'Length := 0;
   begin
      --Make sure data is within range
      while i < Data'Length loop
         --BitLen will always be a multiple of 512 in this function
         pragma Assume (Context.BitLen = 0 or else Context.BitLen mod 512 = 0);
         --Context.DataLen has to go to 64 to trigger the if statement but then it gets reset
         pragma Assume (i <= Data'Length and Context.DataLen < 64);
         --Takes the Data Variable and puts it into Context.Data
         Context.Data(Context.DataLen) := Data(Data'First + Integer(i));
         Context.DataLen := Context.DataLen + 1;
         --Every 64 bits of data, transform that data into Context.H
         if (Context.DataLen = 64) then
            Update_Transform(Context);
            --Add the data that just got transform to the BitLen, 64 octets equals 512 bits
            Context.BitLen := Context.BitLen + 512;

            --Reset DataLen
            Context.DataLen := 0;
         end if;
         i := i + 1;
      end loop;
   end Update_Hash;

   procedure Finalize_Hash(Context : in out SHA256_CTX; Hash_Value : out SHA256_Hash_Type) is
      i : Natural;
   begin
      --DataLen should never be more than 63
      pragma Assume (Context.DataLen < 64);
      i := Context.DataLen;
      --If data length is less than 56 then the 0x80 bit and the 64 bit length will fit in the same block
      if (Context.DataLen < 56) then
         Context.Data(i) := 16#80#;
         i := i + 1;
         --Pad with zeros until it hits the spot where the length bits will go
         while (i < 56) loop
            Context.Data(i) := 16#00#;
            i := i + 1;
         end loop;
      else -- If not then we need to pad the current block and make a new block to put the length into
         Context.Data(i) := 16#80#;
         i := i + 1;
         --Pad the rest of the current block with zeros
         while (i < 64) loop
            Context.Data(i) := 16#00#;
            i := i + 1;
         end loop;
         --Hash the current block into Context.H since the length bits won't fit we need to make another block
         Update_Transform(Context);
         --Make sure Context.Data is empty
         for Y in 0 .. 55 loop
            Context.Data(Y) := 0;
         end loop;
      end if;

      Context.BitLen := Context.BitLen + (Context.DataLen * 8);

      --Get the binary of Context.BitLen and mask it so it can fit into an octet.
      Context.Data(63) := Hermes.Octet(Word(Context.BitLen) and Word(16#FF#));
      Context.Data(62) := Hermes.Octet(ShiftR(Word(Context.BitLen),8) and Word(16#FF#));
      Context.Data(61) := Hermes.Octet(ShiftR(Word(Context.BitLen),16) and Word(16#FF#));
      Context.Data(60) := Hermes.Octet(ShiftR(Word(Context.BitLen),24) and Word(16#FF#));
      Context.Data(59) := Hermes.Octet(ShiftR(Word(Context.BitLen),32) and Word(16#FF#));
      Context.Data(58) := Hermes.Octet(ShiftR(Word(Context.BitLen),40) and Word(16#FF#));
      Context.Data(57) := Hermes.Octet(ShiftR(Word(Context.BitLen),48) and Word(16#FF#));
      Context.Data(56) := Hermes.Octet(ShiftR(Word(Context.BitLen),56) and Word(16#FF#));

      --Hash the rest of the message (It now has been padded and the length has been appended)
      Update_Transform(Context);

      --Since this implementation uses little endian byte ordering and SHA uses big endian,
      --reverse all the bytes when copying the final state to the output hash.
      for U in 0 .. 3 loop
         Hash_Value(U) := Hermes.Octet(ShiftR(Context.H(0),24 - U * 8) And 16#000000ff#);
         Hash_Value(U + 4) := Hermes.Octet(ShiftR(Context.H(1),24 - U * 8) And 16#000000ff#);
         Hash_Value(U + 8) := Hermes.Octet(ShiftR(Context.H(2),24 - U * 8) And 16#000000ff#);
         Hash_Value(U + 12) := Hermes.Octet(ShiftR(Context.H(3),24 - U * 8) And 16#000000ff#);
         Hash_Value(U + 16) := Hermes.Octet(ShiftR(Context.H(4),24 - U * 8) And 16#000000ff#);
         Hash_Value(U + 20) := Hermes.Octet(ShiftR(Context.H(5),24 - U * 8) And 16#000000ff#);
         Hash_Value(U + 24) := Hermes.Octet(ShiftR(Context.H(6),24 - U * 8) And 16#000000ff#);
         Hash_Value(U + 28) := Hermes.Octet(ShiftR(Context.H(7),24 - U * 8) And 16#000000ff#);
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
