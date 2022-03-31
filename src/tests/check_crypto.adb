---------------------------------------------------------------------------
-- FILE    : check_crypto.adb
-- SUBJECT : Package containing tests of the cryptographic services.
-- AUTHOR  : (C) Copyright 2015 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
with AUnit.Assertions;       use AUnit.Assertions;
with Cryptographic_Services; use Cryptographic_Services;
with Hermes;

package body Check_Crypto is

   procedure Test_Hashing(T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      use type Hermes.Octet_Array;

      Input_1 : constant Hermes.Octet_Array(1 .. 5) :=
        (72, 101, 108, 108, 111);  -- "Hello"
      Expected_1 : constant Hermes.Octet_Array(1 .. 32) :=
        (16#18#, 16#5f#, 16#8d#, 16#b3#, 16#22#, 16#71#, 16#fe#, 16#25#,
         16#f5#, 16#61#, 16#a6#, 16#fc#, 16#93#, 16#8b#, 16#2e#, 16#26#,
         16#43#, 16#06#, 16#ec#, 16#30#, 16#4e#, 16#da#, 16#51#, 16#80#,
         16#07#, 16#d1#, 16#76#, 16#48#, 16#26#, 16#38#, 16#19#, 16#69#);

      Input_2 : constant Hermes.Octet_Array(1 .. 13) :=
        (72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33); -- "Hello, World!"
      Expected_2 : constant Hermes.Octet_Array(1 .. 32) :=
        (16#df#, 16#fd#, 16#60#, 16#21#, 16#bb#, 16#2b#, 16#d5#, 16#b0#,
         16#af#, 16#67#, 16#62#, 16#90#, 16#80#, 16#9e#, 16#c3#, 16#a5#,
         16#31#, 16#91#, 16#dd#, 16#81#, 16#c7#, 16#f7#, 16#0a#, 16#4b#,
         16#28#, 16#68#, 16#8a#, 16#36#, 16#21#, 16#82#, 16#98#, 16#6f#);

      Expected_3 : constant Hermes.Octet_Array(1 .. 32) :=
        (16#e3#, 16#b0#, 16#c4#, 16#42#, 16#98#, 16#fc#, 16#1c#, 16#14#,
         16#9a#, 16#fb#, 16#f4#, 16#c8#, 16#99#, 16#6f#, 16#b9#, 16#24#,
         16#27#, 16#ae#, 16#41#, 16#e4#, 16#64#, 16#9b#, 16#93#, 16#4c#,
         16#a4#, 16#95#, 16#99#, 16#1b#, 16#78#, 16#52#, 16#b8#, 16#55#);

      Input_3 : constant Hermes.Octet_Array(1 .. 1) :=
        (1=>115);  -- "s"
      Expected_4 : constant Hermes.Octet_Array(1 .. 32) :=
        (16#04#, 16#3a#, 16#71#, 16#87#, 16#74#, 16#c5#, 16#72#, 16#bd#,
         16#8a#, 16#25#, 16#ad#, 16#be#, 16#b1#, 16#bf#, 16#cd#, 16#5c#,
         16#02#, 16#56#, 16#ae#, 16#11#, 16#ce#, 16#cf#, 16#9f#, 16#9c#,
         16#3f#, 16#92#, 16#5d#, 16#0e#, 16#52#, 16#be#, 16#af#, 16#89#);

      Input_4 : constant Hermes.Octet_Array(1 .. 35) :=
        (49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,
         54,55,56,57,48,49,50,51,52,53,54,55,56,57,48,
         49,50,51,52,53);  -- "12345678901234567890123456789012345"
      Expected_5 : constant Hermes.Octet_Array(1 .. 32) :=
        (16#48#, 16#d7#, 16#38#, 16#ca#, 16#1c#, 16#30#, 16#fd#, 16#ae#,
         16#fd#, 16#88#, 16#aa#, 16#83#, 16#6a#, 16#26#, 16#f4#, 16#89#,
         16#8d#, 16#e8#, 16#b9#, 16#20#, 16#e6#, 16#47#, 16#2c#, 16#74#,
         16#af#, 16#e8#, 16#ab#, 16#44#, 16#a2#, 16#ba#, 16#16#, 16#49#);

      Input_5 : constant Hermes.Octet_Array(1 .. 36) :=
        (49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,
         54,55,56,57,48,49,50,51,52,53,54,55,56,57,48,
         49,50,51,52,53,54);  -- "123456789012345678901234567890123456"
      Expected_6 : constant Hermes.Octet_Array(1 .. 32) :=
        (16#de#, 16#38#, 16#46#, 16#95#, 16#4e#, 16#38#, 16#b3#, 16#43#,
         16#9c#, 16#32#, 16#e7#, 16#14#, 16#70#, 16#31#, 16#a4#, 16#3a#,
         16#4b#, 16#00#, 16#7b#, 16#26#, 16#12#, 16#20#, 16#f0#, 16#36#,
         16#81#, 16#3c#, 16#34#, 16#08#, 16#da#, 16#60#, 16#78#, 16#a3#);

      Input_6 : constant Hermes.Octet_Array(1 .. 1_000_000) :=
        (others => 97); -- 1,000,000 'a's
      Expected_7 : constant Hermes.Octet_Array(1 .. 32) :=
        (16#cd#, 16#c7#, 16#6e#, 16#5c#, 16#99#, 16#14#, 16#fb#, 16#92#,
         16#81#, 16#a1#, 16#c7#, 16#e2#, 16#84#, 16#d7#, 16#3e#, 16#67#,
         16#f1#, 16#80#, 16#9a#, 16#48#, 16#a4#, 16#97#, 16#20#, 16#0e#,
         16#04#, 16#6d#, 16#39#, 16#cc#, 16#c7#, 16#11#, 16#2c#, 16#d0#);


      Context    : SHA256_CTX;
      Hash_Value : SHA256_Hash_Type;

   begin
      Initialize_Hash(Context);
      Update_Hash(Context, Input_1);
      Finalize_Hash(Context, Hash_Value);
      Assert(Hash_Value = Expected_1, "SHA256 hash of Input_1 does not check");

      Initialize_Hash(Context);
      Update_Hash(Context, Input_2( 1 .. 10));
      Update_Hash(Context, Input_2(11 .. 13));
      Finalize_Hash(Context, Hash_Value);
      Assert(Hash_Value = Expected_2, "SHA256 hash of Input_2 does not check");

      Initialize_Hash(Context);
      Finalize_Hash(Context, Hash_Value);
      Assert(Hash_Value = Expected_3, "SHA256 hash of Input_empty does not check");

      Initialize_Hash(Context);
      Update_Hash(Context, Input_3);
      Finalize_Hash(Context, Hash_Value);
      Assert(Hash_Value = Expected_4, "SHA256 hash of Input_3 does not check");

      Initialize_Hash(Context);
      Update_Hash(Context, Input_4);
      Finalize_Hash(Context, Hash_Value);
      Assert(Hash_Value = Expected_5, "SHA256 hash of Input_4 does not check");

      Initialize_Hash(Context);
      Update_Hash(Context, Input_5( 1 .. 10));
      Update_Hash(Context, Input_5(11 .. 20));
      Update_Hash(Context, Input_5(21 .. 36));
      Finalize_Hash(Context, Hash_Value);
      Assert(Hash_Value = Expected_6, "SHA256 hash of Input_5 does not check");

      Initialize_Hash(Context);
      Update_Hash(Context, Input_6);
      Finalize_Hash(Context, Hash_Value);
      Assert(Hash_Value = Expected_7, "SHA256 hash of Input_6 does not check");
   end Test_Hashing;


   procedure Register_Tests(T : in out Crypto_Test) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine(T, Test_Hashing'Access, "Hashing");
   end Register_Tests;


   function Name(T : Crypto_Test) return AUnit.Message_String is
      pragma Unreferenced(T);
   begin
      return AUnit.Format("Crypto");
   end Name;

end Check_Crypto;
