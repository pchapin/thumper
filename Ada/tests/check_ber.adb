---------------------------------------------------------------------------
-- FILE    : check_ber.adb
-- SUBJECT : Package containing tests of the ASN.1 Basic Encoding Rules.
-- AUTHOR  : (C) Copyright 2013 by Peter Chapin and John McCormick
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with AUnit.Assertions; use AUnit.Assertions;
with BER;
with Network;

package body Check_BER is
   use type BER.Status_Type;

   procedure Test_Get_Length(T : in out AUnit.Test_Cases.Test_Case'Class) is
      type Octet_Array_Access is access Network.Octet_Array;

      type Input_Record is
         record
            Data  : Octet_Array_Access;
            Start : Natural;
         end record;

      type Output_Record is
         record
            Stop   : Natural;
            Length : Natural;
            Status : BER.Status_Type;
         end record;

      type Test_Case is
         record
            Input    : Input_Record;
            Expected : Output_Record;
         end record;

      subtype Array_1_Type is Network.Octet_Array(1 .. 1);
      subtype Array_2_Type is Network.Octet_Array(1 .. 2);
      subtype Array_3_Type is Network.Octet_Array(1 .. 3);
      subtype Array_4_Type is Network.Octet_Array(1 .. 4);
      subtype Array_5_Type is Network.Octet_Array(1 .. 5);
      subtype Array_6_Type is Network.Octet_Array(1 .. 6);

      Test_Cases : array(1 .. 16) of Test_Case :=
        -- Correctly formatted definite form using the short encoding.
        ( 1 => (Input    => (Data => new Array_1_Type'(1 => 2#0000_0000#), Start => 1),
                Expected => (Stop => 1, Length => 0, Status => BER.Success)),
          2 => (Input    => (Data => new Array_1_Type'(1 => 2#0000_0001#), Start => 1),
                Expected => (Stop => 1, Length => 1, Status => BER.Success)),
          3 => (Input    => (Data => new Array_1_Type'(1 => 2#0111_1111#), Start => 1),
                Expected => (Stop => 1, Length => 127, Status => BER.Success)),

          -- Indefinite length.
          4 => (Input    => (Data => new Array_1_Type'(1 => 2#1000_0000#), Start => 1),
                Expected => (Stop => 1, Length => 0, Status => BER.Indefinite_Length)),

          -- Reserved encoding.
          5 => (Input    => (Data => new Array_1_Type'(1 => 2#1111_1111#), Start => 1),
                Expected => (Stop => 1, Length => 0, Status => BER.Bad_Length)),

          -- Correctly formatted definite form using the long encoding.
          6 => (Input    => (Data => new Array_2_Type'(2#1000_0001#, 0), Start => 1),
                Expected => (Stop => 2, Length => 0, Status => BER.Success)),
          7 => (Input    => (Data => new Array_2_Type'(2#1000_0001#, 1), Start => 1),
                Expected => (Stop => 2, Length => 1, Status => BER.Success)),
          8 => (Input    => (Data => new Array_2_Type'(2#1000_0001#, 255), Start => 1),
                Expected => (Stop => 2, Length => 255, Status => BER.Success)),
          9 => (Input    => (Data => new Array_3_Type'(2#1000_0010#, 0, 1), Start => 1),
                Expected => (Stop => 3, Length => 1, Status => BER.Success)),
         10 => (Input    => (Data => new Array_3_Type'(2#1000_0010#, 1, 0), Start => 1),
                Expected => (Stop => 3, Length => 256, Status => BER.Success)),
         11 => (Input    => (Data => new Array_3_Type'(2#1000_0010#, 255, 255), Start => 1),
                Expected => (Stop => 3, Length => 2**16 - 1, Status => BER.Success)),
         12 => (Input    => (Data => new Array_4_Type'(2#1000_0011#, 255, 255, 255), Start => 1),
                Expected => (Stop => 4, Length => 2**24 - 1, Status => BER.Success)),
         13 => (Input    => (Data => new Array_5_Type'(2#1000_0100#, 127, 255, 255, 255), Start => 1),
                Expected => (Stop => 5, Length => 2**31 - 1, Status => BER.Success)),

         -- Unimplemented lengths.
         14 => (Input    => (Data => new Array_5_Type'(2#1000_0100#, 128, 0, 0, 0), Start => 1),
                Expected => (Stop => 5, Length => 0, Status => BER.Unimplemented_Length)),
         15 => (Input    => (Data => new Array_6_Type'(2#1000_0101#, 0, 0, 0, 0, 1), Start => 1),
                Expected => (Stop => 6, Length => 0, Status => BER.Unimplemented_Length)),

         -- Various error cases.
         16 => (Input    => (Data => new Array_5_Type'(2#1000_0101#, 0, 0, 0, 0), Start => 1),
                Expected => (Stop => 5, Length => 0, Status => BER.Bad_Length)));

      Test_Stop   : Natural;
      Test_Length : Natural;
      Test_Status : BER.Status_Type;
   begin
      for I in Test_Cases'Range loop
         BER.Get_Length_Value(Test_Cases(I).Input.Data.all, Test_Cases(I).Input.Start, Test_Stop, Test_Length, Test_Status);
         Assert
           (Test_Stop   = Test_Cases(I).Expected.Stop   and
            Test_Length = Test_Cases(I).Expected.Length and
            Test_Status = Test_Cases(I).Expected.Status, "Test case #" & Integer'Image(I) & " failed");
      end loop;
   end Test_Get_Length;


   procedure Register_Tests(T : in out BER_Test) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine(T, Test_Get_Length'Access, "Get Length");
   end Register_Tests;


   function Name(T : BER_Test) return AUnit.Message_String is
   begin
      return AUnit.Format("BER");
   end Name;

end Check_BER;
