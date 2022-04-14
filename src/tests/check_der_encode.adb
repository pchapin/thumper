---------------------------------------------------------------------------
-- FILE    : check_der_encode.adb
-- SUBJECT : Package containing tests of ASN.1 DER encoding.
-- AUTHOR  : (C) Copyright 2015 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
with AUnit.Assertions;
with Hermes.DER.Encode;
with Hermes.OID;

use AUnit.Assertions;
use Hermes;
use Hermes.DER;
use Hermes.DER.Encode;
use Hermes.OID;

package body Check_DER_Encode is

   procedure Test_Put_Length(T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      type Output_Record is
         record
            Value : Hermes.Octet_Array(1 .. 5);
            Size : Positive;
         end record;

      type Test_Case is
         record
            Input : Natural;
            Expected : Output_Record;
         end record;

      Test_Cases : constant array(1 .. 10) of Test_Case :=
        ( 1 => (Input => 0,
                Expected => (Value => (16#00#, others => 0), Size => 1)),
          2 => (Input => 1,
                Expected => (Value => (16#01#, others => 0), Size => 1)),
          3 => (Input => 127,
                Expected => (Value => (16#7F#, others => 0), Size => 1)),
          4 => (Input => 128,
                Expected => (Value => (16#81#, 16#80#, others => 0), Size => 2)),
          5 => (Input => 255,
                Expected => (Value => (16#81#, 16#FF#, others => 0), Size => 2)),
          6 => (Input => 256,
                Expected => (Value => (16#82#, 16#01#, 16#00#, others => 0), Size => 3)),
          7 => (Input => 16#FFFF#,
                Expected => (Value => (16#82#, 16#FF#, 16#FF#, others => 0), Size => 3)),
          8 => (Input => 16#1_0000#,
                Expected => (Value => (16#83#, 16#01#, 16#00#, 16#00#, others => 0), Size => 4)),
          9 => (Input => 16#FF_0000#,
                Expected => (Value => (16#83#, 16#FF#, 16#00#, 16#00#, others => 0), Size => 4)),
         10 => (Input => 16#7FFF_FFFF#,
                Expected => (Value => (16#84#, 16#7F#, 16#FF#, 16#FF#, 16#FF#), Size => 5)));

   begin
      for I in Test_Cases'Range loop
         declare
            Result : constant Hermes.Octet_Array := Put_Length_Value(Test_Cases(I).Input);
         begin
            Assert
              (Result'Length = Test_Cases(I).Expected.Size,
               "Test case #" & Integer'Image(I) & " length failed");
            Assert
              (Result = Test_Cases(I).Expected.Value(1 .. Test_Cases(I).Expected.Size),
               "Test case #" & Integer'Image(I) & " value failed");
         end;
      end loop;
   end Test_Put_Length;


   procedure Test_Put_Boolean(T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      Boolean_False : constant Hermes.Octet_Array := (16#01#, 16#01#, 16#00#);
      Boolean_True  : constant Hermes.Octet_Array := (16#01#, 16#01#, 16#FF#);
   begin
      Assert(Put_Boolean_Value(False) = Boolean_False, "False case failed");
      Assert(Put_Boolean_Value(True) = Boolean_True, "True case failed");
   end Test_Put_Boolean;


   procedure Test_Put_Integer(T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      type Output_Record is
         record
            Value : Hermes.Octet_Array(1 .. 6);
            Size : Positive;
         end record;

      type Test_Case is
         record
            Input : Integer;
            Expected : Output_Record;
         end record;

      -- TODO: Add test cases for negative integers.
      Test_Cases : constant array(1 .. 10) of Test_Case :=
        ( 1 => (Input => 0,
                Expected => ((16#02#, 16#01#, 16#00#, others => 0), Size => 3)),
          2 => (Input => 1,
                Expected => ((16#02#, 16#01#, 16#01#, others => 0), Size => 3)),
          3 => (Input => 127,
                Expected => ((16#02#, 16#01#, 16#7F#, others => 0), Size => 3)),
          4 => (Input => 128,
                Expected => ((16#02#, 16#02#, 16#00#, 16#80#, others => 0), Size => 4)),
          5 => (Input => 255,
                Expected => ((16#02#, 16#02#, 16#00#, 16#FF#, others => 0), Size => 4)),
          6 => (Input => 256,
                Expected => ((16#02#, 16#02#, 16#01#, 16#00#, others => 0), Size => 4)),
          7 => (Input => 16#FFFF#,
                Expected => ((16#02#, 16#03#, 16#00#, 16#FF#, 16#FF#, others => 0), Size => 5)),
          8 => (Input => 16#1_0000#,
                Expected => ((16#02#, 16#03#, 16#01#, 16#00#, 16#00#, others => 0), Size => 5)),
          9 => (Input => 16#FF_0000#,
                Expected => ((16#02#, 16#04#, 16#00#, 16#FF#, 16#00#, 16#00#), Size => 6)),
         10 => (Input => 16#7FFF_FFFF#,
                Expected => ((16#02#, 16#04#, 16#7F#, 16#FF#, 16#FF#, 16#FF#), Size => 6)));
   begin
      for I in Test_Cases'Range loop
         declare
            Result : constant Hermes.Octet_Array := Put_Integer_Value(Test_Cases(I).Input);
         begin
            Assert
              (Result'Length = Test_Cases(I).Expected.Size,
               "Test case #" & Integer'Image(I) & " length failed");
            Assert
              (Result = Test_Cases(I).Expected.Value(1 .. Test_Cases(I).Expected.Size),
               "Test case #" & Integer'Image(I) & " value failed");
         end;
      end loop;
   end Test_Put_Integer;

   procedure Test_Put_OID(T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);
      A : Object_Identifier;
      B : Hermes.OID.Status_Type;
      type Output_Record is
         record
            Value : Hermes.Octet_Array(1 .. 11);
            Size  : Positive;
         end record;

      type Test_Case is
         record
            Input    : Component_Array(1 .. 9);
            Expected : Output_Record;
         end record;
   Test_Cases : constant array(1 .. 1) of Test_Case :=
      ( 1 => (Input => (2,16,840,1,101,3,4,2,1),
              Expected => ((16#06#, 16#09#, 16#60#, 16#86#, 16#48#, 16#01#,
                           16#65#, 16#03#, 16#04#, 16#02#, 16#01#), Size => 11)));


   begin
      for I in Test_Cases'Range loop
         To_Object_Identifier(Test_Cases(I).Input, A, B);
         Assert (B = Success,
          "Test case #" & Integer'Image(I) & " conversion to object identifier failed");
         declare
            Result : constant Hermes.Octet_Array := Put_OID_Value(A);
         begin
            Assert
              (Result'Length = Test_Cases(I).Expected.Size,
               "Test case #" & Integer'Image(I) & " length failed");
            Assert
              (Result = Test_Cases(I).Expected.Value(1 .. Test_Cases(I).Expected.Size),
               "Test case #" & Integer'Image(I) & " value failed");
         end;
      end loop;
   end Test_Put_OID;

   procedure Register_Tests(T : in out DER_Encode_Test) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine(T, Test_Put_Length'Access, "Put Length");
      AUnit.Test_Cases.Registration.Register_Routine(T, Test_Put_Boolean'Access, "Put Boolean");
      AUnit.Test_Cases.Registration.Register_Routine(T, Test_Put_Integer'Access, "Put Integer");
      AUnit.Test_Cases.Registration.Register_Routine(T, Test_Put_OID'Access, "Put OID");
   end Register_Tests;


   function Name(T : DER_Encode_Test) return AUnit.Message_String is
      pragma Unreferenced(T);

   begin
      return AUnit.Format("DER.Encode");
   end Name;

end Check_DER_Encode;
