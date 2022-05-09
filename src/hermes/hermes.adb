---------------------------------------------------------------------------
-- FILE    : hermes.adb
-- SUBJECT : Implementation of the top level package of the Hermes ASN.1 library.
-- AUTHOR  : (C) Copyright 2022 by Peter C. Chapin
--
-- This package contains various utility subprograms of use to the entire library.
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <pchapin@vtc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

use Ada.Strings;
use Ada.Strings.Fixed;
use Ada.Strings.Maps;

package body Hermes is

   procedure String_To_Octet(Text : in Hex_String; Octet_Value : out Octet) is

      function Lookup_Octet(Ch : in Character) return Octet is
          Lookup_Array_Decimal : constant array (Character range '0' .. '9') of Octet :=
           (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
         Lookup_Array_Extra   : constant array (Character range 'A' .. 'F') of Octet :=
           (10, 11, 12, 13, 14, 15);
      begin
         if Ch in '0' .. '9' then
            return Lookup_Array_Decimal(Ch);
         else
            return Lookup_Array_Extra(Ch);
         end if;
      end Lookup_Octet;

   begin
      Octet_Value := 16 * Lookup_Octet(Text(Text'First)) + Lookup_Octet(Text(Text'Last));
   end String_To_Octet;


   procedure String_To_Octet_Array(Text : in Readible_Hex_String; Result : out Octet_Array) is
      Number    : Natural := 0;         -- Component_Array index value
      Component : Octet;
      Alpha     : constant Character_Set  := To_Set(' ');
      Iterator  : Natural := 1;
      Lower     : Positive;             -- Lower index location for numbers
      Upper     : Natural;              -- Upper index location for numbers
   begin
      while Iterator in Text'Range loop
         Find_Token
           (Source => Text,
            Set    => Alpha,
            From   => Iterator,
            Test   => Outside,
            First  => Lower,
            Last   => Upper);
      exit when Upper = 0;
         String_To_Octet(Text(Lower .. Upper), Component);
         Number := Number + 1;
         Result(Number) := Component;
         Iterator := Upper + 1;
      end loop;
   end String_To_Octet_Array;


   function Octets_To_String(Octets : in Octet_Array) return String is
      Returned_String : String(1 .. 3  * Octets'Length - 1) := (others => ' ');
      Lookup_Array    : constant array(Octet range 0 .. 15) of Character :=
        ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
      Octet_Value : Octet;
      Temp_Value  : Octet;
      J           : Positive := 1;
   begin
      for I in Octets'Range loop
         Octet_Value := Octets(I);

         Temp_Value := Octet_Value / 16;
         Returned_String(J) := Lookup_Array(Temp_Value);
         J := J + 1;

         Temp_Value := Octet_Value rem 16;
         Returned_String(J) := Lookup_Array(Temp_Value);
         J := J + 2;
      end loop;
      return Returned_String;
   end Octets_To_String;

end Hermes;
