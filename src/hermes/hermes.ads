---------------------------------------------------------------------------
-- FILE    : hermes.ads
-- SUBJECT : Top level package of the Hermes ASN.1 library.
-- AUTHOR  : (C) Copyright 2022 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <pchapin@vtc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package Hermes is

   type Octet is mod 2**8;
   type Octet_Array is array(Natural range <>) of Octet;

   -- Converts an array of octets to a space-delimited collection of hex values.
   function Octets_To_String(Octets : in Octet_Array) return String;

   subtype Hex_Digit_Type is Character
     with Static_Predicate => Hex_Digit_Type in '0' .. '9' | 'A' .. 'F';
   subtype Hex_String is String
     with Dynamic_Predicate => (for all Ch of Hex_String => Ch in Hex_Digit_Type);
   subtype Readible_Hex_String is String
     with Dynamic_Predicate => (for all Ch of Readible_Hex_String => Ch in Hex_Digit_Type | ' ');

   -- Converts a string of two hex digits to an octet. The digits 'A' .. 'F' must be uppercase.
   procedure String_To_Octet(Text : in Hex_String; Octet_Value : out Octet)
     with Pre => Text'Length = 2;

   -- Converts a string of space delimited octet values expressed in hex into an octet array.
   procedure String_To_Octet_Array(Text : in Readible_Hex_String; Result : out Octet_Array);

end Hermes;
