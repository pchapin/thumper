---------------------------------------------------------------------------
-- FILE    : hermes-der-encode.ads
-- SUBJECT : Specification of a package for encoding DER encoded data.
-- AUTHOR  : (C) Copyright 2022 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Hermes.OID;

package Hermes.DER.Encode is

   -- Constructs an identifier octet from its constituent parts.
   function Make_Leading_Identifier
     (Tag_Class       : Tag_Class_Type;
      Structured_Flag : Structured_Flag_Type;
      Tag             : Leading_Number_Type) return Octet;

   -- Returns the DER encoded length.
   function Put_Length_Value(Length : Natural) return Hermes.Octet_Array
     with Post => Put_Length_Value'Result'Length in 1 .. 5 and Put_Length_Value'Result'First = 1;

   -- Returns the DER encoded TLV triple of a Boolean value.
   function Put_Boolean_Value(Value : Boolean) return Hermes.Octet_Array
     with Post => Put_Boolean_Value'Result'Length = 3;

   -- Returns the DER encoded TLV triple of an integer value.
   function Put_Integer_Value(Value : Integer) return Hermes.Octet_Array;

   -- Returns the DER encoded TLV triple of an octet string.
   function Put_Octet_String_Value(Value : Hermes.Octet_Array) return Hermes.Octet_Array;

   -- Returns the DER encoded TLV triple of a null value.
   function Put_Null_Value return Hermes.Octet_Array;

   -- Returns the DER encoded TLV triple of an object identifier value.
   function Put_OID_Value(Value : Hermes.OID.Object_Identifier) return Hermes.Octet_Array;

end Hermes.DER.Encode;
