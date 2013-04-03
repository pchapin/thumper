---------------------------------------------------------------------------
-- FILE    : ber.ads
-- SUBJECT : Specification of a package that encapsulates subprograms that handle the basic encoding rules.
-- AUTHOR  : (C) Copyright 2013 by Peter Chapin and John McCormick
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Network;
use type Network.Octet;

--# inherit Network;
package BER is

   -- Used to indicate the success or value of the subprograms in this package.
   type Status_Type is
     (Success,
      Bad_Identifier,
      Indefinite_Length,
      Unimplemented_Length,
      Bad_Length,
      Unimplemented_Value,
      Bad_Value);

   type Tag_Class_Type is (Class_Universal, Class_Application, Class_Context_Specific, Class_Private);
   type Structured_Flag_Type is (Primitive, Constructed);
   type Leading_Number_Type is
     (Tag_Reserved,
      Tag_Boolean,
      Tag_Integer,
      Tag_Bit_String,
      Tag_Octet_String,
      Tag_Null,
      Tag_Object_Identifier,
      Tag_Object_Descriptor,
      Tag_Instance_Of,
      Tag_External,
      Tag_Real,
      Tag_Enumerated,
      Tag_Embedded_PDV,
      Tag_UTF8_String,
      Tag_Relative_OID,
      Tag_Sequence,
      Tag_Sequence_Of,
      Tag_Set,
      Tag_Set_Of,
      Tag_Numeric_String,
      Tag_Printable_String,
      Tag_Teletex_String,
      Tag_T61_String,
      Tag_Videotex_String,
      Tag_IA5_String,
      Tag_UTC_Time,
      Tag_Generalized_Time,
      Tag_Graphic_String,
      Tag_Visible_String,
      Tag_ISO646_String,
      Tag_General_String,
      Tag_Universal_String,
      Tag_Character_String,
      Tag_BMP_String,
      Tag_EXTENDED_TAG);

   function Make_Leading_Identifier
     (Tag_Class       : Tag_Class_Type;
      Structured_Flag : Structured_Flag_Type;
      Tag             : Leading_Number_Type) return Network.Octet;

   procedure Split_Leading_Identifier
     (Value           : in  Network.Octet;
      Tag_Class       : out Tag_Class_Type;
      Structured_Flag : out Structured_Flag_Type;
      Tag             : out Leading_Number_Type;
      Status          : out Status_Type);
   --# derives Tag_Class, Structured_Flag, Tag, Status from Value;

   -- Examines the Message starting at position Index looking for a BER encoded length. That length is returned in Length.
   -- The index of the last octet processed as part of the encoded length is returned in Stop.
   --  "Fails" with Indefinite_Length if the encoded length is in the indefinite form. In that case the returned value of
   --     Stop points at the last octet of the encoded length as usual and the returned length is zero.
   --   Fails with Bad_Length if the data is not encoded properly. In that case the returned length is zero.
   --   Fails with Unimplemented_Length if the encoded length is too large for this implementation to handle. In that case the
   --     returned value of Stop points at the last octet of the full length.
   --
   procedure Get_Length_Value
     (Message : in  Network.Octet_Array;
      Index   : in  Natural;
      Stop    : out Natural;
      Length  : out Natural;
      Status  : out Status_Type);
   --# derives Stop, Length, Status from Message, Index;
   --# pre Message'First <= Index and Index <= Message'Last;


   -- Examines the Message starting at position Index looking for a BER encoded integer. That integer is returned in Value.
   -- The index of the last octet processed as part of the encoded integer is returned in Stop.
   --   Fails with Bad_Value if the data is not encoded properly. In that case the returned value is zero.
   --   Fails with Unimplemented_Value if the encoded value is too large for this implementation to handle. In that case the
   --   returned value of Stop points at the last octet of the encoded value.
   --
   procedure Get_Integer_Value
     (Message : in  Network.Octet_Array;
      Index   : in  Natural;
      Stop    : out Natural;
      Value   : out Integer;
      Status  : out Status_Type);
   --# derives Stop, Value, Status from Message, Index;
   --# pre Message'First <= Index and Index <= Message'Last;
end BER;
