---------------------------------------------------------------------------
-- FILE    : hermes-der.ads
-- SUBJECT : Specification of a package for handling the distinguished encoding rules.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package Hermes.DER is

   -- Used to indicate the success or value of the subprograms in this package.
   type Status_Type is
     (Success,
      Bad_Identifier,
      Indefinite_Length,
      Unimplemented_Length,
      Bad_Length,
      Unimplemented_Value,
      Bad_Value);

   type Tag_Class_Type is
     (Class_Universal, Class_Application, Class_Context_Specific, Class_Private);

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

   -- Constructs an identifier octet from its constituent parts.
   function Make_Leading_Identifier
     (Tag_Class       : Tag_Class_Type;
      Structured_Flag : Structured_Flag_Type;
      Tag             : Leading_Number_Type) return Octet;

   -- Splits a leading identifier octet into its constituent parts. Fails with
   -- DER.Bad_Identifier if Value is an invalid first identifier octet.
   --
   procedure Split_Leading_Identifier
     (Value           : in  Octet;
      Tag_Class       : out Tag_Class_Type;
      Structured_Flag : out Structured_Flag_Type;
      Tag             : out Leading_Number_Type;
      Status          : out Status_Type)
     with
       Depends => ( (Tag_Class, Structured_Flag, Tag, Status) => Value);

   -- Examines the Message starting at position Index looking for a DER encoded length.
   --
   -- Message => The message to examine.
   -- Index   => The starting position in the message where the length is to be extracted.
   -- Stop    => The last position in the message used by the encoded length.
   -- Length  => The extracted length.</parameter>
   -- Status  => The status of the extraction (success/failure, etc).
   --
   -- Decodes a DER encoded length from the given message. Fails with DER.Indefinite_Length if
   -- the encoded length is in the indefinite form. In that case the returned Length is zero and
   -- the returned Stop is at the last octet of the encoded length as usual. Fails with
   -- DER.Bad_Length if the length is not encoded properly. In that case the returned Length is
   -- zero and the returned Stop is undefined (it will depend on the precise nature of the
   -- encoding problem). Fails with DER.Unimplemented_Length if the encoded length is too large
   -- for this implementation to handle. In that case the returned Length is zero and the
   -- returned Stop is at the last octet of the encoded length as usual.
   --
   procedure Get_Length_Value
     (Message : in  Octet_Array;
      Index   : in  Natural;
      Stop    : out Natural;
      Length  : out Natural;
      Status  : out Status_Type)
     with
       Depends => ( (Stop, Length, Status) => (Message, Index) ),
       Pre => Message'First <= Index and Index <= Message'Last;


   -- Examines the Message starting at position Index looking for a DER encoded integer.
   --
   -- Message => The message to examine.
   -- Index   => The starting position in the message where the integer is to be extracted.
   -- Stop    => The last position in the message used by the encoded integer.
   -- Value   => The extracted integer.
   -- Status  => The status of the extraction (success/failure, etc).
   --
   -- Decodes a DER encoded integer from the given message. Fails with DER.Bad_Value if the data
   -- is not encoded properly. In that case the returned Value is zero and the returned Stop is
   -- undefined (it will depend on the precise nature of the encoding problem). Fails with
   -- DER.Unimplemented_Value if the encoded value is too large for this implementation to
   -- handle. In that case the returned Value is zero and the returned Stop is at the last octet
   -- of the encoded value as usual.
   --
   procedure Get_Integer_Value
     (Message : in  Octet_Array;
      Index   : in  Natural;
      Stop    : out Natural;
      Value   : out Integer;
      Status  : out Status_Type)
     with
       Depends => ( (Stop, value, Status) => (Message, Index) ),
       Pre => Message'First <= Index and Index <= Message'Last;

end Hermes.DER;
