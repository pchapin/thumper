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

   type Tag_Class_Type is (Class_Universal, Class_Application, Class_Context_Specific, Class_Private);
   type Structured_Flag_Type is (Primitive, Constructed);

   -- Universal tags.
   type Universal_Tag_Type is range 0 .. 31;

   Tag_Reserved           : constant Universal_Tag_Type := 0;
   Tag_Boolean            : constant Universal_Tag_Type := 1;
   Tag_Integer            : constant Universal_Tag_Type := 2;
   Tag_Bit_String         : constant Universal_Tag_Type := 3;
   Tag_Octet_String       : constant Universal_Tag_Type := 4;
   Tag_Null               : constant Universal_Tag_Type := 5;
   Tag_Object_Identifier  : constant Universal_Tag_Type := 6;
   Tag_Object_Descriptor  : constant Universal_Tag_Type := 7;
   Tag_Instance_Of        : constant Universal_Tag_Type := 8;
   Tag_External           : constant Universal_Tag_Type := 8;  -- Same as Instance_Of
   Tag_Real               : constant Universal_Tag_Type := 9;
   Tag_Enumerated         : constant Universal_Tag_Type := 10;
   Tag_Embedded_PDV       : constant Universal_Tag_Type := 11;
   Tag_UTF8_String        : constant Universal_Tag_Type := 12;
   Tag_Relative_OID       : constant Universal_Tag_Type := 13;
   -- Values 14 .. 15 omitted (not defined?)
   Tag_Sequence           : constant Universal_Tag_Type := 16;
   Tag_Sequence_Of        : constant Universal_Tag_Type := 16;  -- Same as Sequence
   Tag_Set                : constant Universal_Tag_Type := 17;
   Tag_Set_Of             : constant Universal_Tag_Type := 17;  -- Same as Set
   Tag_Numeric_String     : constant Universal_Tag_Type := 18;
   Tag_Printable_String   : constant Universal_Tag_Type := 19;
   Tag_Teletex_String     : constant Universal_Tag_Type := 20;
   Tag_T61_String         : constant Universal_Tag_Type := 20;  -- Same as Teletex_String
   Tag_Videotex_String    : constant Universal_Tag_Type := 21;
   Tag_IA5_String         : constant Universal_Tag_Type := 22;
   Tag_UTC_Time           : constant Universal_Tag_Type := 23;
   Tag_Generalized_Time   : constant Universal_Tag_Type := 24;
   Tag_Graphic_String     : constant Universal_Tag_Type := 25;
   Tag_Visible_String     : constant Universal_Tag_Type := 26;
   Tag_ISO646_String      : constant Universal_Tag_Type := 26;  -- Same as Visible_String
   Tag_General_String     : constant Universal_Tag_Type := 27;
   Tag_Universal_String   : constant Universal_Tag_Type := 28;
   Tag_Character_String   : constant Universal_Tag_Type := 29;
   Tag_BMP_String         : constant Universal_Tag_Type := 30;
   Tag_EXTENDED_TAG       : constant Universal_Tag_Type := 31;

   function Make_Universal_Tag
     (Tag_Class       : Tag_Class_Type;
      Structured_Flag : Structured_Flag_Type;
      Tag             : Universal_Tag_Type) return Network.Octet;
end BER;
