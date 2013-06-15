---------------------------------------------------------------------------
-- FILE    : asn1-oid.ads
-- SUBJECT : Specification of Object Identifier package.
-- AUTHOR  : (C) Copyright 2013 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Network;

package ASN1.OID is

   -- Components_Type holds object identifiers as an array of component values.
   Maximum_Component_Count : constant := 15;
   type    Component  is range 0 .. Integer'Last;
   subtype Component_Count_Type is Integer range 0 .. Maximum_Component_Count;
   subtype Component_Index_Type is Integer range 1 .. Maximum_Component_Count;
   type    Components_Type is array(Component_Index_Type range <>) of Component;
   type    Status_Type is (Success, Invalid_Root, Invalid_Second_Level, Insufficient_Space);

   type Object_Identifier is private;

   -- Converts an OID in the form of separate components into an abstract object identifier.
   procedure To_Object_Identifier(Separates : in Components_Type; Result : out Object_Identifier; Status : out Status_Type)
   with Depends => ( (Result, Status) => Separates);

   -- Returns the number of components inside the given object identifier.
   function Component_Count(Identifier : Object_Identifier) return Component_Count_Type;

   -- Converts an object identifier into an array of separate components. Returns in the Number_Of_Components parameter
   -- the number of components used. If there is a problem with the conversion (due to lack of space) a count of zero is
   -- returned. Unused space in the Result array is filled with zero component values.
   --
   procedure To_Separates
     (Identifier : Object_Identifier; Result : out Components_Type; Number_Of_Components : out Component_Count_Type)
   with Depends => ( (Result, Number_Of_Components) => Identifier );

   -- Converts an object identifier into an array of raw bytes. Returns in the Octet_Count parameter the number of bytes
   -- used. If there is a problem with the conversion (for example, due to lack of space) a count of zero is returned. Unused
   -- space in the Result array is filled with zero byte values.
   --
   procedure To_Octet_Array
     (Identifier : in Object_Identifier; Result : out Network.Octet_Array; Octet_Count : out Natural)
   with Depends => ( (Result, Octet_Count) => Identifier );

private

   -- 1.3.6.14.311.5.1007
   -- Suppose 0.40.... Then first byte of encoded OID would = 40*0 + 40 = 40
   -- Suppose 1.0...   Then first byte of encoded OID would = 40*1 + 0  = 40 ambiguous!
   -- Rule: If the first OID subcomponent is 0 or 1. The second subcomponent is 0 .. 39
   --       Thus if first OID subcomponent is 2, the second subcomponent must be 175 at most. (2*40 + 175 = 255)

   subtype Root_Component_Type is Component range 0 .. 2;
   subtype Second_Level_Component_Type is Component range 0 .. 175;
   subtype Other_Index_Type is Component_Index_Type range 1 .. Component_Index_Type'Last - 2;
   subtype Other_Count_Type is Integer range 0 .. Other_Index_Type'Last;
   type    Other_Components_Type is array(Other_Index_Type) of Component;

   type Object_Identifier is
      record
         Root_Component         : Root_Component_Type;
         Second_Level_Component : Second_Level_Component_Type;
         Other_Components       : Other_Components_Type;
         Other_Component_Count  : Other_Count_Type;
      end record;

end ASN1.OID;
