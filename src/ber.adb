---------------------------------------------------------------------------
-- FILE    : ber.adb
-- SUBJECT : Body of a package that encapsulates subprograms that handle the basic encoding rules.
-- AUTHOR  : (C) Copyright 2013 by Peter Chapin and John McCormick
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package body BER is

   function Make_Universal_Tag
     (Tag_Class       : Tag_Class_Type;
      Structured_Flag : Structured_Flag_Type;
      Tag             : Universal_Tag_Type) return Network.Octet is

      type Tag_Class_Lookup_Type is array(Tag_Class_Type) of Network.Octet;
      type Structured_Flag_Lookup_Type is array(Structured_Flag_Type) of Network.Octet;

      Tag_Class_Lookup_Table : constant Tag_Class_Lookup_Type := Tag_Class_Lookup_Type'
        (Class_Universal        => 2#0000_0000#,
         Class_Application      => 2#0100_0000#,
         Class_Context_Specific => 2#1000_0000#,
         Class_Private          => 2#1100_0000#);

      Structured_Flag_Lookup_Table : constant Structured_Flag_Lookup_Type := Structured_Flag_Lookup_Type'
        (Primitive   => 2#0000_0000#,
         Constructed => 2#0010_0000#);

   begin
      return Tag_Class_Lookup_Table(Tag_Class) or Structured_Flag_Lookup_Table(Structured_Flag) or Network.Octet(Tag);
   end Make_Universal_Tag;

end BER;
