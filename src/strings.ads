---------------------------------------------------------------------------
-- FILE    : strings.ads
-- SUBJECT : Specification of a string handling package.
-- AUTHOR  : (C) Copyright 2013 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Network;

--# inherit Network;
package Strings is

   -- Converts Text into a sequence of Octets. If all characters of Text were converted, Complete is True (otherwise False).
   procedure To_Octet_Array
     (Text : in String; Data : out Network.Octet_Array; Octet_Count : out Natural; Complete : out Boolean);
   --# derives Data        from Text &
   --#         Octet_Count from Text &
   --#         Complete    from Text;

end Strings;
