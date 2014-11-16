---------------------------------------------------------------------------
-- FILE    : strings.ads
-- SUBJECT : Specification of a string handling package.
-- AUTHOR  : (C) Copyright 2014 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Network;

package Strings is

   -- Converts Text into a sequence of Octets. On exit Octet_Count is the number of elements of
   -- Data that were actually used. This might be less than the length of Text if the Data array
   -- is not sufficiently large. Elements of Data that are not used are set to 0.
   --
   procedure To_Octet_Array
     (Text : in String; Data : out Network.Octet_Array; Octet_Count : out Natural)
   with
     Depends => ( Data => (Text, Data), Octet_Count => (Text, Data) );

end Strings;
