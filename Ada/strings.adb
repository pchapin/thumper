---------------------------------------------------------------------------
-- FILE    : strings.adb
-- SUBJECT : Body of a string handling package.
-- AUTHOR  : (C) Copyright 2014 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package body Strings is

   Not_Implemented : exception;

   ---------------------
   -- Public Subprograms
   ---------------------

   procedure To_Octet_Array
     (Text : in String; Data : out Network.Octet_Array; Octet_Count : out Natural; Complete : out Boolean) is
   begin
      raise Not_Implemented;
   end To_Octet_Array;

end Strings;
