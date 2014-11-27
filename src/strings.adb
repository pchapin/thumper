---------------------------------------------------------------------------
-- FILE    : strings.adb
-- SUBJECT : Body of a string handling package.
-- AUTHOR  : (C) Copyright 2014 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Strings is

   ---------------------
   -- Public Subprograms
   ---------------------

   procedure To_Octet_Array
     (Text : in String; Data : out Network.Octet_Array; Octet_Count : out Natural) is
   begin
      Data := (others => 0);
      Octet_Count := Text'Length;
      if Data'Length < Octet_Count then
         Octet_Count := Data'Length;
      end if;

      for I in Natural range 1 .. Octet_Count loop
         Data(Data'First + (I - 1)) := Character'Pos(Text(Text'First + (I - 1)));
      end loop;
   end To_Octet_Array;

end Strings;
