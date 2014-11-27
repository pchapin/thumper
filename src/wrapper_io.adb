---------------------------------------------------------------------------
-- FILE    : wrapper_io.adb
-- SUBJECT : Implementation of the basic I/O wrapper package.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(Off);

with Ada.Text_IO;

package body Wrapper_IO is

   procedure Put_Line(Text : String) is
   begin
      Ada.Text_IO.Put_Line(Text);
   end Put_Line;

end Wrapper_IO;
