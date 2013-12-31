---------------------------------------------------------------------------
-- FILE    : messages.ads
-- SUBJECT : Specification of a package that defines the basic message type exchanged.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Network;

package Messages is

   subtype Index_Type is Positive range 1 .. 512;
   subtype Count_Type is Natural  range 0 .. Index_Type'Last;
   subtype Message is Network.Octet_Array(Index_Type);

end Messages;
