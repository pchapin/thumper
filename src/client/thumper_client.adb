---------------------------------------------------------------------------
-- FILE    : thumper_client.adb
-- SUBJECT : Main procedure of the Thumper client.
-- AUTHOR  : (C) Copyright 2015 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Network.Socket;

procedure Thumper_Client is
begin
   Network.Socket.Create_Socket;
end Thumper_Client;
