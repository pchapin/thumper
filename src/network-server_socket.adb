---------------------------------------------------------------------------
-- FILE    : network-server_socket.adb
-- SUBJECT : Body of a boundary variable package represening the server socket.
-- AUTHOR  : (C) Copyright 2013 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package body Network.Server_Socket is

   Not_Implemented : exception;

   procedure Create_Socket(Port : in Addresses.Port_Type; Status : out Status_Type) is
   begin
      raise Not_Implemented;
   end Create_Socket;


   procedure Receive
     (Data        : out Network.Octet_Array;
      Octet_Count : out Natural;
      Address     : out Addresses.UDPv4;
      Status      : out Status_Type) is
   begin
      raise Not_Implemented;
   end Receive;


   procedure Send
     (Address     : in Addresses.UDPv4;
      Data        : in Network.Octet_Array;
      Octet_Count : in Natural) is
   begin
      raise Not_Implemented;
   end Send;


   procedure Close is
   begin
      raise Not_Implemented;
   end Close;

end Network.Server_Socket;
