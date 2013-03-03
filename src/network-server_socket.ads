---------------------------------------------------------------------------
-- FILE    : network-server_socket.ads
-- SUBJECT : Specification of a boundary variable package represening the server socket.
-- AUTHOR  : (C) Copyright 2013 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Network.Addresses;

--# inherit Network.Addresses;
package Network.Server_Socket
--# own State; in Input; out Output;
--# initializes State;
is

   type Status_Type is (Success, Receive_Failure, Send_Failure);

   procedure Create_Socket(Port : in Addresses.Port_Type; Status : out Status_Type);
   --# global in out State;
   --# derives State  from State, Port &
   --#         Status from State, Port;

   -- This procedure receives a datagram from the given socket into the indicated array.
   procedure Receive
     (Data        : out Network.Octet_Array;
      Octet_Count : out Natural;
      Address     : out Addresses.UDPv4;
      Status      : out Status_Type);
   --# global in State, Input;
   --# derives Data        from Input, State &
   --#         Octet_Count from Input, State &
   --#         Address     from Input, State &
   --#         Status      from Input, State;

   -- This procedure sends a datagram to the given socket. There is no error indication.
   procedure Send
     (Address     : in Addresses.UDPv4;
      Data        : in Network.Octet_Array;
      Octet_Count : in Natural);
   --# global in State; out Output;
   --# derives Output from Address, Data, Octet_Count, State;

   -- This procedure closes the indicated socket. After being closed the socket should not be used.
   procedure Close;
   --# global in out State;
   --# derives State from State;

end Network.Server_Socket;
