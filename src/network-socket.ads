---------------------------------------------------------------------------
-- FILE    : network-socket.ads
-- SUBJECT : Specification of a boundary variable package representing a single socket.
-- AUTHOR  : (C) Copyright 2014 by Peter C. Chapin
--
-- This package encapsulates the vendor's sockets library so clients are not aware of which
-- underlying library is used. Thus switching to a different sockets library only entails
-- changing the body of this package. This package also provides a simplified, easier to use
-- interface exposing the abstraction of a single UDP socket (either client or server). All
-- errors are reported with the Network_Error exception; exceptions from the underlying library
-- are translated to Network_Error.
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Messages;
with Network.Addresses;

package Network.Socket is

   -- A general purpose exception used for all kinds of network problems.
   Network_Error : exception;

   -- This procedure creates the socket without binding it to a specified port.
   -- It is useful for clients.
   procedure Create_Socket;

   -- This procedure creates a socket and binds it to the specified port.
   -- It is useful for servers.
   procedure Create_And_Bind_Socket(Port : in Addresses.Port_Type);

   -- This procedure receives a datagram. It also returns the source address.
   procedure Receive(Message : out Messages.Network_Message; From : out Addresses.UDPv4);

   -- This procedure sends a datagram to the given destination address.
   procedure Send(Message : in Messages.Network_Message; To : in Addresses.UDPv4);

   -- This procedure closes the socket. After being closed the socket should not be used.
   procedure Close;

end Network.Socket;
