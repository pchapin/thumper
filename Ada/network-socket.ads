---------------------------------------------------------------------------
-- FILE    : network-socket.ads
-- SUBJECT : Specification of a boundary variable package representing a single socket.
-- AUTHOR  : (C) Copyright 2014 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Network.Addresses;

package Network.Socket
  with
     Abstract_State => (State, (Network_Stack with External)),
     Initializes => (State, Network_Stack)
is

   type Status_Type is (Success, Create_Failure, Receive_Failure);

   -- This procedure creates a socket without binding it to a specified port.
   procedure Create_Socket(Status : out Status_Type)
     with
       Global => (In_Out => State),
       Depends => ( (State, Status) => State );

   -- This procedure creates a socket and binds it to the specified port.
   procedure Create_And_Bind_Socket(Port : in Addresses.Port_Type; Status : out Status_Type)
     with
       Global => (In_Out => State),
       Depends => ( (State, Status) => (State, Port) );

   -- This procedure receives a datagram from the given socket into the indicated array.
   procedure Receive
     (Data        : out Network.Octet_Array;
      Octet_Count : out Natural;
      Address     : out Addresses.UDPv4;
      Status      : out Status_Type)
     with
       Global => ( Input => (State, Network_Stack) ),
       Depends => ( (Data, Octet_Count, Address, Status) => (State, Network_Stack) );


   -- This procedure sends a datagram to the given socket. There is no error indication.
   -- TODO: Should Send return a Status value to indicate failure (or maybe we don't care about send failures)?
   procedure Send
     (Address     : in Addresses.UDPv4;
      Data        : in Network.Octet_Array;
      Octet_Count : in Natural)
     with
       Global => (Input => State, Output => Network_Stack),
       Depends => ( Network_Stack => (Address, Data, Octet_Count, State) );

   -- This procedure closes the indicated socket. After being closed the socket should not be used.
   procedure Close
     with
       Global => (In_Out => State),
       Depends => (State =>+ null);

end Network.Socket;
