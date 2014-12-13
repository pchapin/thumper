---------------------------------------------------------------------------
-- FILE    : network-socket.adb
-- SUBJECT : Body of a boundary variable package representing a single socket.
-- AUTHOR  : (C) Copyright 2014 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Exceptions;
with Ada.Streams;
with GNAT.Sockets;

use Ada.Exceptions;

package body Network.Socket is
   use type Ada.Streams.Stream_Element_Offset;

   -- A single global socket.
   Socket : GNAT.Sockets.Socket_Type;


   procedure Create_Socket is
   begin
      GNAT.Sockets.Create_Socket
        (Socket, GNAT.Sockets.Family_Inet, GNAT.Sockets.Socket_Datagram);
   exception
      when Ex : others =>
         raise Network_Error with Exception_Message(Ex);
   end Create_Socket;


   procedure Create_And_Bind_Socket(Port : in Addresses.Port_Type) is
   begin
      GNAT.Sockets.Create_Socket
        (Socket, GNAT.Sockets.Family_Inet, GNAT.Sockets.Socket_Datagram);
      GNAT.Sockets.Bind_Socket
        (Socket, (Family => GNAT.Sockets.Family_Inet,
                  Addr   => GNAT.Sockets.Any_Inet_Addr,
                  Port   => GNAT.Sockets.Port_Type(Port)));
   exception
      when Ex : others =>
         raise Network_Error with Exception_Message(Ex);
   end Create_And_Bind_Socket;


   procedure Receive
     (Data        : out Network.Octet_Array;
      Octet_Count : out Natural;
      Address     : out Addresses.UDPv4) is

      IP_Address         : Addresses.IPv4;
      Address_Status     : Addresses.Status_Type;
      GNAT_Style_Address : GNAT.Sockets.Sock_Addr_Type;
      Elements           : Ada.Streams.Stream_Element_Array(0 .. Data'Length - 1);
      Last               : Ada.Streams.Stream_Element_Offset;
   begin
      GNAT.Sockets.Receive_Socket(Socket, Elements, Last, GNAT_Style_Address);

      -- Convert the received Ada.Streams.Stream_Element_Array into a Network.Octet_Array.
      for I in Elements'First .. Last loop
         Data(Data'First + Natural(I - Elements'First)) := Network.Octet(Elements(I));
      end loop;
      Octet_Count := Natural((Last - Elements'First) + 1);

      -- Convert GNAT address to an IP address.
      -- The address should be valid so conversion can't fail.
      Addresses.To_IPv4_Address
        (GNAT.Sockets.Image(GNAT_Style_Address.Addr), IP_Address, Address_Status);
      Address :=
        Addresses.To_UDPv4_Address(IP_Address, Addresses.Port_Type(GNAT_Style_Address.Port));
   exception
      when Ex : GNAT.Sockets.Socket_Error =>
         -- TODO: Probably should also give Data and Address "null" values in case of failure.
         Octet_Count := 0;
         raise Network_Error with Exception_Message(Ex);
   end Receive;


   procedure Send
     (Address     : in Addresses.UDPv4;
      Data        : in Network.Octet_Array;
      Octet_Count : in Natural) is

      IP_String          : String(1 .. 15);
      IP_String_Size     : Natural;
      GNAT_Style_Address : GNAT.Sockets.Sock_Addr_Type;
      Elements           : Ada.Streams.Stream_Element_Array(0 .. Data'Length - 1);
      Last               : Ada.Streams.Stream_Element_Offset;
   begin
      -- Convert the incoming IP address to GNAT style.
      Addresses.To_IPv4_String(Addresses.Get_IPv4(Address), IP_String, IP_String_Size);
      GNAT_Style_Address.Addr := GNAT.Sockets.Inet_Addr(IP_String(1 .. IP_String_Size));
      GNAT_Style_Address.Port := GNAT.Sockets.Port_Type(Addresses.Get_Port(Address));

      -- Covert the incoming Network.Octet_Array to an Ada.Streams.Stream_Element_Array.
      for I in Data'Range loop
         Elements( Elements'First  + Ada.Streams.Stream_Element_Offset(I - Data'First) ) :=
           Ada.Streams.Stream_Element(Data(I));
      end loop;

      -- Send the datagram.
      -- If the entire datagram didn't send, that's the best we can do so we'll ignore Last.
      GNAT.Sockets.Send_Socket(Socket, Elements, Last, GNAT_Style_Address);
   exception
      when Ex : others =>
         raise Network_Error with Exception_Message(Ex);
   end Send;


   procedure Close is
   begin
      GNAT.Sockets.Close_Socket(Socket);
   end Close;

end Network.Socket;
