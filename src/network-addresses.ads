---------------------------------------------------------------------------
-- FILE    : network-addresses.ads
-- SUBJECT : Specification of a network address handling package.
-- AUTHOR  : (C) Copyright 2013 by Peter Chapin and John McCormick
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

--# inherit Network;
package Network.Addresses is

   type Port_Type is range 0 .. 2**16 - 1;

   type IPv4  is private;
   type UDPv4 is private;

   type Status_Type is (Success, Invalid_Address);

   -- Convert "x.y.z.w" addresses into a suitable binary representation. Returns an 'Invalid_Address' status if the address
   -- is not in an acceptable form. All values x, y, z, and w must be between 0 and 255 in decimal. No leading, trailing,
   -- or embedded non-digits allowed.
   --
   procedure To_IPv4_Address(Text : in String; Result : out IPv4; Status : out Status_Type);
   --# derives Result from Text &
   --#         Status from Text;

   function To_UDPv4_Address(Address : IPv4; Port : Port_Type) return UDPv4;

private

   subtype IPv4_Address_Index_Type is Integer range 1 .. 4;
   type IPv4 is array(IPv4_Address_Index_Type) of Network.Octet;

   type UDPv4 is
      record
         Address : IPv4;
         Port    : Port_Type;
      end record;

end Network.Addresses;
