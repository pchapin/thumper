---------------------------------------------------------------------------
-- FILE    : network-addresses.adb
-- SUBJECT : Body of a network address handling package.
-- AUTHOR  : (C) Copyright 2013 by Peter Chapin and John McCormick
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package body Network.Addresses is

   ---------------------
   -- Public Subprograms
   ---------------------

   procedure To_IPv4_Address(Text : in String; Result : out IPv4; Status : out Status_Type) is
      -- Some suggested declarations. Using these are not required.
      subtype Digit_Type is Character range '0' .. '9';
      subtype Digit_Value_Type is Network.Octet range 0 .. 9;
      type    Digit_Array_Type is array(Digit_Type) of Digit_Value_Type;

      Digit_Lookup_Table : constant Digit_Array_Type := Digit_Array_Type'(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

      Result_Index : IPv4_Address_Index_Type;
      Value        : Network.Octet;
      Seen_Value   : Boolean;
      Error_Found  : Boolean;
   begin
      Result := IPv4'(others => 0);

      Result_Index := Result'First;
      Value        := 0;
      Seen_Value   := False;
      Error_Found  := False;

      for I in Positive range Text'First .. Text'Last loop
         --# assert Result_Index >= Result'First and Result_Index <= Result'Last;
         if Text(I) = '.' and Seen_Value then
            -- Handle dot.
            Result(Result_Index) := Value;
            if Result_Index = Result'Last then
               Error_Found := True;
            else
               Result_Index := Result_Index + 1;
               Value := 0;
               Seen_Value := False;
            end if;
         elsif Text(I) in Digit_Type then
            -- Handle digit.
            if Value > (255 - Digit_Lookup_Table(Text(I))) / 10 then
               Error_Found := True;
            else
               Value := 10 * Value + Digit_Lookup_Table(Text(I));
               Seen_Value := True;
            end if;
         else
            -- Handle unknown or unexpected character.
            Error_Found := True;
         end if;
         exit when Error_Found;
      end loop;

      -- Did the loop above find bad things?
      if Error_Found or Result_Index < Result'Last or not Seen_Value then
         Status := Invalid_Address;
      else
         Result(Result_Index) := Value;
         Status := Success;
      end if;
   end To_IPv4_Address;


   procedure To_IPv4_String
     (Address         : in  IPv4;
      Text            : out String;
      Character_Count : out Natural;
      Status          : out Status_Type) is
   begin
      -- TODO: Finish me!
      null;
   end To_IPv4_String;


   function To_UDPv4_Address(Address : IPv4; Port : Port_Type) return UDPv4 is
      Result : UDPv4;
   begin
      Result.Address := Address;
      Result.Port := Port;
      return Result;
   end To_UDPv4_Address;


   function Get_Port(Endpoint_Address : UDPv4) return Port_Type is
   begin
      return Endpoint_Address.Port;
   end Get_Port;


   function Get_IPv4(Endpoint_Address : UDPv4) return IPv4 is
   begin
      return Endpoint_Address.Address;
   end Get_IPv4;

end Network.Addresses;
