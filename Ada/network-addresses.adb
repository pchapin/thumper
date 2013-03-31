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

   subtype Digit_Type is Character range '0' .. '9';
   subtype Value_Type is Network.Octet range 0 .. 9;
   type    Digit_To_Value_Type is array(Digit_Type) of Value_Type;
   type    Value_To_Digit_Type is array(Value_Type) of Digit_Type;

   Value_Lookup_Table : constant Digit_To_Value_Type :=
     Digit_To_Value_Type'(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

   Digit_Lookup_Table : constant Value_To_Digit_Type :=
     Value_To_Digit_Type'('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');

   ---------------------
   -- Public Subprograms
   ---------------------

   procedure To_IPv4_Address(Text : in String; Result : out IPv4; Status : out Status_Type) is
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
            if Value > (255 - Value_Lookup_Table(Text(I))) / 10 then
               Error_Found := True;
            else
               Value := 10 * Value + Value_Lookup_Table(Text(I));
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

      subtype Skip_Type is Positive range 1 .. 4;

      Index   : Positive;       -- Index of where current octet starts in the output string.
      Skip    : Skip_Type;      -- Number of characters to skip forward after handling the current octet.
      Space   : Natural;        -- Number of characters left in the output string.
      Value   : Network.Octet;  -- An address octet.
      Digit_2 : Digit_Type;     -- Most significant digit of an address octet (in decimal).
      Digit_1 : Digit_Type;     -- ... etc.
      Digit_0 : Digit_Type;     -- ... etc.
   begin
      Text            := (others => ' ');  -- Make output all spaces.
      Character_Count := 0;                -- No characters written to output so far.
      Status          := Success;          -- Set status to Insufficient_Space when (if) we discover that problem.

      -- For each octet...
      for I in IPv4_Address_Index_Type loop

         -- Compute starting position and remaining space in output string.
         Index := Text'First + Character_Count;
         Space := Text'Length - Character_Count;

         -- Compute the digit characters for this octet.
         Value := Address(I);
         Digit_2 := Digit_Lookup_Table(Value / 100);
         Value   := Value rem 100;
         Digit_1 := Digit_Lookup_Table(Value /  10);
         Value   := Value rem 10;
         Digit_0 := Digit_Lookup_Table(Value);

         -- Verify that enough space remains to output the digits of this octet.
         if (Digit_2 /= '0' and Space < 3) or else
            (Digit_1 /= '0' and Space < 2) or else
            (                   Space < 1) then

            Status := Insufficient_Space;
            exit;
         else
            -- Output the digits appropriately.
            if Digit_2 /= '0' then
               Text(Index + 0) := Digit_2;
               Text(Index + 1) := Digit_1;
               Text(Index + 2) := Digit_0;
               Skip := 3;
            elsif Digit_1 /= '0' then
               Text(Index + 0) := Digit_1;
               Text(Index + 1) := Digit_0;
               Skip := 2;
            else
               -- If all three digits are zero this case will correctly place a single '0' into Text.
               Text(Index + 0) := Digit_0;
               Skip := 1;
            end if;
         end if;

         -- Place the dot unless this is the last octet and then only if there is still sufficient space.
         if I /= IPv4_Address_Index_Type'Last and Text'Length - (Character_Count + Skip) < 1 then
            Status := Insufficient_Space;
            exit;
         elsif I /= IPv4_Address_Index_Type'Last then
            Text(Index + Skip) := '.';
            Skip := Skip + 1;
         end if;

         -- Update Character_Count to reflect the number of characters we just output.
         Character_Count := Character_Count + Skip;
      end loop;
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
