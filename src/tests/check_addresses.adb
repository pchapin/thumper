---------------------------------------------------------------------------
-- FILE    : check_addresses.adb
-- SUBJECT : Package containing tests of network address handling.
-- AUTHOR  : (C) Copyright 2013 by Peter Chapin and John McCormick
--
-- Please send comments or bug reports to
--
--      Peter Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with AUnit.Assertions; use AUnit.Assertions;
with Network.Addresses;
with Network.Addresses.Test;

package body Check_Addresses is
   use Network.Addresses;
   use Network.Addresses.Test;

   procedure Test_IPv4_Conversion(T : in out AUnit.Test_Cases.Test_Case'Class) is
      Address : IPv4;
      Status  : Status_Type;
      -- TODO: Create a table of test cases!
   begin
      -- Good addresses should be accepted.
      To_IPv4_Address("155.42.234.60", Address, Status);
      Assert(Status  = Success, "Failed to convert 155.42.234.60");
      Assert(Address = (155, 42, 234, 60), "Incorrectly converted 155.42.234.60");

      To_IPv4_Address("0.0.0.0", Address, Status);
      Assert(Status  = Success, "Failed to convert 0.0.0.0");
      Assert(Address = (0, 0, 0, 0), "Incorrectly converted 0.0.0.0");

      To_IPv4_Address("255.255.255.255", Address, Status);
      Assert(Status  = Success, "Failed to convert 255.255.255.255");
      Assert(Address = (255, 255, 255, 255), "Incorrectly converted 255.255.255.255");

      -- Bad addresses should be rejected.
      To_IPv4_Address("", Address, Status);
      Assert(Status = Invalid_Address, "Converted the empty string");
      To_IPv4_Address("256.0.0.0", Address, Status);
      Assert(Status = Invalid_Address, "Converted 256.0.0.0");
      To_IPv4_Address("1234.0.0.0", Address, Status);
      Assert(Status = Invalid_Address, "Converted 1234.0.0.0");
      To_IPv4_Address("0.0.0.256", Address, Status);
      Assert(Status = Invalid_Address, "Converted 0.0.0.256");
      To_IPv4_Address("0.0.0.1234", Address, Status);
      Assert(Status = Invalid_Address, "Converted 0.0.0.1234");
      To_IPv4_Address("0.0..0", Address, Status);
      Assert(Status = Invalid_Address, "Converted 0.0..0");
      To_IPv4_Address("0.0.0", Address, Status);
      Assert(Status = Invalid_Address, "Converted 0.0.0");
      To_IPv4_Address(".0.0.0", Address, Status);
      Assert(Status = Invalid_Address, "Converted .0.0.0");
      To_IPv4_Address("0.0.0.", Address, Status);
      Assert(Status = Invalid_Address, "Converted 0.0.0.");
      To_IPv4_Address("0.0.0.0.", Address, Status);
      Assert(Status = Invalid_Address, "Converted 0.0.0.0.");
      To_IPv4_Address("0.0.0.0.0", Address, Status);
      Assert(Status = Invalid_Address, "Converted 0.0.0.0.0");
      To_IPv4_Address(" 0.0.0.0", Address, Status);
      Assert(Status = Invalid_Address, "Converted 0.0.0.0 with leading space");
      To_IPv4_Address("0.0.0.0 ", Address, Status);
      Assert(Status = Invalid_Address, "Converted 0.0.0.0 with trailing space");
      To_IPv4_Address("0.0.0x.0", Address, Status);
      Assert(Status = Invalid_Address, "Converted 0.0.0x.0");
      To_IPv4_Address("0.0.x.0", Address, Status);
      Assert(Status = Invalid_Address, "Converted 0.0.x.0");
   end Test_IPv4_Conversion;


   procedure Register_Tests(T : in out Address_Test) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine(T, Test_IPv4_Conversion'Access, "IPv4 Conversion");
   end Register_Tests;


   function Name(T : Address_Test) return AUnit.Message_String is
   begin
      return AUnit.Format("Address");
   end Name;

end Check_Addresses;
