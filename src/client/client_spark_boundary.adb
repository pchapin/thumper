---------------------------------------------------------------------------
-- FILE    : client_spark_boundary.adb
-- SUBJECT : Body of a package enclosing the SPARK portion of the client.
-- AUTHOR  : (C) Copyright 2022 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Sequential_IO;
with Client_Logger;
with Cryptographic_Services;
with Hermes;

use Client_Logger;
use Cryptographic_Services;
use Hermes;

package body Client_SPARK_Boundary is

   package Octet_IO is new Ada.Sequential_IO(Hermes.Octet);
   use Octet_IO;

   function Read_Document_File(Document_File_Name : in String) return SHA256_Hash_Type is
      Read_Octet        : Hermes.Octet; -- Temporary octet value.
      Read_Octet_Array  : Hermes.Octet_Array(1 .. 1);
      Any_File          : Octet_IO.File_Type;
      File_Hash_Context : SHA256_CTX;
      File_Hash         : SHA256_Hash_Type;
   begin
      Initialize_Hash(File_Hash_Context);
      Open(Any_File, In_File, Document_File_Name);
      while not End_Of_File(Any_File) loop
         Read(Any_File, Read_Octet);
         Read_Octet_Array(1) := Read_Octet;
         Update_Hash(File_Hash_Context, Read_Octet_Array);
      end loop;
      Finalize_Hash(File_Hash_Context, File_Hash);
      Close(Any_File);
      return File_Hash;
   end Read_Document_File;


   -- TODO: This is a useful function. Put it in a common library somewhere?
   function Octets_To_String (Octets : in Hermes.Octet_Array) return String is
      Returned_String : String(1 .. 3  * Octets'Length - 1) := (others => ' ');
      Lookup_Array    : constant array(Octet range 0 .. 15) of Character :=
        ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
      Octet_Value : Hermes.Octet;
      Temp_Value  : Hermes.Octet;
      J           : Positive := 1;
   begin
      for I in Octets'Range loop
         Octet_Value := Octets(I);

         Temp_Value := Octet_Value / 16;
         Returned_String(J) := Lookup_Array(Temp_Value);
         J := J + 1;

         Temp_Value := Octet_Value rem 16;
         Returned_String(J) := Lookup_Array(Temp_Value);
         J := J + 2;
      end loop;
      return Returned_String;
   end Octets_To_String;


   procedure Fetch_Timestamp (Document_File_Name : in String; Timestamp_File_Name : in String) is
      pragma Unreferenced(Timestamp_File_Name); -- TODO: Finish this procedure!

      File_Hash    : constant SHA256_Hash_Type := Read_Document_File(Document_File_Name);
      Printed_Hash : constant String := Octets_To_String(File_Hash);
   begin
      Write_Information("Verify file name: " & Document_File_Name & "; SHA256 hash: " & Printed_Hash);

      -- Call Client_Timestamp_Maker.Create_Timestamp
      -- Save the resulting timestamp (if the above was successful).
      raise Program_Error with "Client_SPARK_Boundary.Fetch_Timestamp not implemented";
   end Fetch_Timestamp;


   function Check_Timestamp
     (Document_File_Name : String; Timestamp_File_Name : String) return Boolean
   is
      pragma Unreferenced(Timestamp_File_Name);

      File_Hash    : constant SHA256_Hash_Type := Read_Document_File(Document_File_Name);
      Printed_Hash : constant String := Octets_To_String(File_Hash);
   begin
      Write_Information("Check file name: " & Document_File_Name & "; SHA256 hash: " & Printed_Hash);

      raise Program_Error with "Client_SPARK_Boundary.Check_Timestamp not implemented";
      return False;
   end Check_Timestamp;

end Client_SPARK_Boundary;
