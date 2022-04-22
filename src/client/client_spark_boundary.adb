---------------------------------------------------------------------------
-- FILE    : client_spark_boundary.adb
-- SUBJECT : Body of a package enclosing the SPARK portion of the client.
-- AUTHOR  : (C) Copyright 2015 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);
with Cryptographic_Services, Ada.Sequential_IO, Hermes, Client_Logger;
use Cryptographic_Services, Hermes, Client_Logger;

package body Client_SPARK_Boundary is

    function Read_This_File (Document_File_Name : in String) return SHA256_Hash_Type is
      readOctet : Hermes.Octet; -- Temporary octet value, used for storing octets into the array
      readOctetArray : Hermes.Octet_Array (1 .. 1);

      package octetIO is new Ada.Sequential_IO( Hermes.Octet );
      use octetIO;

      anyFile : octetIO.File_Type;

      fileHash : SHA256_CTX;

      fileHashFinal : SHA256_Hash_Type;
   begin
      Initialize_Hash(fileHash);
      Open( anyFile, In_File, Document_File_Name );
      while not End_Of_File (anyFile) loop -- Loops through each line of the file, and updates the hash accordingly
         Read (anyFile, readOctet);
         readOctetArray(1) := readOctet;
         Update_Hash(fileHash, readOctetArray);
      end loop;
      Finalize_Hash(FileHash, fileHashFinal); -- Finalizes and returns the hash
      Close(anyFile);
      return fileHashFinal;
   end Read_This_File;

   function Octets_To_String (Octets : in Hermes.Octet_Array) return String is
      Returned_String : String(1 .. 3  * Octets'Length - 1) := (others => ' ');
      Lookup_Array : constant array(Octet range 0 .. 15) of Character := ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
      Octet_Value : Hermes.Octet;
      Temp_Value : Octet;
      J : Positive := 1;
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
      Used_Hash : constant SHA256_Hash_Type := Read_This_File(Document_File_Name);
      Printed_Hash : constant String := Octets_To_String(Used_Hash); -- SHA256 hash in readable form
   begin
      Write_Information("File Name: " & Document_File_Name & ". SHA256 hash: " & Printed_Hash);


      -- Read the document and compute its hash.
      -- Call Client_Timestamp_Maker.Create_Timestamp
      -- Save the resulting timestamp (if the above was successful).
      raise Program_Error with "Client_SPARK_Boundary.Fetch_Timestamp not implemented";
   end Fetch_Timestamp;

   function Check_Timestamp
     (Document_File_Name : String; Timestamp_File_Name : String) return Boolean is
   begin
      raise Program_Error with "Client_SPARK_Boundary.Check_Timestamp not implemented";
      return False;
   end Check_Timestamp;


end Client_SPARK_Boundary;
