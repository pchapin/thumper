---------------------------------------------------------------------------
-- FILE    : remote_access.adb
-- SUBJECT : Package providing AWS support for Thumper.
-- AUTHOR  : (C) Copyright 2022 by Peter Chapin
--
-- This package contains the necessary callbacks for Thumper's embedded web server.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <pchapin@vtc.edu>
---------------------------------------------------------------------------
with AWS.Messages;
with AWS.MIME;           use AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;         use AWS.Status;
with AWS.Utils;

with Data_Storage;       use Data_Storage;
with Hermes;
with Serial_Generator;   use Serial_Generator;
with Server_Logger;
with Timestamp_Messages; use Timestamp_Messages;

package body Remote_Access is

   -- Server declaration.
   Web_Server : AWS.Server.HTTP;


   -- TODO (SECURITY): Make sure the URI contains no '..' elements!
   function To_File_Name (URI : String) return String
     with Pre => URI'Length > 0
   is
      -- TODO: Make the base URI configurable.
      Base_URI : constant String := "TestHTML";
   begin
      if URI(URI'Last) = '/' then
         return Base_URI & URI & "/index.html";
      else
         return Base_URI & URI;
      end if;
   end To_File_Name;


   function Service (Request : AWS.Status.Data) return AWS.Response.Data is
      URI       : constant String := AWS.Status.URI(Request);
      File_Name : constant String := To_File_Name(URI);

      HTML_File_Head : constant String := "<html><head>";
      HTML_Body      : constant String := "<body>";
      HTML_File_End  : constant String := "</body></html>";

      Count_File_Begin  : constant String :=
        "<title>Count of Time Stamps</title></head><body>The count is ";
      Count_File_End    : constant String := "</body></html>";
      Result_File_Begin : constant String := "<title>Timestamp:</title>";
      Stamp_File_Begin  : constant String :=
        "<html><head><title>Timestamp</title></head><body>The timestamp is ";

      Hash             : Hermes.Octet_Array (1 .. Hash_Size);
      Generalized_Time : String(1 .. 15) := (others => ' ');

      function To_Hex(Byte : Hermes.Octet) return String is
         use type Hermes.Octet;

         Result : String(1 .. 2);
         Lookup : constant array (Hermes.Octet range 0 .. 15) of Character :=
           ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
      begin
         Result(1) := Lookup(Byte /   16);
         Result(2) := Lookup(Byte rem 16);
         return Result;
      end To_Hex;

      -- TODO: A function like this also exists in the client where a file is hashed.
      function Hash_Conversion(Raw_Hash : Hermes.Octet_Array) return String is
         Workspace : String(1 .. (2 * Hash_Size));
      begin
         for I in Raw_Hash'Range loop
            Workspace
              (2 * (I - Raw_Hash'First) + 1 .. 2 * (I - Raw_Hash'First) + 2) :=
              To_Hex (Raw_Hash (I));
         end loop;
         return Workspace;
      end Hash_Conversion;

   begin --Service
      Server_Logger.Write_Information("AWS: Requested URI: " & URI);

      if URI = "/count.html" then
         return
           AWS.Response.Build
             (Text_HTML, Count_File_Begin & Count_Type'Image(Timestamp_Count) & Count_File_End);

      elsif URI = "/submit.html" then
         Server_Logger.Write_Information(Parameter(Request, "datetimes"));
         -- TODO: It would probably be better to use AWS's templating mechanism.
         return
           AWS.Response.Build
             (Text_HTML,
              HTML_File_Head & Result_File_Begin & HTML_Body &
              "The requested date and time range was : " & Parameter(Request, "datetimes") &
              HTML_File_End);

      elsif URI = "/serialtest.html" then
         declare
            Timestamps : Constant Timestamp_Array :=
              Timestamp_Retrieve(Serial_Number_Type'Value(Parameter(Request, "datetimes")));
         begin
            Hash             := Timestamps(1).Hashed_Message;
            Generalized_Time := Timestamps(1).Generalized_Time;
         end;
         return
           AWS.Response.Build
             (Text_HTML,
              HTML_File_Head & Result_File_Begin & HTML_Body & "Hash is: " &
              Hash_Conversion(Hash) & HTML_File_End);

      elsif AWS.Utils.Is_Regular_File(File_Name) then
         Server_Logger.Write_Information("AWS: Attempting to return file: " & File_Name);
         return
           AWS.Response.File
             (Content_Type => Content_Type(File_Name), Filename => File_Name);

      else
         return
           AWS.Response.Acknowledge
             (AWS.Messages.S404, "<p>Not found: " & URI & "</p>");
      end if;
   end Service;


   procedure Initialize is
   begin
      AWS.Server.Start(Web_Server, "Web_Server", Service'Access, Port => 8_000);
      Server_Logger.Write_Information ("AWS: Internal web server has started");
   end Initialize;


   procedure Shutdown is
   begin
      AWS.Server.Shutdown(Web_Server);
      Server_Logger.Write_Information("AWS: Internal web server has shutdown");
   end Shutdown;

end Remote_Access;
