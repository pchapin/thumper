
-- TODO: Remove dependency on Ada.Text_IO.
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Gtk.Main;
with GtkAda.File_Selection;
with Client_SPARK_Boundary;

use Ada.Text_IO;
use Ada.Strings.Unbounded;
use GtkAda.File_Selection;
use Client_SPARK_Boundary;

package body Client_GUI is

   --  This is a callback function.

   procedure Fetch_Timestamp_Callback(Widget : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced(Widget);

      Document_File_Name: Unbounded_String;
      TimeStamp_File: Unbounded_String;
   begin
      Document_File_Name := To_Unbounded_String
        (File_Selection_Dialog(Title => "Select File", Must_Exist => True));
      TimeStamp_File := Document_File_Name & ".tsp";
      Put_Line("Fetch File : " & To_String(Document_File_Name));
      --There was some issues calling it.
      --Fetch_Timestamp(To_String(Document_File_Name), To_String(TimeStamp_File));
   end Fetch_Timestamp_Callback;


   procedure Check_Timestamp_Callback(Widget : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Widget);

      TimeStamp_File: Unbounded_String;
   begin
      TimeStamp_File := To_Unbounded_String
        (File_Selection_Dialog(Title => "Select TimeStamp File", Must_Exist  => True));

      --There was some issues calling it.
      --Check_Timestamp( To_String (TimeStamp_File) );
      Put_Line("Check TimeStamp :" & To_String(TimeStamp_File));
   end Check_Timestamp_Callback;


   --   Another callback
   procedure Destroy(Widget : access Gtk_Widget_Record'Class) is
      pragma Unreferenced(Widget);
   begin
      Gtk.Main.Main_Quit;
   end Destroy;

end Client_GUI;

