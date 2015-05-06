with Gdk.Event;
with Gtk.Widget;
with Gtk.Handlers;

use Gdk.Event;
use Gtk.Widget;

package Client_GUI is

   package Handlers is new Gtk.Handlers.Callback(Widget_Type => Gtk_Widget_Record);

   package Return_Handlers is new Gtk.Handlers.Return_Callback
     (Widget_Type => Gtk_Widget_Record,
      Return_Type => Boolean);

   procedure Fetch_Timestamp_Callback(Widget : access Gtk_Widget_Record'Class);

   procedure Check_Timestamp_Callback(Widget : access Gtk_Widget_Record'Class);

   procedure Destroy(Widget : access Gtk_Widget_Record'Class);

end Client_GUI;
