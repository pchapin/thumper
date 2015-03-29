
with Interfaces.C;  use Interfaces.C;

package body PostgreSQL is

   type Connection_Status_Type is
     (CONNECTION_OK, CONNECTION_BAD,
      -- Non-blocking mode only below here.
      -- The existence of these should never be relied upon - they should only
      -- be used for user feedback or similar purposes.
      CONNECTION_STARTED,           -- Waiting for connection to be made.
      CONNECTION_MADE,              -- Connection OK; waiting to send.
      CONNECTION_AWAITING_RESPONSE, -- Waiting for a response from the postmaster.
      CONNECTION_AUTH_OK,           -- Received authentication; waiting for backend startup.
      CONNECTION_SETENV,            -- Negotiating environment.
      CONNECTION_SSL_STARTUP,       -- Negotiating SSL.
      CONNECTION_NEEDED)            -- Internal state: connect() needed.
     with Convention => C;

   -- PGconn's true definition is hidden inside libpq. We only need to manipulate pointers
   -- to PGconn objects. However, Ada requires a full declaration for this type so I'm just
   -- using a dummy declaration to satisfy that requirement. TODO: Clean this up!
   --
   type PGconn is new Natural;
   type PGconn_Ptr is access PGconn;

   Connection_Handle : PGconn_Ptr;

   function PQconnectdb(Connection_Info : char_array) return PGConn_Ptr
     with
       Import,
       Convention => C,
       External_Name => "PQconnectdb";

   function PQstatus(Handle : PGconn_Ptr) return Connection_Status_Type
     with
       Import,
       Convention => C,
       External_Name => "PQstatus";

   procedure PQfinish(Handle : PGconn_Ptr)
     with
       Import,
       Convention => C,
       External_Name => "PQfinish";


   procedure Connect
     (Server : String; Port : Port_Type; Database : String; User : String; Password : String) is

      Connection_String : constant String :=
        "host="      & Server &
        " port="     & Port_Type'Image(Port) &
        " dbname="   & Database &
        " user="     & User &
        " password=" & Password &
        " connect_timeout=15";

   begin
      Connection_Handle := PQconnectdb(To_C(Connection_String));
      if PQstatus(Connection_Handle) /= CONNECTION_OK then
         PQfinish(Connection_Handle);
         raise PostgreSQL_Error with "Failed to connect to postmaster";
      end if;
   end Connect;


   procedure Disconnect is
   begin
      PQfinish(Connection_Handle);
   end Disconnect;

end PostgreSQL;
