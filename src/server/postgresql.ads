
-- Package that manages a single connection to a PostgreSQL server.
package PostgreSQL is

   type Port_Type is range 0 .. 65535;

   PostgreSQL_Error : exception;

   procedure Connect
     (Server : String; Port : Port_Type; Database : String; User : String; Password : String);

   procedure Disconnect;

end PostgreSQL;
