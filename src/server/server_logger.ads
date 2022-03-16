---------------------------------------------------------------------------
-- FILE    : server_logger.ads
-- SUBJECT : Specification of a log management package.
-- AUTHOR  : (C) Copyright 2022 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package Server_Logger
  with
     Abstract_State => (Log_Stream with External => (Async_Readers, Effective_Writes)),
     Initializes => Log_Stream
is

   procedure Write_Error(Message : in String)
     with Global => (Output => Log_Stream);
   procedure Write_Information(Message : in String)
     with Global => (Output => Log_Stream);
   procedure Write_Warning(Message : in String)
     with Global => (Output => Log_Stream);

end Server_Logger;
