---------------------------------------------------------------------------
-- FILE    : remote_access.ads
-- SUBJECT : Package providing AWS support for Thumper.
-- AUTHOR  : (C) Copyright 2022 by Peter Chapin
--
-- This package contains the necessary callbacks for Thumper's embedded web server. The server
-- runs entirely in it's own task so once it starts no further management from the main program
-- is necessary. The web server makes use of the facilities of package Data_Storage requiring
-- that package to be task-safe.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <pchapin@vtc.edu>
---------------------------------------------------------------------------

package Remote_Access is

   -- Does what is necessary to get the remote access ready for use.
   procedure Initialize;

   -- Does what is necessary to put the remote access to bed.
   procedure Shutdown;

end Remote_Access;
