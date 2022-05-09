Thumper
=======

Thumper is a secure time stamp client/server system that implements
[RFC-3161](https://datatracker.ietf.org/doc/html/rfc3161). It allows clients to obtain
cryptographic time stamps that can be used to later verify that certain documents existed on or
before the time mentioned in the time stamp. Thumper is written in a combination of Ada 2012 and
SPARK 2014 and makes use of external C libraries for database access and cryptographic
operations. Thumper is used as a SPARK technology demonstration.

The system is intended to satisfy two major goals.

1. To serve as an educational vehicle for secure, high integrity programming, along with related
   topics.

2. To allow students to obtain strong time stamps of their work that can be used to prove it was
   completed on time.

Thumper requires manipulating ASN.1 data and includes a library called Hermes written in SPARK
2014 for doing this. The library is not organized as a separate project but is simply a
collection of files in the `hermes` folder beneath `src`. Tests and documentation for Hermes are
also provided and integrated into the other Thumper tests and documentation. However, Hermes
does enjoy some level of independence, at least conceptually, and could potentially be separated
into its own project in the future.

Quick Start
-----------

Thumper was developed and tested primarily on Windows. However, the project file, `thumper.gpr`,
is intended to be cross-platform. The Thumper client uses GtkAda, which is not available on
macOS, so users of that platform will not be able to build the client. See [Additional Notes](#AdditionalNotes) below for more information.

To build Thumper you'll need the following software installed:

+ GNAT Community 2021 (or its equivalent). This can be downloaded from
  [AdaCore](http://www.adacore.com/community). GNAT Community contains a suitable version of
  SPARK and some other supporting libraries (such as AUnit).

+ GtkAda. This can be downloaded from [AdaCore](http://libre.adacore.com/community). You may
  need to select the "additional packages" option to find the installer on the web site. GtkAda
  is used for the client side graphical user interface.
  
+ AWS. The Ada Web Server can be downloaded from the [GitHub
  page](https://github.com/AdaCore/aws). We recommend downloading an official release rather
  than using the latest revision in the Git repository. Note, however, that (at the time of this
  writing) the release archive does not include the templates\_parser subproject. Thus you must
  go to the GitHub repository for that project and clone it into the proper folder before you
  try to build AWS. There is a link on the code page for AWS that points at the appropriate
  version in the templates\_parser history. On Windows, be sure the GNAT tools are in the path
  and then do `win32\build.cmd C:\GNAT\2021` to compile and install AWS to your GNAT
  installation folder (replace the target path as appropriate). For Linux and macOS do `make
  setup build install`.

+ PostgreSQL 14. The exact version is not necessarily critical but if you use a different
  version, or if you install it in a non-standard location, you'll need to modify the
  `thumper.gpr` project file to specify an appropriate path to PostgreSQL's `lib` folder. You
  should also be sure the PostgreSQL `bin` folder is in your PATH or otherwise make an
  arrangement so the needed shared libraries can be found during execution.
  
Be sure you match the bit size of the compiler target (e.g., 64 bits) with the bit size of
GtkAda and PostgreSQL.

You will also need to set up a database in the PostgreSQL server for use by Thumper. In what
follows, the $ character is the console prompt and not part of the commands. Proceed as follows:

    $ psql -U postgres
    postgres=> CREATE ROLE thumper WITH CREATEDB LOGIN PASSWORD 'rabbitsfoot';
    postgres=> \q
    $ createdb -U thumper ThumperDB

The above commands log into PostgreSQL as the server superuser, creates a thumper user on the
server with the ability to create databases, and then uses `createdb` to create the (empty)
database needed by the Thumper server.

After setting up the software above, you should be able to load the `thumper.gpr` project file
into GNATstudio or a similar IDE (Visual Studio Code, etc.) and build the Thumper client,
server, and test programs (for Thumper and Hermes). Note that the Hermes library will be
automatically built when Thumper is built.

See the documentation in the `doc` folder for more information.

<a name="AdditionalNotes"></a>
Additional Notes
----------------

The macOS platform does not support GtkAda and developers on other platforms may choose not to
install it (GtkAda is only needed for the client). In that case, create a file `gtkada.gpr`
in the root of the project containing the following:

    project GtkAda is
    end GtkAda;
    
This dummy file will be loaded by `thumper.gpr` thus allowing development to proceed without
editing the main project file (which should be avoided if possible).

On Linux systems, you will of course need to have the PostgreSQL server package installed. You
may also need to install a PostgreSQL development package in order to have the necessary
libraries on your system. On Ubuntu 20.04 do:

    sudo apt install libpq-dev
    
PostgreSQL on Linux systems may be configured by default in a way that makes it awkward to log
into the server as the thumper user from the system running the server. Consider modifying the
file `/etc/postgresql/[VERSION]/main/pg_hba.conf` to contain the following lines (order is
significant):

    # "local" is for Unix domain socket connections only
    local   ThumperServer   thumper        md5
    local   all             all            peer

This allows the user thumper to attach to the ThumperServer database via md5-style passwords,
even when connecting from the local system. Peer authentication won't work because there is
(probably) no thumper user on the overall Linux system. The order of the lines matter because if
they were in the opposite order, the "all all" will match every database and every user before
the specific information is reached. After making this change you will need to restart the
server:

    # systemctl restart postgresql

The default port used for communication between the client and server is 318. On Unix-like
systems (macOS and Linux), the server will not be able to bind to that port when run under an
ordinary user account ("permission denied"). Use the -p option when running the server to
specify an available port address greater than 1024:

    $ ./thumper_server -p 9000
    
Of course, a similar option will need to be provided to the client in order for the two programs
to connect successfully.

Contributors
------------

Several of my students have assisted in the development of Thumper. Specifically:

+ Luke Alden: Unit testing.
+ Nathan Brown: Interfacing to OpenSSL.
+ Andrew Clark: Setting up digital signatures using OpenSSL.
+ Tim Cross: Primitive encoding/decoding subprograms in Hermes.
+ Michael Green: Implementation of SHA-256 in Ada/SPARK.
+ Nicole Hurley: Interfacing to OpenSSL.
+ Nancy Mai: Client side GUI.
+ Elizabeth Nittler: Database access.
+ Carter Ransom: Primitive encoding/decoding subprograms in Hermes.
+ Ian Schulze: Providing AWS support for remote access to the server.
+ Falit Sehgal: Providing AWS support for remote access to the server.
+ Stirling Sidaway: Client file hashing.

Peter C. Chapin  
pchapin@vtc.edu
