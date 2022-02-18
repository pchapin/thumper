Thumper
=======

Thumper is a secure time stamp client/server system that implements RFC-3161. It allows clients
to obtain cryptographic time stamps that can be used to later verify that certain documents
existed on or before the time mentioned in the time stamp. Thumper is written in a combination
of Ada 2012 and SPARK 2014 and makes use of external C libraries for database access and
cryptographic operations. Thumper is used as a SPARK technology demonstration.

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
macOS, so users of that platform will not be able to build the client. They will also need to
provide a dummy `gtkada.gpr` file or else the main project file will not load.

To build Thumper you'll need the following software installed:

+ GNAT Community 2021 (or its equivalent). This can be downloaded from
  [AdaCore](http://www.adacore.com/community). GNAT Community contains a suitable version of
  SPARK and some other supporting libraries (such as AUnit).

+ GtkAda. This can be downloaded from [AdaCore](http://libre.adacore.com/community). You may
  need to select the "additional packages" option to find the installer on the web site. GtkAda
  is used for the client side graphical user interface.

+ PostgreSQL 14. The exact version is not necessarily critical but if you use a different
  version, or if you install it in a non-standard location, you'll need to modify the
  `thumper.gpr` project file to specify an appropriate path to PostgreSQL's `lib` folder. You
  should also be sure the PostgreSQL `bin` folder is in your PATH or otherwise make an
  arrangement so the needed shared libraries can be found during execution.
  
Be sure you match the bit size of the compiler target (e.g., 64 bits) with the bit size of
GtkAda and PostgreSQL.

After setting up the software above, you should be able to load the `thumper.gpr` project file
into GNATstudio or a similar IDE (Visual Studio Code, etc.) and build the Thumper client,
server, and test programs (for Thumper and Hermes). Note that the Hermes library will be
automatically built when Thumper is built.

See the documentation in the `doc` folder for more information.

Contributors
------------

Several of my students have assisted in the development of Thumper. Specifically:

+ Nathan Brown: Interfacing to OpenSSL.
+ Tim Cross: Primitive encoding/decoding subprograms in Hermes.
+ Nicole Hurley: Interfacing to OpenSSL.
+ Nancy Mai: Client side GUI.
+ Ian Schulze: Providing AWS support for remote access to Thumper statistics.

Peter C. Chapin  
pchapin@vtc.edu
