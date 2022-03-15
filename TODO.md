
Thumper TODO List
=================

There are several general areas in the Thumper project where work is needed. Some of these areas
are relatively simple, others are significantly complex. The order of the items below is in
approximate priority order. The focus is first on the server (implementation followed by
testing) and then the client. Interaction with a database or with web clients would be fun and
interesting but not essential to the functionality of the system.

+ Server Decoding of Request and Encoding of Reply
+ Support for Digital Signatures via OpenSSL
+ Testing/Verification
+ Server Testing Against a Known Client
+ Client Hash Computation
+ Client Encoding of Request and Decoding of Reply
+ Client Timestamp Verification
+ Client Testing Against a Known Server
+ Client GUI Improvements
+ General Configuration System
+ Server Database Connectivity
+ Server Web Access
+ SPARK Implementation of SHA256
+ SPARK Implementation of Asymmetric Key Algorithms

What follows are some more details about each of these areas. For each area I outline briefly
what that area entails (as I see it) along with some issues that need to be solved.

Server Decoding of Request and Encoding of Reply
------------------------------------------------

Before the Thumper system can do anything useful, the request/response pair must be properly
created and decoded. There are four components to this issue: encoding the request (in the
client), decoding the request (in the server), encoding the response (in the server), and
decoding the response (in the client). This will entail working with and expanding the Hermes
library. Also, this section of the project is in SPARK.

This area would entail learning about the ASN.1 formatting rules and providing the necessary
subprograms to encode/decode data according to those rules. This work has already been started
but it is incomplete.

Another aspect of this area is that the ASN.1 encoding/decoding (basically the Hermes library)
should be written in SPARK and proved free of runtime errors. Thus this area would also entail
working more closely with SPARK.

As an aside... The "proper" way to deal with ASN.1 is to use a tool called an "ASN.1 compiler"
that converts the high level notation of a protocol into code in some programming language that
implements the encoding/decoding of that protocol. Once the ASN.1 compiler is written it can be
used for any protocol that employs ASN.1, greatly simplifying the task of using ASN.1 in
applications. It is *not* the intention of this project to go this route. Instead we will hand
build the encoding/decoding subprograms.

There is a group looking at creating an ASN.1 compiler that generates Ada (none currently exists
as far as I know) but they are not planning to output provable SPARK. At some future time the
Hermes effort may wish to become associated with this other effort in some way.

Support for Digital Signatures via OpenSSL
------------------------------------------

Currently, the subprograms for public key cryptography are unimplemented. We will need to fix
that by calling appropriate subprograms in the OpenSSL library. This entails calling C from Ada.

OpenSSL is written in C and so this area is mostly about interfacing Ada to C. The method for
doing this is described in AdaCore's documentation for GNAT and in the Ada standard itself. In
principle it isn't hard to do but, as always, the devil is in the details.

The building of Thumper will become more complex due to this mixed language interaction. First
it may be necessary to write some C helper functions (GPS allows projects that are a mixture of
Ada and C so this is not a show stopper). Second it will be necessary to install OpenSSL and
work out how to draw its libraries into the Thumper build.

One extra complication is that a true X.509 certificate will be needed by the OpenSSL functions.
Fortunately the OpenSSL library comes with a program for creating and managing such
certificates. Learning how to use that program is thus also a part of this area.

Testing/Verification
--------------------

Package Bodies: The various `Check_*` packages in the `tests` folders of Thumper and Hermes.

Some software teams designate one team member to the job of testing. In such teams that person
because a "testing specialist" that writes tests for all aspects of the project. That is the
approach I'm visualizing here.

The current code base has a few tests already created but they are minimal. Furthermore as more
functionality is written, it will need to be tested. All tests should be fully automated so they
can be easily executed in batch. In many environments a special server (called a "continuous
integration" server) is configured to build and test the entire system after every commit... or
at least every day. Although I'm not suggesting we go that far we should at least have tests
that could potentially be used in that kind of environment.

Some tests are complicated to do automatically such as tests involving multiple, interacting
programs (client/server!). This area is all about building tests, making sure the tests pass,
and alerting responsible people to tests that are failing.

Because certain parts of Thumper and Hermes are in SPARK, this area should also concern itself
with running the SPARK verification tools over all parts of the system that are supposed to
verify successfully to ensure that they actually do.

Note that some testing might be done via scripts using a currently unspecified scripting
language.

Server Testing Against a Known Client
-------------------------------------

There is an implementation of RFC-3161 in Python that we could use for testing. The idea would
be to use our client against the Python server and use the Python client with our server. This
would check interoperability and help verify conformance to the standard. Note that since
neither our client nor our server are fully functional, this testing can't really be done.
However, the infrastructure to do it could still be set up.

Client Hash Computation
-----------------------

Client Encoding of Request and Decoding of Reply
------------------------------------------------

Client Timestamp Verification
-----------------------------

Client Testing Against a Known Server
-------------------------------------

Client GUI Improvements
-----------------------

Client GUI. The graphical user interface on the client could use enhancement. One good starting
point would be updating the `Client_Logger` package to display messages using a suitable message
box. Other improvements to the client's UI could also be made. This would entail learning about
GtkAda.

General Configuration System
----------------------------

Certain runtime parameters, such as information about the database server, should be taken from
a configuration file of some kind. The server definitely needs one. The client might need one.
Support for reading a configuration file could be added to the common code. No support for
writing such a file is required; it could be written using an ordinary text editor.

Server Database Connectivity
----------------------------

The package `Data_Storage` in the server is intended to provide access to the database. The idea
is to store information about every timestamp created. This will entail defining a table to hold
that information and then implementing the body of package `Data_Storage` so that it can store
and retrieve timestamp data.

Server Web Access
-----------------

The server should allow an administrator with a web browser to access the server and view
information about what the server has been doing (for example, a list of timestamps it has
generated). This can be done using AWS... the Ada Web Server. AWS is a library that can be
included in any Ada program that implements a web server inside that program. The package
`Remote_Access` already exists, but it needs to be completed (and linked into the server
executable).

SPARK Implementation of SHA256
------------------------------

Thumper's reliance on OpenSSL is intended to be temporary. Ultimately we will want our own
implementation of the cryptographic operations written in SPARK. We could start by writing our
own implementation of the SHA256 hash algorithm. This should be done in a task-safe way.

SPARK Implementation of Asymmetric Key Algorithms
-------------------------------------------------

