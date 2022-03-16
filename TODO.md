
Thumper TODO List
=================

There are several general areas in the Thumper project where work is needed. Some of these areas
are relatively simple, others are significantly complex. The order of the areas below is in
approximate priority order. The focus is first on the server (implementation followed by
testing) and then the client. Interaction with a database or with web clients would be fun and
interesting but not essential to the functionality of the system.

+ [Server Decoding of Request and Encoding of Reply](#ServerDecoding)
+ [Support for Digital Signatures via OpenSSL](#DigitalSignatures)
+ [Testing/Verification](#Testing)
+ [Server Testing Against a Known Client](#KnownClient)
+ [Client Hash Computation](#HashComputation)
+ [Client Encoding of Request and Decoding of Reply](#ClientEncoding)
+ [Client Timestamp Verification](#TimestampVerification)
+ [Client Testing Against a Known Server](#KnownServer)
+ [Client GUI Improvements](#GUIImprovements)
+ [General Configuration System](#Configuration)
+ [Documentation Tasks](#Documentation)
+ [Server Database Connectivity](#Database)
+ [Server Web Access](#WebAccess)
+ [SPARK Implementation of SHA256](#SHA256)
+ [SPARK Implementation of Asymmetric Key Algorithms](#AsymmetricKey)

What follows are some more details about each of these areas. For each area I outline briefly
what that area entails (as I see it) along with some issues that need to be solved.

<a name="ServerDecoding"></a>
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

<a name="#DigitalSignatures"></a>
Support for Digital Signatures via OpenSSL
------------------------------------------

Currently, the subprograms for public key cryptography are unimplemented. We will need to fix
that by calling appropriate subprograms in the OpenSSL library. This entails calling C from Ada
and dealing with OpenSSL.

First, the OpenSSL command line tool (`openssl`) can be used to generate a DSA (digital
signature algorithm) key and control its format. For example, the tool can be used to output the
key in DER encoding (ASN.1) if necessary. The idea here is that the public/private keypair used
by Thumper would be generated and managed externally to Thumper by OpenSSL. We do *not* intend
to implement the key generation ourselves. Ultimately the key will be stored in a file that the
Thumper server could read.

Processing the key to make signatures can be done by suitable OpenSSL C functions. The
documentation on version 1.1.1 of OpenSSL [contains a list of all functions in the OpenSSL
libraries](https://www.openssl.org/docs/man1.1.1/man3/). In particular, the documentation on
function [DSA\_do\_sign](https://www.openssl.org/docs/man1.1.1/man3/DSA_do_sign.html) is of
interest.

One approach to handling this area would be to first try using the OpenSSL C library directly
(in C) to compute a digital signature of some data. Once the details of how to do that are
understood, one could then worry about how to call the C function(s) from Ada.

Interfacing C from Ada is actually not difficult. The Ada standard contains [a package that
facilitates interfacing to
C](https://www.adaic.org/resources/add_content/standards/12rm/html/RM-B-3.html). It might be
necessary to create some C helper functions to implement certain aspects of the connection
between Ada and C (largely due to the limitations of SPARK). However, this is not a problem as
GNATstudio is perfectly happy building programs from a mixture of C and Ada.

One potential complication is that a true X.509 certificate will be needed by Thumper (it is a
requirement of RFC-3161). These certificates have a more complex formatting than raw keys, but
the OpenSSL tool can manage them.

<a name="#Testing"></a>
Testing/Verification
--------------------

Some software teams designate one team member to the job of testing. In such teams that person
because a "testing specialist" that writes tests for all aspects of the project. That is the
approach I'm visualizing here.

The current code base has a few tests already created but they are minimal. Furthermore as more
functionality is written, it will need to be tested. All tests should be fully automated so they
can be easily executed in batch. In many environments a special server (called a "continuous
integration" server) is configured to build and test the entire system after every commit... or
at least every day. Although I'm not suggesting we go that far (unless you want to explore it!)
we should at least have tests that could potentially be used in that kind of environment.

Some tests are complicated to do automatically such as tests involving multiple, interacting
programs (client/server!). This area is all about building tests, making sure the tests pass,
and alerting responsible people to tests that are failing.

Because certain parts of Thumper and Hermes are in SPARK, this area should also concern itself
with running the SPARK verification tools over all parts of the system that are supposed to
verify successfully to ensure that they actually do.

Note that some testing might be done via scripts using a currently unspecified scripting
language.

<a name="#KnownClient"></a>
Server Testing Against a Known Client
-------------------------------------

There is an implementation of RFC-3161 client in
[Java](https://github.com/eclipsoft/java-tsp-client) and in
[Python](https://pypi.org/project/rfc3161/) that we could use for testing. The idea would be to
use a known and presummably working client with our server to verify that our server is properly
decoding requests, constructing timestamps, and encoding those timestamps. This is a kind of
integration test that could be done in addition to the testing and verification mentioned
earlier.

Although at the moment the Thumper server (and client) do not work, there are still things to be
done in this area. First, the existing implementations could be tested against themselves to
verify that we understand how to use them. Next scripts or other tools could be set up to
facilitate testing the Thumper server against the known client. While the test will currently
fail, the infrastructure to run the test could still be made.

Note that currently the Thumper documentation set (in Build.xml) discusses the option of using
the Python RFC-3161 implementation for testing. That documentation should be updated/moved as
part of work on this area.

<a name="#HashComputation"></a>
Client Hash Computation
-----------------------

The client needs to compute a SHA256 hash of the "document" (really any file) selected by the
client user. Right now this is a comment "Read the document and compute its hash" in the file
`client_spark_boundary.adb` in the client code base. The comment needs to be replaced with code
that opens and reads the specified file and passes the file's contents through the hashing
process. Ultimately the hash value will be passed into the `Create_Timestamp` procedure which,
on the client side, entails forming a request packet, sending it to the server, receiving the
reply, and decoding that reply.

As an aside, the server code does all of the network interaction in `server_spark_boundary.adb`
and leaves its `Create_Timestamp` only to do the internal, non-network, manipulations of the
timestamp itself. In contrast on the client side, the network manipulations have been pushed
into `Create_Timestamp`. Is this asymmetry unsightly? The client doesn't really create
timestamps at all. From the client's point of view, creating a timestamp is about getting the
server to do it, and thus entails communication with the server. We could improve the symmetry
between the client and server by moving the network handling on the client into the "SPARK
boundary" package and reserving `Create_Timestamp` for just non-network manipulations. However,
on the client side that would only entail the preparation of the request and, later, the
decoding of the reply, so perhaps the name `Create_Timestamp` should be changed as well (and the
procedure be split into two procedures).

<a name="#ClientEncoding"></a>
Client Encoding of Request and Decoding of Reply
------------------------------------------------

This is the dual of the server decoding the request and encoding the reply. In fact, once the
server side is written the client side may be much easier. However, there are four distinct
operations requred overall request encode/decode and reply encode/decode. While conceptually
similar, I question how much code they can share.

<a name="#TimestampVerification"></a>
Client Timestamp Verification
-----------------------------

In the package `Client_SPARK_Boundary` there is a function `Check_Timestamp` that is intended to
verify that the timestamp it is given is correct. That function has to compare the hash in the
timestamp with the hash of the document, verify the "sensibility" of the time information in the
timestamp, and then check the digital signature of the server on the timestamp. The function is
used in response to a user request to verify a timestamp but also during the timestamp fetching
process to verify that the timestamp just sent by the server is reasonable.

<a name="#KnownServer"></a>
Client Testing Against a Known Server
-------------------------------------

This is another integration test where the Thumper client is used against an existing, assumed
to work, server to verify interoperability and overall functionality. See the area about testing
the server against a known client for more information.

<a name="#GUIImprovements"></a>
Client GUI Improvements
-----------------------

The graphical user interface on the client could use enhancement. One good starting point would
be updating the `Client_Logger` package to display messages using a suitable message box. Other
improvements to the client's UI could also be made such as support for selecting an entire
folder of files and getting time stamps on all the files in that folder, perhaps with a progress
indication. This would entail learning about GtkAda. It might also ential learning about how to
manage Ada tasks since, for efficiency reasons, fetching multiple timestamps could potentially
be done in multiple tasks.

<a name="#Configuration"></a>
General Configuration System
----------------------------

Certain runtime parameters, such as information about the database server, should be taken from
a configuration file of some kind. The server definitely needs one. The client might need one.
Support for reading a configuration file could be added to the common code. No support for
writing such a file is required; it could be written using an ordinary text editor.

In general it is normal for command line options to override configuration settings in a
configuration file, and for configuration file settings to override default settings. A uniform
way of handling this issue, for both server and client, would be desirable. Package
`Thumper_Switches` in the top level source folder (common to both client and server) is where
the command line is currently being handled. This package could be enhanced to deal with a
configuation file as well (and possibly renamed).

Here is a sample of potential configuration file syntax (the `#` symbol starts comments):

    # This is a configuration file for the Thumper client/server
    ServerPort: 318       # Default value is 318, use -p command line option to set.
    ServerAccessPort: 80  # For web browser access to the server.
    
We would have to come up with names for the various configurable parameters, suitable default
values for them, and suitable command line options to set them.

<a name="#Documentation"></a>
Documentation Tasks
-------------------

The Thumper documentation in the `doc` folder (written in DocBook) needs much tending. Here are
some documents (or chapters of a single document) that could be provided:

+ A proper requirements document.
+ User documentation that describes the configuration options for both the client and server.
+ Design documentation the describes the internal structure of the both programs.
+ A web page that describes the project and provides access to other documentation, resources,
  and downloads.

<a name="#Database"></a>
Server Database Connectivity
----------------------------

The package `Data_Storage` in the server is intended to provide access to the database. The idea
is to store information about every timestamp created. This will entail defining a table to hold
that information (a single table should be fine), and then implementing the body of package
`Data_Storage` so that it can store and retrieve timestamp data.

Possible attributes include: the IP address of the client who made the request, the hash of the
document, and the date/time when the timestamp was made. Other attributes could be considered.

<a name="#WebAccess"></a>
Server Web Access
-----------------

The server should allow an administrator with a web browser to access the server and view
information about what the server has been doing (for example, a list of timestamps it has
generated). This can be done using AWS... the Ada Web Server. AWS is a library that can be
included in any Ada program that implements a web server inside that program. The package
`Remote_Access` already exists, but it needs to be completed (and linked into the server
executable).

AWS has a template based system to simplify creating consistent web sites. However, for this
application we might not need to use it, which might be easier. At the most basic level, the
administrator of the Thumper server should be able to fill out a form with a range of dates, and
then view a list of timestamps created between those dates (AWS would have to retrieve that
information from the database).

<a name="#SHA256"></a>
SPARK Implementation of SHA256
------------------------------

Thumper's reliance on OpenSSL is intended to be temporary. Ultimately we will want our own
implementation of the cryptographic operations written in SPARK. We could start by writing our
own implementation of the SHA256 hash algorithm. The formal specification of this algorithm is
given by [FIPS 180-4](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf).

<a name="#AsymmetricKey"></a>
SPARK Implementation of Asymmetric Key Algorithms
-------------------------------------------------

Similarly to the SHA256 implementation, we will ultimately want a SPARK implementation of the
digital signature algorithm used by Thumper. This is more complex than implementing SHA256
because, in addition to the algorithm being more complex, it will ential the processing of X.509
certificates. I do not suggest working on this area at this time.
