Thumper
=======

Thumper is a secure time stamp client/server system that implements RFC-3161. It allows clients
to obtain cryptographic time stamps that can be used to later verify that certain documents
existed on or before the time mentioned in the time stamp. Thumper is written in a combination
of Ada 2012 and SPARK 2014 and makes use of an external C library. Thumper is used as a SPARK
technology demonstration as well as a vehicle for exploring high integrity programming
techniques.

The system is intended to satisfy two major goals.

1. To serve as an educational vehicle for secure, high integrity programming, along with related
   topics.

2. To allow students to obtain strong time stamps of their work that can be used to prove it was
   completed on time.

Note that Thumper requires access to a supporting project providing ASN.1 support. That project
is included in this repository in the Hermes folder. Hermes is intended to be largely
independent of Thumper and could potentially be used in other projects. At some future time it
may be split into a separate repository.

See the documentation in the doc folder for more information.

Peter C. Chapin  
PChapin@vtc.vsc.edu
