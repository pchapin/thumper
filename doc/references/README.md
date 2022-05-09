
Thumper References
==================

This folder contains local copies of certain important references. It is not necessary to
populate this folder but you may find it convenient to do so. These references are not included
in the repository because they are not artifacts of this project.

There are several RFCs of interest.

+ [RFC-3161](http://www.rfc-editor.org/rfc/rfc3161.txt) Internet X.509 Public Key Infrastructure
  Time-Stamp Protocol (TSP). This is the primary RFC that describes the protocol implemented by
  Thumper.

+ [RFC-3370](http://www.rfc-editor.org/rfc/rfc3370.txt) Cryptographic Message Syntax (CMS)
  Algorithms. Information about specific algorithms has been split off from the description of
  CMS itself. This allows the IETF to update to the two documents independently. This RFC is the
  latest CMS algorithms document (but see the update in RFC-5754).

+ [RFC-5652](http://www.rfc-editor.org/rfc/rfc5652.txt) Cryptographic Message Syntax (CMS). CMS
  describes the specific way in which encrypted and digitally signed data is represented in
  various IETF standards (including TSP).

+ [RFC-5754](http://www.rfc-editor.org/rfc/rfc5754.txt) Using SHA2 Algorithms with Cryptographic
  Message Syntax. This RFC updates RFC-3370 by providing details on using the SHA2 algorithm
  with CMS.

+ [RFC-5816](http://www.rfc-editor.org/rfc/rfc5816.txt) ESSCertIDv2 Update for RFC 3161.

Formal specifications of cryptographic algorithms used by Thumper.

+ Thumper uses the [SHA256](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf) hash
  function.
  
+ Documentation for the [OpenSSL library](https://www.openssl.org/docs/).

Information about ASN.1.

+ ASN.1-Communication-Between-Hetero-Systems.pdf. A book on ASN.1 by Olivier Dubuisson. This
  book can be
  [freely downloaded](http://www.oss.com/asn1/resources/books-whitepapers-pubs/dubuisson-asn1-book.PDF)
  from the [OSS Nokalva](http://www.oss.com/) site.

+ ASN.1-Communication-Between-Hetero-Systems-Errata.pdf. Corrections to the book above.
  [Free download](http://www.oss.com/asn1/resources/books-whitepapers-pubs/ASN.1.%20Communication%20between%20heterogeneous%20systems%20-%20Errata.pdf).

+ ASN.1-Complete.pdf. A book on ASN.1 by John Larmouth. This book can be
  [freely downloaded](http://www.oss.com/asn1/resources/books-whitepapers-pubs/larmouth-asn1-book.pdf)
  from the [OSS Nokalva](http://www.oss.com/) site.

The ITU is the source of the documents below. Try these pages to obtain these documents:
[X.680](http://www.itu.int/rec/T-REC-X.680/en), [X.681](http://www.itu.int/rec/T-REC-X.681/en),
[X.682](http://www.itu.int/rec/T-REC-X.682/en), [X.683](http://www.itu.int/rec/T-REC-X.683/en),
and [X.690](http://www.itu.int/rec/T-REC-X.690/en).

+ T-REC-X.680-202102.pdf. Information technology - Abstract Syntax Notation One (ASN.1):
  Specification of basic notation.

+ T-REC-X.681-202102.pdf. Information technology - Abstract Syntax Notation One (ASN.1):
  Information object specification.

+ T-REC-X.682-202102.pdf. Information technology - Abstract Syntax Notation One (ASN.1):
  Constraint specification.

+ T-REC-X.683-202102.pdf. Information technology - Abstract Syntax Notation One (ASN.1):
  Parameterization of ASN.1 specifications.

+ T-REC-X.690-202102.pdf. Information technology - ASN.1 encoding rules: Specification of Basic
  Encoding Rules (BER), Canonical Encoding Rules (CER), and Distinguished Encoding Rules (DER).

Here are some papers related to this project.

+ [Design and Implementation of a RFC3161-Enhanced Time-Stamping Service](http://crypto.nknu.edu.tw/publications/2004IWAP.pdf)
  by Chung-Huang Yang, Chih-Ching Yeh, and Fang-Dar Chu.

Here is some documentation about AdaCore's products/libraries that Thumper uses (GtkAda and
AWS).

+ Documentation about [GtkAda](https://docs.adacore.com/gtkada-docs/gtkada_ug/_build/html/) on
  AdaCore's site. AdaCore also has a [Getting Started with
  GtkAda](https://docs.adacore.com/live/wave/gtkada/html/gtkada_ug/getting_started.html)
  document.

+ [Developing a Web Server in Ada with
  AWS](https://silo.tips/download/developing-a-web-server-in-ada-with-aws) by J-P. Rosen.

+ Documentation about the [Ada Web Server](https://docs.adacore.com/aws-docs/aws/) on AdaCore's
  site.

+ The [GitHub site for AWS](https://github.com/AdaCore/aws) is also interesting.
