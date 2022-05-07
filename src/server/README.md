
Server README
=============

The file `server.key` is a private key for the server. The file `server.crt` is a self-signed
certiicate for the server containing the server's public key. The key files are for
demonstration and testing purposes only. Regenerate them as described below when the server is
deployed.

The file `signature_demo.c` is a standalone program that illustrates how to create digital
signatures using OpenSSL. To build the demo program use a command similar to:

    $ gprbuild -Pthumper.gpr signature_demo.c

The executable will be left in the `build` folder.

Generating Keys
---------------

This section explains how to generate an RSA public/private key pair using the command line
OpenSSL tool.

The command for generating the private key is:

    oopenssl genrsa -aes256 -passout pass:createAPassword -out server.pass.key 2048

Documentation on the `openssl genrsa` command can be found
[here](https://www.openssl.org/docs/man1.1.1/man1/openssl-genrsa.html).

The command to write the RSA key from server.pass.key to server.key is:

    openssl rsa -passin pass:thePasswordYouCreated -in server.pass.key -out server.key

Documentation on the `openssl rsa` command can be found
[here](https://www.openssl.org/docs/man1.1.1/man1/openssl-rsa.html).

You can now delete the server.pass.key leaving just the server.key.

You now need to create a certificate signing request (CSR). The command for generating the CSR
file from the private key is:

    openssl req -new -key server.key -out server.csr

You will be prompted with a series of questions to indicate your identity. Documentation on the
`openssl req` command can be found
[here](https://www.openssl.org/docs/man1.0.2/man1/openssl-req.html)

Now that a .csr file exists. It needs to be signed to create a certificate containing the public
key (.crt). The command for self-signing the server.csr using your own server.key private key
is:

    openssl x509 -req -sha256 -days 730 -in server.csr -signkey server.key -out server.crt

This will create a self-signed certificate containing your public key that has an expiration
date in 730 days (2 years). Documentation for the `openssl x509` command can be found
[here](https://www.openssl.org/docs/man1.1.1/man1/x509.html).

The certificate signing request (server.csr) file is no longer needed and can be removed.

You now have an RSA 2048-bit public/private key pair.
