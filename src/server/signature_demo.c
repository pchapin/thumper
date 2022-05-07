
#include <stdlib.h>
#include <stdio.h>

// GNATstudio doesn't find this header file even though the compiler does.
// It may be related to where the -I option is placed in the project file (a guess).
#include <openssl/evp.h>

// Forward declaration.
unsigned char *sign_RSA_hash( const char *message, size_t message_size );

int main (int argc, char *argv[])
{
    int return_code;

    unsigned char *signature = sign_RSA_hash( "Hello", 6 );
    if (signature == NULL) {
        return_code = EXIT_FAILURE;
    }
    else {
        // We have a signature. Do something with it! Maybe print it??
        // ...
        OPENSSL_free( signature );
        return_code = EXIT_SUCCESS;
    }
    return return_code;
}


unsigned char *sign_RSA_hash( const char *message, size_t message_size )
{
    // Asymetric key signing using EVP.
    // Code and information from: https://wiki.openssl.org/index.php/EVP_Signing_and_Verifying
    EVP_MD_CTX *mdctx = NULL;
    unsigned char *signature = NULL;
    size_t signature_length;

    // Create the message digest context.
    if( !( mdctx = EVP_MD_CTX_create( ) ) ) {
        printf("Error: Unable to create digest context!\n");
    }
    // Initialise the DigestSign operation.
    // TODO: Read the key file to get something meaningful for 'key'
    else if( EVP_DigestSignInit( mdctx, NULL, EVP_sha256( ), NULL, NULL /* key */) != 1 ) {
        printf("Error: Unable to initialize the DigestSign operation!\n");
        EVP_MD_CTX_destroy( mdctx );
    }
    // Call update with the message.
    else if( EVP_DigestSignUpdate( mdctx, message, message_size ) != 1 ) {
        printf("Error: Unable to execute DigestSignUpdate!\n");
        EVP_MD_CTX_destroy( mdctx );
    }
    // Finalise the DigestSign operation.
    // First call EVP_DigestSignFinal with a NULL sig parameter to obtain the length of the signature.
    else if( EVP_DigestSignFinal( mdctx, NULL, &signature_length ) != 1) {
        printf("Error: Unable to find the length of the signature!\n");
        EVP_MD_CTX_destroy( mdctx );
    }
    // Allocate memory for the signature based on size in signature_length.
    else if( !( signature = OPENSSL_malloc( signature_length * sizeof(unsigned char) ) ) ) {
        printf("Error: Unable to allocate memory for the signature!\n");
        EVP_MD_CTX_destroy( mdctx );
    }
    // Obtain the signature.
    else if( EVP_DigestSignFinal( mdctx, signature, &signature_length ) != 1) {
        printf("Error: Unable to obtain the signature!\n");
        OPENSSL_free( signature );
        signature = NULL;
        EVP_MD_CTX_destroy( mdctx );
    }
    else {
        // It worked! Clean up.
        EVP_MD_CTX_destroy( mdctx );
    }
    return signature;
}
