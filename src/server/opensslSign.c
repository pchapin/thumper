//#include <openssl/dsa.h>
#include <openssl/evp.h>
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char* argv[]) {
    
    unsigned char* = signRSAHash( , );
    
    
}

unsigned char* signRSAHash(const char* msg, size_t msg_size) {
    //Asymetric key signing using EVP. Code and information from https://wiki.openssl.org/index.php/EVP_Signing_and_Verifying
    EVP_MD_CTX* mdctx = NULL;
    int ret = 0;
    size_t slen;
    unsigned char* sig = NULL;
    
    
    
    /* Create the Message Digest Context */
    if(!(mdctx = EVP_MD_CTX_create())) goto err;
     
    /* Initialise the DigestSign operation */
     if(1 != EVP_DigestSignInit(mdctx, NULL, EVP_sha256(), NULL, key)) goto err;
     
     /* Call update with the message */
     if(1 != EVP_DigestSignUpdate(mdctx, msg, msg_size)) goto err;
     
     /* Finalise the DigestSign operation */
     /* First call EVP_DigestSignFinal with a NULL sig parameter to obtain the length of the
      * signature. Length is returned in slen */
     if(1 != EVP_DigestSignFinal(mdctx, NULL, &slen)) goto err;
    
     /* Allocate memory for the signature based on size in slen */
     if(!(sig = OPENSSL_malloc(sizeof(unsigned char) * (slen)))) goto err;
    
     /* Obtain the signature */
     if(1 != EVP_DigestSignFinal(mdctx, sig, &slen)) goto err;
     
     /* Success */
     ret = 1;
     
     err:
     if(ret != 1)
     {
       /* Do some error handling */
     }
     
     /* Clean up */
     if(sig && !ret) OPENSSL_free(sig);
     if(mdctx) EVP_MD_CTX_destroy(mdctx);
    return sig;
}
