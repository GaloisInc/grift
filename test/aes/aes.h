/*********************************************************************
* Filename:   aes.h
* Author:     Brad Conte (brad AT bradconte.com)
* Copyright:
* Disclaimer: This code is presented "as is" without any guarantees.
* Details:    Defines the API for the corresponding AES implementation.
*********************************************************************/

#ifndef AES_H
#define AES_H

/*************************** HEADER FILES ***************************/
#include <stddef.h>

/****************************** MACROS ******************************/
#define AES_BLOCK_SIZE 16               // AES operates on 16 bytes at a time

/**************************** DATA TYPES ****************************/
typedef unsigned char BYTE;            // 8-bit byte
typedef unsigned int WORD;             // 32-bit word, change to "long" for 16-bit machines

/*********************** FUNCTION DECLARATIONS **********************/
///////////////////
// AES
///////////////////
// Key setup must be done before any AES en/de-cryption functions can be used.

/*@ requires \valid_read(key + (0 .. (keysize/8)-1));
    requires \valid(w + (0 .. 4*(7 + keysize/32)-1));
    requires (keysize == 128 || keysize == 192 || keysize == 256);
    requires \separated(key + (0 .. (keysize/8)-1),w + (0 .. 4*(7 + keysize/32)-1));
    assigns w[0 .. 4*(7 + keysize/32)-1];
*/
void aes_key_setup(const BYTE key[],          // The key, must be 128, 192, or 256 bits
                   WORD w[],                  // Output key schedule to be used later
                   int keysize);              // Bit length of the key, 128, 192, or 256

/*@ requires \valid_read(in + (0 .. AES_BLOCK_SIZE-1));
    requires \valid(out + (0 .. AES_BLOCK_SIZE-1));
    requires \valid_read(key + (0 .. 4*(7 + keysize/32)-1));
    requires (keysize == 128 || keysize == 192 || keysize == 256);
    requires \separated(in + (0 .. AES_BLOCK_SIZE-1),out + (0 .. AES_BLOCK_SIZE-1));
    requires \separated(key + (0 .. 4*(7 + keysize/32)-1),out + (0 .. AES_BLOCK_SIZE-1));
    assigns out[0 .. AES_BLOCK_SIZE-1];
*/
void aes_encrypt(const BYTE in[],             // 16 bytes of plaintext
                 BYTE out[],                  // 16 bytes of ciphertext
                 const WORD key[],            // From the key setup
                 int keysize);                // Bit length of the key, 128, 192, or 256

/*@ requires \valid_read(in + (0 .. AES_BLOCK_SIZE-1));
    requires \valid(out + (0 .. AES_BLOCK_SIZE-1));
    requires \valid_read(key + (0 .. 4*(7 + keysize/32)-1));
    requires (keysize == 128 || keysize == 192 || keysize == 256);
    requires \separated(in + (0 .. AES_BLOCK_SIZE-1),out + (0 .. AES_BLOCK_SIZE-1));
    requires \separated(key + (0 .. 4*(7 + keysize/32)-1),out + (0 .. AES_BLOCK_SIZE-1));
    assigns out[0 .. AES_BLOCK_SIZE-1];
*/
void aes_decrypt(const BYTE in[],             // 16 bytes of ciphertext
                 BYTE out[],                  // 16 bytes of plaintext
                 const WORD key[],            // From the key setup
                 int keysize);                // Bit length of the key, 128, 192, or 256

#endif   // AES_H
