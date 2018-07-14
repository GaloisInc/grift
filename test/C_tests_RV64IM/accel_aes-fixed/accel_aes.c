// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved.

// This program exercises an AES "accelerator" IP block in the SoC.
// The IP block is configured and queried by writing and reading,
// respectively, into a set of 32b CSRs (Config and Status Regs)
// that are memory mapped at a given base address.

//   base_addr  [0]: Command/status port (see command list below)
//   base_addr  [4]: memory address of 128b AES key
//   base_addr  [8]: memory address of source text buffer (N x 128b blocks)
//   base_addr [12]: memory address of target text buffer (N x 128b blocks)
//   base_addr [16]: N (# of 128-bit blocks to encrypt/decrypt)
//
// Writes into base_addr[0] are commands. The commands are:
//   0: ignored
//   1: 'do key expansion for encryptor'
//   2: 'encrypt from source to target'
//   3: 'decrypt from source to target'
// Reads from base_addr[0] allow the CPU to check whether the last
// command has completed:
//   => 0:  key expansion/encryption/decryption completed
//   => 1/2/3: key expansion/encryption/decryption still running


#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

// Base address of the AES accelerator IP
const uint32_t base_addr = 0xC0000200;

// Test data from Cryptol AES example

// 128b key
uint32_t key [8]         = { 0x09cf4f3c, 0xabf71588, 0x28aed2a6, 0x2b7e1516 };

// Single 128b block of plaintext
uint32_t pt [8]          = { 0xe0370734, 0x313198a2, 0x885a308d, 0x3243f6a8 };

// Single 128b block of ciphertext = encrypted(ciphertext)
uint32_t ct_out [8];

// Expected ciphertext (for verification)
uint32_t ct_expected [8] = { 0x196a0b32, 0xdc118597, 0x02dc09fb, 0x3925841d };

// Single 128b block of plaintext = decrypted(ciphertext)
uint32_t pt_out [8];

int main (int argc, char *argv[])
{
    int j;
    bool ok;
    uint32_t addr;
    uint32_t *aes_ip = (uint32_t *) base_addr;    // base addr of CSRs of accelerator

    // Set the key
    addr = (uint32_t) (& (key [0]));
    printf ("key addr: 0x%08x\n", addr);
    aes_ip[1] = addr;
    aes_ip[0] = 1; while (aes_ip[0] != 0);

    // Do encrypt pt -> ct_out;
    addr = (uint32_t) (& (pt [0]));
    printf ("pt addr: 0x%08x\n", addr);
    aes_ip[2] = addr;
    addr = (uint32_t) (& (ct_out [0]));
    printf ("ct_out addr: 0x%08x\n", addr);
    aes_ip[3] = addr;
    aes_ip[4] = 1;
    aes_ip[0] = 2;
    // Await completion
    while (aes_ip[0] != 0);

    // Verify encrypted text
    ok = true;
    for (j = 0; j < 4; j++) {
	if (ct_out [j] != ct_expected [j]) {
	    printf ("Encryption: error. Word [%0d] is 0x%08x, expected 0x%08x\n", j, ct_out[j], ct_expected [j]);
	    ok = false;
	}
    }
    if (ok)
	printf ("Encryption: ok\n");

    // Do decrypt ct_expected -> pt_out
    addr = (uint32_t) (& (ct_expected [0]));
    printf ("ct_expected addr: 0x%08x\n", addr);
    aes_ip[2] = addr;
    addr = (uint32_t) (& (pt_out [0]));
    printf ("pt_out addr: 0x%08x\n", addr);
    aes_ip[3] = addr;
    aes_ip[4] = 1;
    aes_ip[0] = 3;
    // Await completion
    while (aes_ip[0] != 0);

    // Verify decrypted text
    ok = true;
    for (j = 0; j < 4; j++) {
	if (pt_out [j] != pt [j]) {
	    printf ("Decryption: error. Word [%0d] is 0x%08x, expected 0x%08x\n", j, pt_out[j], pt [j]);
	    ok = false;
	}
    }
    if (ok)
	printf ("Decryption: ok\n");

    return 0;
}
