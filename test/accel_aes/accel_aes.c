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

// ================================================================
// Since the AES IP block reads/writes directly to memory for the key,
// cleartext and ciphertext, we use 'fence' to ensure that caches are
// empty, i.e., memory contains definitive data and caches will be
// reloaded.

static void fence (void)
{
    asm volatile ("fence");
}

// ================================================================

// Base address of the AES accelerator IP
const uint32_t base_addr = 0xC0000200;

// Test data from Cryptol AES example

// 128b key
uint8_t key [16] = { 0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6,
		     0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c };

// Single 128b block of plaintext
uint8_t pt  [16] = { 0x32, 0x43, 0xf6, 0xa8, 0x88, 0x5a, 0x30, 0x8d,
		     0x31, 0x31, 0x98, 0xa2, 0xe0, 0x37, 0x07, 0x34 };

// Single 128b block of ciphertext = encrypted(ciphertext)
uint8_t ct_out [16];

// Expected ciphertext (for verification)
uint8_t ct_expected [16] = { 0x39, 0x25, 0x84, 0x1d, 0x02, 0xdc, 0x09, 0xfb,
			     0xdc, 0x11, 0x85, 0x97, 0x19, 0x6a, 0x0b, 0x32 };

// Single 128b block of plaintext = decrypted(ciphertext)
uint8_t pt_out [16];

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
    fence ();
    aes_ip[0] = 1; while (aes_ip[0] != 0);
    fence ();

    // Do encrypt pt -> ct_out;
    addr = (uint32_t) (& (pt [0]));
    printf ("pt addr: 0x%08x\n", addr);
    aes_ip[2] = addr;
    addr = (uint32_t) (& (ct_out [0]));
    printf ("ct_out addr: 0x%08x\n", addr);
    aes_ip[3] = addr;
    aes_ip[4] = 1;
    fence ();
    aes_ip[0] = 2;
    // Await completion
    while (aes_ip[0] != 0);
    fence ();

    // Verify encrypted text
    ok = true;
    for (j = 0; j < 16; j++) {
	if (ct_out [j] != ct_expected [j]) {
          printf ("Encryption: error. Byte [%0d] is 0x%02x, expected 0x%02x\n", j, ct_out[j], ct_expected [j]);
	    ok = false;
	}
    }
    if (ok) {
      printf ("Encryption: ok\n");
    }

    // Do decrypt ct_expected -> pt_out
    addr = (uint32_t) (& (ct_expected [0]));
    printf ("ct_expected addr: 0x%08x\n", addr);
    aes_ip[2] = addr;
    addr = (uint32_t) (& (pt_out [0]));
    printf ("pt_out addr: 0x%08x\n", addr);
    aes_ip[3] = addr;
    aes_ip[4] = 1;
    fence ();
    aes_ip[0] = 3;
    // Await completion
    while (aes_ip[0] != 0);
    fence ();

    // Verify decrypted text
    ok = true;
    for (j = 0; j < 16; j++) {
	if (pt_out [j] != pt [j]) {
          printf ("Decryption: error. Byte [%0d] is 0x%02x, expected 0x%02x\n", j, pt_out[j], pt [j]);
          ok = false;
	}
    }
    if (ok) {
      printf ("Decryption: ok\n");
    }

    return 0;
}
