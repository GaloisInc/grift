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

// This version of the accel_aes tests encrypts a zero block with a
// zero key, and decrypts it back.

int main (int argc, char *argv[])
{
    // 128b key
    uint8_t key [16] = { 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
			 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 };

    // Single 128b block of plaintext
    uint8_t pt [16]  = { 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
			 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 };

    // Single 128b block of ciphertext = encrypted (ciphertext)
    uint8_t ct_out [16];

    // Single 128b block of plaintext = decrypted (ciphertext)
    uint8_t pt_out [16];

    int j;
    bool ok;
    uint32_t addr;
    uint32_t *aes_ip = (uint32_t *) base_addr;    // base addr of CSRs of accelerator

    // Set the key
    printf ("Setting key\n");
    addr = (uint32_t) (& (key [0]));
    printf ("key addr: 0x%08x\n", addr);
    aes_ip[1] = addr;
    fence ();
    aes_ip[0] = 1; while (aes_ip[0] != 0);
    fence ();

    // Do encrypt pt -> ct_out;
    printf ("Encrypting\n");
    addr = (uint32_t) (& (pt [0]));
    printf ("  from pt addr: 0x%08x\n", addr);
    aes_ip[2] = addr;
    addr = (uint32_t) (& (ct_out [0]));
    printf ("  to   ct_out addr: 0x%08x\n", addr);
    aes_ip[3] = addr;
    aes_ip[4] = 1;
    fence ();
    aes_ip[0] = 2;
    // Await completion
    while (aes_ip[0] != 0);
    fence ();

    // Print encrypted text
    for (j = 0; j < 16; j++) {
	printf ("  Encrypted: Byte [%0d] is 0x%02x\n", j, ct_out[j]);
    }

    // Do decrypt ct_out -> pt_out
    printf ("Decrypting\n");
    addr = (uint32_t) (& (ct_out [0]));
    printf ("  from ct_out addr: 0x%08x\n", addr);
    aes_ip[2] = addr;
    addr = (uint32_t) (& (pt_out [0]));
    printf ("  to   pt_out addr: 0x%08x\n", addr);
    aes_ip[3] = addr;
    aes_ip[4] = 1;
    fence ();
    aes_ip[0] = 3;
    // Await completion
    while (aes_ip[0] != 0);
    fence ();

    // Print decrypted text
    for (j = 0; j < 16; j++) {
	printf ("  Decrypted: Byte [%0d] is 0x%02x\n", j, pt_out[j]);
    }

    return 0;
}
