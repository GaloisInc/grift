#include <stdio.h>
#include "aes.h"
#include "randomkey.h"

int main()
{
  WORD w[128]; // actually only needs to be of size >= 33...?

  aes_key_setup(key, w, 128);

  BYTE encrypted_str[17];
  encrypted_str[16] = '\0';

  aes_encrypt(in_str, encrypted_str, w, 128);

  //  printf("Input: %s\n", in_str);
  //  printf("Encrypted input: %s\n", encrypted_str);

  BYTE decrypted_str[17];
  decrypted_str[16] = '\0';

  aes_decrypt(encrypted_str, decrypted_str, w, 128);

  //  printf("Decrypted input: %s\n", decrypted_str);

  return decrypted_str[5]; // should be 0x4e
}

