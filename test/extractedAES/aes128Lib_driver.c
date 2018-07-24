/* Example driver program for aes128Lib. */
/* Automatically generated by SBV. Edit as you see fit! */

#include <stdio.h>
#include "aes128Lib.h"

void aes128KeySchedule_driver(void)
{
  const SWord32 key[4] = {
    0x50b9ca4fUL, 0xd3619c69UL, 0x2bf68859UL, 0xce897221UL
  };

  // printf("Contents of input array key:\n");
  int key_ctr;
  for(key_ctr = 0; key_ctr < 4 ; ++key_ctr)
    ; // printf("  key[%d] = 0x%08"PRIx32"UL\n", key_ctr, key[key_ctr]);

  SWord32 encKS[44];

  aes128KeySchedule(key, encKS);

  // printf("aes128KeySchedule(key, encKS) ->\n");
  int encKS_ctr;
  for(encKS_ctr = 0; encKS_ctr < 44 ; ++encKS_ctr)
    ; // printf("  encKS[%d] = 0x%08"PRIx32"UL\n", encKS_ctr, encKS[encKS_ctr]);
}

void aes128BlockEncrypt_driver(void)
{
  const SWord32 pt[4] = {
      0xdce36f9cUL, 0xab3b5d5dUL, 0xe5c62926UL, 0x90544bbbUL
  };

  ; // printf("Contents of input array pt:\n");
  int pt_ctr;
  for(pt_ctr = 0; pt_ctr < 4 ; ++pt_ctr)
    ; // printf("  pt[%d] = 0x%08"PRIx32"UL\n", pt_ctr, pt[pt_ctr]);

  const SWord32 xkey[44] = {
      0xe2479a36UL, 0x910f3e76UL, 0xa618be3dUL, 0x76f05bd3UL,

      0xc5b2b2b7UL, 0x8a5c7c9dUL, 0x8bce9a1fUL, 0x58bab8dfUL,
      0x68b12fc0UL, 0xdc748883UL, 0xb598b192UL, 0xa46f9a4fUL,
      0xe3a7b94cUL, 0x8279efaeUL, 0xb13543feUL, 0x1ea3030eUL,
      0x829c366bUL, 0xef77f643UL, 0xa7c9de0cUL, 0x1ecf06ceUL,
      0x4c260d26UL, 0x073aa726UL, 0x12f049bfUL, 0x661795a4UL,
      0x24db5086UL, 0xe0178b96UL, 0x02221909UL, 0xc0eac0b7UL,
      0x6805a48cUL, 0xc404fe16UL, 0x8b33e810UL, 0x038dc3e4UL,
      0x528d1552UL, 0x109b70b1UL, 0xe6657d08UL, 0x439606c3UL,
      0xe4b53e26UL, 0x8ea320a4UL, 0xf8b3b604UL, 0x7d614787UL,
      
      0x01a0327aUL, 0x7d5d4381UL, 0xeb06ff97UL, 0x1f067368UL
  };

  ; // printf("Contents of input array xkey:\n");
  int xkey_ctr;
  for(xkey_ctr = 0; xkey_ctr < 44 ; ++xkey_ctr)
    ; // printf("  xkey[%d] = 0x%08"PRIx32"UL\n", xkey_ctr, xkey[xkey_ctr]);

  SWord32 ct[4];

  aes128BlockEncrypt(pt, xkey, ct);

  ; // printf("aes128BlockEncrypt(pt, xkey, ct) ->\n");
  int ct_ctr;
  for(ct_ctr = 0; ct_ctr < 4 ; ++ct_ctr)
    ; // printf("  ct[%d] = 0x%08"PRIx32"UL\n", ct_ctr, ct[ct_ctr]);
}

void aes128BlockDecrypt_driver(void)
{
  const SWord32 ct[4] = {
      0x4c87e0c0UL, 0x5db304c1UL, 0x1b8527cfUL, 0xa66dee25UL
  };

  ; // printf("Contents of input array ct:\n");
  int ct_ctr;
  for(ct_ctr = 0; ct_ctr < 4 ; ++ct_ctr)
    ; // printf("  ct[%d] = 0x%08"PRIx32"UL\n", ct_ctr, ct[ct_ctr]);

  const SWord32 xkey[44] = {
      0x4c939536UL, 0x16e2c3fcUL, 0x46e678a1UL, 0x6d309451UL,
      0xfab6603dUL, 0x854870c8UL, 0x16c52e78UL, 0xf80a35eeUL,
      0x8722187fUL, 0xc35efa12UL, 0x413d9721UL, 0x465d9ddcUL,
      0x408c5490UL, 0xde022ca4UL, 0xca0b0259UL, 0x79695b80UL,
      0xd747ecc5UL, 0x4c5699eeUL, 0x5b3abf33UL, 0x89ce8f2aUL,
      0xb79b919bUL, 0x140a1f50UL, 0x6ea9b947UL, 0x22c1079bUL,
      0x6bb7b927UL, 0x81ebc200UL, 0x595c8f14UL, 0x12fafe89UL,
      0x9c7af531UL, 0xca36dcafUL, 0xbc2a480bUL, 0x7b39724eUL,
      0x4e946c9fUL, 0xc87b6a78UL, 0xb16b2ecaUL, 0x7c2efeb2UL,
      0x76f9ca7bUL, 0x51982bc7UL, 0xeef6a59bUL, 0x568489f4UL,
      0x2a3d154eUL, 0x9ec8a434UL, 0x2a431099UL, 0xe1ff43e1UL
  };

  ; // printf("Contents of input array xkey:\n");
  int xkey_ctr;
  for(xkey_ctr = 0; xkey_ctr < 44 ; ++xkey_ctr)
    ; // printf("  xkey[%d] = 0x%08"PRIx32"UL\n", xkey_ctr, xkey[xkey_ctr]);

  SWord32 pt[4];

  aes128BlockDecrypt(ct, xkey, pt);

  ; // printf("aes128BlockDecrypt(ct, xkey, pt) ->\n");
  int pt_ctr;
  for(pt_ctr = 0; pt_ctr < 4 ; ++pt_ctr)
    ; // printf("  pt[%d] = 0x%08"PRIx32"UL\n", pt_ctr, pt[pt_ctr]);
}

int main(void)
{
  ; // printf("====================================\n");
  ; // printf("** Driver run for aes128KeySchedule:\n");
  ; // printf("====================================\n");
  aes128KeySchedule_driver();

  ; // printf("=====================================\n");
  ; // printf("** Driver run for aes128BlockEncrypt:\n");
  ; // printf("=====================================\n");
  aes128BlockEncrypt_driver();

  ; // printf("=====================================\n");
  ; // printf("** Driver run for aes128BlockDecrypt:\n");
  ; // printf("=====================================\n");
  aes128BlockDecrypt_driver();

  return 0;
}