import random
import os

# grift takes forever with the coverage analysis it's doing now, so we only do this once
for i in range(0,1):
    f = open("randomkey.h", "w")

    f.write("  BYTE key[] = { \n")
    for i in range (0, 4):
        f.write("    ")
        for i in range (0, 8):
            f.write(str(random.randint(0, 255)) + ", ")
        f.write("\n")
    f.write("  };\n")

    f.write("  BYTE in_str[17] = { ")
    for i in range (0, 16):
        f.write(str(random.randint(0,255)) + ", ")
    f.write("};")

    f.close()

    os.system("riscv64-unknown-elf-gcc -o a aes.c aes_main.c")
    os.system("grift 1000000 a >a.out")

    with open('a.out') as out:
        first_line = out.readline()
        # print first_line

        if (first_line != 'MInstRet = 35459\n'):
            print "found aberration"
            break
