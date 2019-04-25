# Marsenne Twister Assembly Module

# To include this module in the source file, make sure this file, the C-source,
# twister64.c, and the header file, mt64.h are in a folder named "twister64"
# in the same folder as the Forthx64 source and uncomment the line:

# .include  "twister64/twister64.s"

# in the source file in the section titled "Include external modules here".

# To assemble, compile, and link for Twister, include the C program file twister64.c 
# as follows:

#   gcc -no-pie forthx.s -o forthx forthx.c twister64/twister64.c -ludis86 -lm
#      -lquadmath

# Twister64

    .global  init_genrand64, genrand64_int64

# twist64 - Return a 64-bit random integer using the Marsenne Twister Algorithm.
#          Seeded by the Linux system call getrandom (#318).

head    "twist64", ario
twstsf: .quad twtsf0
textm
twtsf0: sub   r14, 8                # Make room on stack
        mov   rdi, r14              # Use stack for buffer

# Get seed.

        mov   rsi, 8                # Get 8 bytes
        xor   rdx, rdx              # Set no flags
        mov   rax, 318              # Call # for getrandom

        syscall

# Initialize Mersenne Twister.

        mov   rdi, [r14]            # Get seed value

        CFCm  init_genrand64

# Get 64-bit random number.

        CFCm  genrand64_int64

        mov   [r14], rax
        nextm
datam

