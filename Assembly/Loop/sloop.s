# sloop.s - Module for simple loop using the loop instruction.
# Gets the count from the stack.

# Assemble with: as -o sloop.o sloop.s && ld -e 0 --oformat binary -o sloop.fcm sloop.o

.intel_syntax noprefix

# Code goes here:

        mov   rcx, [r14]
        add   r14, 8
slp1:   loop  slp1

# The following is code, called "next", to continue execution
# in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]


