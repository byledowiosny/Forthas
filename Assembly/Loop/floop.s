# floop.s - Module for simple loop using decrement and branch.
# Gets the count from the stack.

# Assemble with: as -o floop.o floop.s && ld -e 0 --oformat binary -o floop.fcm floop.o

.intel_syntax noprefix

# Code goes here:

        mov   rcx, [r14]
        add   r14, 8
flp1:   sub   rcx, 1
        jne   flp1

# The following is code, called "next", to continue execution
# in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]

