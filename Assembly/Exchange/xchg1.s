# xchg1.s - This module gets a loop count from the stack and does a register-to-memory
# exchange using the xchg instruction.

# Assemble with: as -o xchg1.o xchg1.s && ld -e 0 --oformat binary -o xchg1.fcm xchg1.o

.intel_syntax noprefix

# Code goes here:

        mov   rcx, [r14]
        mov   rax, 2
xchg1:  xchg  [r14], rax
        sub   rcx, 1
        jne   xchg1
        add   r14, 8

# The following is code, called "next", to continue execution
# in the Forthx64 inner interpreter:

        mov   r11,[r12]
        add   r12,8
        jmp   [r11]

