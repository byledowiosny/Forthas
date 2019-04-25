# xchg2.s - This module gets a loop count from the stack and does a register-to-memory
# exchange using only mov instructions.

# Assemble with: as -o xchg2.o xchg2.s && ld -e 0 --oformat binary -o xchg2.fcm xchg2.o

.intel_syntax noprefix

# Code goes here:

        mov   rcx, [r14]
        mov   rax, 2
xchg2:  mov   rbx, [r14]
        mov   [r14], rax
        mov   rax, rbx
        sub   rcx, 1
        jne   xchg2
        add   r14, 8

# The following is code, called "next", to continue execution
# in the Forthx64 inner interpreter:

        mov   r11,[r12]
        add   r12,8
        jmp   [r11]


