# starten1.s - Module for multiplying by 10 by shifting and adding.

# Assemble with:  as -o starten1.o starten1.s && ld -e 0 --oformat binary -o starten1.fcm starten1.o

.intel_syntax noprefix

# Code goes here:

        mov   rcx, [r14]
        add   r14, 8
strt1:  mov   rax, 0xFFFFFFFFFFF
        mov   rbx, rax
        shl   rax, 2
        add   rax, rax
        shl   rbx, 1
        add   rax, rbx
        sub   rcx, 1
        jne   strt1

# The following is code, called "next", to continue execution
# in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]

