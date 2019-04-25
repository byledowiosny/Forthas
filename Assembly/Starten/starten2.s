# starten2.s - Module for multiplying by 10 using mul instruction.

# Assemble with:  as -o starten2.o starten2.s && ld -e 0 --oformat binary -o starten2.fcm starten2.o

.intel_syntax noprefix

# Code goes here:

        mov   rcx, [r14]
        add   r14, 8
        mov   rbx, 10

strt2:  mov   rax, 0xFFFFFFFFFFF
        mul   rbx
        sub   rcx, 1
        jne   strt2

# The following is code, called "next", to continue execution
# in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]

