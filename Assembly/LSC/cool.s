# cool.s - This module uses Syscall 1 to output a message.

# Assemble with:  as -o cool.o cool.s && ld -e 0 --oformat binary -o cool.fcm cool.o

.intel_syntax noprefix

# Code goes here:

        mov   rax, 1                # Syscall 1 is write
        mov   rdi, 1                # Standard handle
        lea   rsi, [rip+str0]       # Get address of string length 
        mov   rdx, [rsi]            # Get length
        add   rsi, 8                # Advance to string

        syscall

# The following is code, called "next", to continue execution
# in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]

# Data or containers must be put after the code.

str0:   .quad end0-.-8
        .byte 10
        .ascii "This is cool!"
        .byte 10
end0:

