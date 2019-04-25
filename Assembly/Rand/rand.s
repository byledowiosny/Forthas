# rand.s - Return a 64-bit random number using Linux System Call 318

# Assemble with:  as -o rand.o rand.s && ld -e 0 --oformat binary -o rand.fcm rand.o

.intel_syntax noprefix

# Code goes here:

        sub   r14, 8
        mov   rdi, r14              # Use stack for buffer
        mov   rsi, 8                # Get 8 bytes
        xor   rdx, rdx              # Set no flags
        mov   rax, 318              # Call # for getrandom

        syscall

# The following is code, called "next", to continue execution
# in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]

