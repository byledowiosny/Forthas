# assemble.s - Shell for Creating Assembly Language Modules
#
# Assemble with:       as -o <file>.o <file>.s
# Create binary with:  ld -e 0 --oformat binary -o <file>.fcm <file>.o
# In one go use:       as -o <file>.o <file>.s && ld -e 0 --oformat binary -o <file>.fcm <file>.o

.intel_syntax noprefix

# Any address references must be program counter relative. The syntax is: [rip+<label>]


# Code goes here:


# The following is code, called "next", to continue execution
# in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]

# This must be the last code executed.

# Data or containers are located here:

