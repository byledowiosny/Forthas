# x87 FPU Interface for Forthx64

# (c) Copyright 2018 by John F. Healy. All rights reserved.
#
# FPUmath.s is distributed under the terms of the 2-clause BSD License.
# Copyright (c) 2018, John F. Healy <healyjohnf@gmail.com>
# All rights reserved.
# Redistribution and use in source and binary forms, with or without modification,
# are permitted provided that the following conditions are met:
# 1. Redistributions of source code must retain the above copyright notice
#    this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
# ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# This module provides an interface to the x87 FPU that is an integral
# part of the x86_64 processor. It uses the eight x87 registers as an
# 8-level floating-point stack. Operands are passed in and out through
# the Forthx64 data stack. The floating-point stack is completely
# independent and uses its own stack operators, such as `fdup`, `fswap`
# `fover`, `drop`, etc. All elementary functions that can be programmed
# with x87 instructions are available.
# Single-precision, double-precision, and extended-precision IEEE data
# can be exchanged between the x87 and Forthx64's data stack, however
# all calculations are done in extended-precision by the FPU itself.
# The floating-point stack is managed by a 4-bit counter (fpcnt) 
# in the System Flags variable - bits 32-35. If the value of these bits
# is 0, the stack is empty. If the value is 8, the stack is full.
# The counter is reset to 0 by the `finit` instruction.
# The count is incremented for each number loaded and decremented
# for each number popped. If a pop is attempted when the counter is zero,
# execution is interrupted, and the stack underflow message is issued.
# If a load is attempted when the counter is 8, the bottom stack number
# is dumped and the new number is loaded to st(0). The stack underflow
# and overflow conditions are the reason for the counter, fpcnt.
# If the actual condition is allowed to happen, a stack fault results,
# producing a NaN (Not a Number), and causing subsequent calculations
# to go awry.

# This module is included by default in the Forthx64 source file in the section
# titled "Include external modules here". To exclude it, simply comment out
# the line:
#
#   .include "FPUmath.s"

# No changes need to be made to the assemble, compile, and link command line.

# Macros to Keep Track of Floating-Point Registers

# The macros below implement the floating-point stack management scheme.
# The scheme resembles the old HP RPN calculators that had a four-level
# stack, except this stack is eight levels.

# Macro to put fp register count in rbx.

.macro  fpcntm

        xor   rbx, rbx
        mov   bl, byte ptr[rip+flgs0+4]
        and   rbx, 0xF

.endm

# Macro to increment counter

.macro  icntm

        add   byte ptr[rip+flgs0+4], 1

.endm

# Macro to decrement counter

.macro  dcntm

        sub   byte ptr[rip+flgs0+4], 1

.endm

# Macro for loading an fp register. If fpcnt is not 8, increment fpcnt
# in advance of the load. If fpcnt is 8, make room for the new number
# by dumping the bottom stack number.

.macro  fpldm

        bt    word ptr[rip+flgs0+4], 3
        jnc   66f

        fdecstp
        fstp  st
        jmp   67f

66:
  
        icntm

67:

.endm

# Macro for popping an fp register. Throw 'floating-point stack is empty'
# error message if fpcnt is 0. Otherwise decrement fpcnt in advance
# of the pop.

.macro  fpstm

        fpcntm
        cmp   rbx, 0
        jne   67f

        throwm 131

67:

        dcntm

.endm

# Macro for single operand instructions
# If operand is not present, avoid stack fault by throwing error,
# 'not enough operands on floating-point stack'.

.macro  fp1opm

        fpcntm
        cmp   rbx, 1
        jnc   67f

        throwm 132

67:
.endm

# Macro for 2-operand instructions
# If two operands are not present, avoid stack fault by throwing error,
# 'not enough operands on floating-point stack'.

.macro  fp2opm

        fpcntm
        cmp   rbx, 2
        jnc   68f

        throwm 132

68:
.endm

# Macro for 3-operand instructions
# If two operands are not present, avoid stack fault by throwing error,
# 'not enough operands on floating-point stack'.

.macro fp3opm

        fpcntm
        cmp   rbx, 3
        jnc   69f

        throwm 132

69:
.endm

# Macro to save all x87 registers to the location given as parameter
# (so far only fprgs0). The location must be able to accommodate
# at least 16 quads.

.macro  savefprgsm area
        mov   qword ptr[rip+\area-8], 256

.set    stp,  0

        sub   r14, 16

.rept   8

        mov   qword ptr[r14+8], 0       # Clear high order quad
        fstpt [r14]                     # Get 1st/next register
        mov   rax, [r14]
        mov   [rip+\area+stp], rax
        mov   rax, [r14+8]
        mov   [rip+\area+stp+8], rax
        fldt  [r14]
        fincstp

.set    stp,  stp+16

.endr
        add   r14, 16
.endm

# Macro to truncate to integer. Sets rounding mode to chop (round toward zero)
# before rounding to integer. Saves and restores the previous rounding mode.

.macro  fintm

        fstcw [rip+fcw0]
        fstcw [rip+fcw0+4]
        bts   word ptr[rip+fcw0], 11
        bts   word ptr[rip+fcw0], 10
        fldcw [rip+fcw0]
        frndint
        fldcw [rip+fcw0+4]

.endm

# Macro to compute 2^x. Used for calculating exponentials.

.macro  ftwttxm

        fpldm
        fld   st(0)
        fintm
        fsub  st(1), st(0)
        fpldm
        fld1
        fscale
        fstp  st(1)
        dcntm
        fxch
        f2xm1
        fpldm
        fld1
        faddp st(1)
        dcntm
        fmulp
        dcntm

.endm

# Macro to find the natural logarithm of st0

.macro  flnm

        fpldm
        fld1
        fxch
        fyl2x
        dcntm
        fpldm
        fldl2e
        fdivp
        dcntm
        fwait

.endm

# Macro to find the natural logarithm of st0+1

.macro  fln1pm

        fpldm
        fld1
        fxch
        fyl2xp1
        dcntm
        fpldm
        fldl2e
        fdivp
        dcntm
        fwait

.endm

# finit - Initialize the FPU. Clear counter in flgs0.

head    "finit", fpmo
fnit:   .quad fnit0
textm
fnit0:  btr   word ptr[rip+flgs0+4], 0
        btr   word ptr[rip+flgs0+4], 1
        btr   word ptr[rip+flgs0+4], 2
        btr   word ptr[rip+flgs0+4], 3

        finit
        nextm
datam

# fwait - Wait for processor to test for pending unmasked floating-point
# exceptions before proceeding.

head    "fwait", fpmo
fpwat:  .quad fpwat0
textm
fpwat0: fwait
        nextm
datam

# fclex - Clear floating-point flags.

head    "fclex", fpmo
fclx:   .quad fclx0
textm
fclx0:  fclex
        nextm
datam

# fxsave - Save the floating-point registers to fprgs0.

head    "fxsave", fpmo
fxsve:  .quad fxsve0
textm
fxsve0: fxsave  [rip+fpusv0]
        nextm
datam

# savefp - Save the x86 floating-point registers to fprgs0.

head    "savefp", fpmo
svefp:  .quad svefp0
textm
svefp0:
        savefprgsm fprgs0
        nextm
datam

# fptos - Return the number of the register which is at the top
# of the floating-point stack (0-7).

head    "fptos", fpmo
fptos:  .quad fptos0
textm
fptos0: xor   rax, rax
        fstsw ax
        shr   rax, 11
        and   rax, 7
        sub   r14, 8
        mov   [r14], rax
        nextm
datam

# fxam - Test the FPU operand in st0 and put the resulting x87
# status word on the stack.

head    "fxam", fpmo
fexam:  .quad fexam0
textm
fexam0: xor     rax, rax
        fxam
        fnstsw  ax
        shr     rax, 8
        and     rax, 0x47
        btr     rax, 6
        jnc     fexam1

        bts     rax, 3

fexam1: sub     r14, 8
        mov     [r14], rax
        nextm
datam

# (.fxm) - Display the contents of the top of the floating-point stack as a
# real number based on the value returned by fxam. Because of the idiotic
# numbering of the fxam results (13 is left out), this word uses its own
# special code-field routine that maps 14 to 13. It is designed to be used
# with `fxam` and does no bounds checking. Therefore it is hidden.

head    "(.fxm)", fpmo, hidden
pdxm:   .quad pdfp0
        .quad pdxm1,pdxm2,pdxm3,pdxm4,pdxm5,pdxm6,pdxm7,pdxm8
        .quad pdxm9,pdxm10,pdxm11,pdxm12,pdxm13,pdxm14

# Special jump table routine for `(.fp)`. It maps a 14 returned by fxam
# to 13 for the index.

textm
pdfp0:  mov   rdx, [r14]                # Get index
        add   r14, 8                    # Pop stack
        cmp   rdx, 14                   # See if 14
        jnz   pdfp1                     # Branch if not

        sub   rdx, 1                    # Subtract 1

pdfp1:  mov   r11, [r11+rdx*8+8]        # Get corresponding vector
        jmp   [r11]
datam

pdxm1:  .quad docl0
        pdtqm "+unsupported format"
        .quad semis

pdxm2:  .quad docl0
        pdtqm "+NaN"
        .quad semis

pdxm3:  .quad docl0
        pdtqm "-unsupported format"
        .quad semis

pdxm4:  .quad docl0
        pdtqm "-NaN"
        .quad semis

pdxm5:  .quad docl0
        .quad ftxp,ddup,xptf,xptre,dotre,semis

pdxm6:  .quad docl0
        pdtqm "+infinity"
        .quad semis

pdxm7:  .quad docl0
        .quad ftxp,ddup,xptf,xptre,dotre,semis

pdxm8:  .quad docl0
        pdtqm "-infinity"
        .quad semis

pdxm9:  .quad docl0
        .quad zero,zero,xptre,dotre,semis
        .quad semis

pdxm10: .quad docl0
        pdtqm "+empty"
        .quad semis

pdxm11: .quad docl0
        pdtqm "-0"
        .quad semis

pdxm12: .quad docl0
        pdtqm "-empty"
        .quad semis

pdxm13: .quad docl0
        pdtqm "+denormal"
        .quad semis

pdxm14: .quad docl0
        pdtqm "-denormal"
        .quad semis

# .fxm - Display the contents of the top of the floating-point stack as a
# real number based on the value returned by fxam.

# : .fxm   fxam (.fxm) ;

head    ".fxm", fpmo
dtfxm:  .quad docl0
        .quad fexam,pdxm,semis

# ckfpstk - ( n --- ) - Check if there are at least n items on the
# floating-point stack and throw an error if not.

head    "ckfpstk"
ckfps:  .quad ckfps0
textm
ckfps0: fpcntm                          # Get count
        add   r14, 8                    # Prepop stack
        cmp   rbx, [r14-8]              # Compare required ops
        jnc   ckfps1                    # Branch if enough

        throwm 132                      # Throw not enough operands

ckfps1: nextm
datam

# .fpstk - Display the contents of the floating-point stack in a column
# of real numbers. If rdigs is not 64 or less, temporarily set it to 64.
# This admittedly arbitrary number is the maximum number of meaningful
# binary digits for numbers greater than 1, and the limit is intended
# to prevent the inconveniently large amount of output that that would
# be generated if rdigs were even larger.

head    ".fpstk", fpmo
dfstk:  .quad docl0
        .quad rdigs,qat,asto,lit,64,great,zbran,dfst1
        .quad lit,64,rdigs,stor
dfst1:  .quad fdcst,cr
        pdtqm "st(7) = "
        .quad dtfxm
        .quad fdcst,cr
        pdtqm "st(6) = "
        .quad dtfxm
        .quad fdcst,cr
        pdtqm "st(5) = "
        .quad dtfxm
        .quad fdcst,cr
        pdtqm "st(4) = "
        .quad dtfxm
        .quad fdcst,cr
        pdtqm "st(3) = "
        .quad dtfxm
        .quad fdcst,cr
        pdtqm "st(2) = "
        .quad dtfxm
        .quad fdcst,cr
        pdtqm "st(1) = "
        .quad dtfxm
        .quad fdcst,cr
        pdtqm "st(0) = "
        .quad dtfxm
        .quad froma,rdigs,stor,semis

# fpstkmsg - Set `msgout` to output the FPU stack upon successful
# completion of interpret loop.

# fpstkmsg - Set `msgout` to output the FPU stack.

# : fpstkmsg   4 punch msgout ;

head    "fpstkmsg", hlfo
fstkm:  .quad docl0
        .quad four,lit,msgout,pnch,semis

# .fpstks - Show the Forthx64 data stack display followed
# by the floating-point stack display.

# : .fpstks   .stk .fpstk ;

head    ".fpstks", hlfo
fpstks: .quad docl0
        .quad dtstk,dfstk,semis

# fpstksmsg - Set `msgout` to output the Forthx64 data stack
# followed by the FPU stack.

# : fpstksmsg   5 punch msgout ;

head    "fpstksmsg", hlfo
fpstsm: .quad docl0
        .quad lit,5,lit,msgout,pnch,semis

# fpstkerr - Error handler for `.fpstk`.
# If the entry in st0 can't be displayed in the current quadsize,
# set it to infinity, so the error won't recurr.

head    "fpstkerr", hlfo
fpserr: .quad docl0
        .quad fdrop,finf,errout,semis

# fstatus - Display the contents of the floating-point stack
# and the fstack depth.

# : fstatus   .fpstk cr ." fdepth=" fdepth . ;

head    "fstatus", fpmo
fstts:  .quad docl0
        .quad dfstk,cr
        pdtqm "fdepth="
        .quad fdpth,dot,cr
        pdtqm "rounding mode: "
        .quad dtfrm,semis

# fincstp - Increment the FPU stack pointer.
# This operator is hidden, since it can disrupt
# FPU stack handling.

head    "fincstp", fpmo, hidden
fncst:  .quad fncst0
textm
fncst0: fincstp
        nextm
datam

# fdecstp - Decrement the FPU stack pointer.
# This operator is hidden, since it can disrupt
# FPU stack handling.

head    "fdecstp", fpmo, hidden
fdcst:  .quad fdcst0
textm
fdcst0: fdecstp
        nextm
datam

# fcc> - Put the x87 Condition Code nibble on the stack.

head    "fcc>", fpmo
fccf:   .quad fccf0
textm
fccf0:  xor   rax, rax
        fstsw ax
        shr   rax, 8                    # Shift right to CC bits
        btr   rax, 3                    # Clear bit #3
        bt    rax, 6                    # See if C3 is set
        jnc   fccf1                     # Branch if clear

        bts   rax, 3                    # Else set bit #3

fccf1:  and   rax, 0xF                  # Clear all higher bits
        sub   r14, 8                    # Make room on stack
        mov   [r14], rax
        nextm
datam

# .fcc - Display the FPU Condition Codes as a binary nibble.

# : .fcc   base @ >a 4 bin u.lz a> base ! ;

head    ".fcc", fpmo
dtfcc:  .quad docl0
        .quad fccf,base,qat,toa,four,bin
        .quad udotlz,froma,base,stor,semis

# frndn - Set rounding mode to 00 - round to nearest (default).

head    "frndn", fpmo
frndn:  .quad frndn0
textm
frndn0: fstcw [rip+fcw0]
        btr   word ptr[rip+fcw0], 11
        btr   word ptr[rip+fcw0], 10
        fldcw [rip+fcw0]
        nextm
datam

# frndd - Set rounding mode to 01 - round down.

head    "frndd", fpmo
frndd:  .quad frndd0
textm
frndd0: fstcw [rip+fcw0]
        btr   word ptr[rip+fcw0], 11
        bts   word ptr[rip+fcw0], 10
        fldcw [rip+fcw0]
        nextm
datam

# frndu - Set rounding mode to 10 - round up.

head    "frndu", fpmo
frndu:  .quad frndu0
textm
frndu0: fstcw [rip+fcw0]
        bts   word ptr[rip+fcw0], 11
        btr   word ptr[rip+fcw0], 10
        fldcw [rip+fcw0]
        nextm
datam

# frndz - Set rounding mode to 11 - round toward zero.

head    "frndz", fpmo
frndz:  .quad frndz0
textm
frndz0: fstcw [rip+fcw0]
        bts   word ptr[rip+fcw0], 11
        bts   word ptr[rip+fcw0], 10
        fldcw [rip+fcw0]
        nextm
datam

# frm@ - return number for rounding mode - 0, 1, 2, or 3.
# 0 - Round to nearest
# 1 - Round down
# 2 - Round up
# 3 - Round toward zdero

head    "frm@", fpmo
frmat:  .quad frmat0
textm
frmat0: fstcw [rip+fcw0]
        mov   rax, [rip+fcw0]
        and   rax, 0xC00
        shr   rax, 10
        sub   r14, 8
        mov   [r14], rax
        nextm
datam

# (.frm) - Display the current rounding mode. This is designed to be used
# with `frm@` and does no bounds checking. Therefore it is hidden.

head    "(.frm)", fpmo, hidden
pdtrm:  .quad pdrm0
        .quad pdrm1,pdrm2,pdrm3,pdrm4
       
# Jump table routine for `(.frm)`.

textm
pdrm0:  mov   rdx, [r14]                # Get index
        add   r14, 8                    # Pop stack
        mov   r11, [r11+rdx*8+8]        # Get corresponding vector
        jmp   [r11]
datam

pdrm1:  .quad docl0
        pdtqm "round to nearest (default)"
        .quad semis

pdrm2:  .quad docl0
        pdtqm "round down"
        .quad semis

pdrm3:  .quad docl0
        pdtqm "round up"
        .quad semis

pdrm4:  .quad docl0
        pdtqm "round toward zero"
        .quad semis

# .frm - Display the current rounding mode.

head    ".frm", fpmo
dtfrm:  .quad docl0
        .quad frmat,pdtrm,semis

# sp>f - Push the 32-bit single precision floating-point number contained
# in the 64-bit cell on top of the stack to the floating-point stack (st0).
# Pop the parameter stack.

head    "sp>f", fpmo
sptf:   .quad sptf0
textm
sptf0:  fpldm
        fld   dword ptr[r14]
        add   r14, 8
        nextm
datam

# f>sp - Pop the 32-bit single precision floating-point number from the
# floating-point stack into a 64-bit cell on top of the parameter stack.

head    "f>sp", fpmo
ftsp:   .quad ftsp0
textm
ftsp0:  fpstm
        sub   r14, 8
        fstp  dword ptr[r14]
        nextm
datam

# dp>f - Push the 64-bit double precision floating-point number contained
# in the 64-bit cell on top of the stack to the floating-point stack (st0).
# Pop the parameter stack.

head    "dp>f", fpmo
dptf:   .quad dptf0
textm
dptf0:  fpldm
        fld   qword ptr[r14]
        add   r14, 8
        nextm
datam

# f>dp - Pop the 64-bit double precision floating-point number from the
# floating-point stack into a 64-bit cell on top of the parameter stack.

head    "f>dp", fpmo
ftdp:   .quad ftdp0
textm
ftdp0:  fpstm
        sub   r14, 8
        fstp  qword ptr[r14]
        nextm
datam

# xp>f - Push the 80-bit extended precision floating-point number contained
# in the 128-bit cell on top of the stack to the floating-point stack (st0).

head    "xp>f", fpmo
xptf:   .quad xptf0
textm
xptf0:  fpldm
        fldt  [r14]
        add   r14, 16
        nextm
datam

# f>xp - Pop the 80-bit extended precision floating-point number from the
# floating-point stack into a 128-bit cell on top of the parameter stack.
# For extended precision, the high order stack quad must be cleared
# before the fetch.

head    "f>xp", fpmo
ftxp:   .quad ftxp0
textm
ftxp0:  fpstm
        sub   r14, 16
        mov   qword ptr[r14+8], 0           # Clear high order quad
        fstpt [r14]
        nextm
datam

# sq>f - ( n --- ) ( F: --- r ) Pop the 64-bit integer from the data stack
# and push it on the floating-point stack as a floating-point number.
# Note that a 64-bit integer will always be representable as an IEEE
# Extended floating-point number.

head    "sq>f", fpmo
sqfp:   .quad sqfp0
textm
sqfp0:  fpldm
        fild  qword ptr[r14]
        add   r14, 8
        nextm
datam

# f>sq - ( --- n ) ( F: r --- ) Pop the integer portion of the top
# floating-point stack entry into a 64-bit integer on the data stack.
# Result is always rounded toward zero regardless of the current
# rounding mode setting.
# Note: This operator needs an error check in case the integer cannot
# be represented in a single quad. This incurs an IE exception in the
# FPU.

head    "f>sq", fpmo
fpsq:   .quad fpsq0
textm
fpsq0:  fp1opm
        fpstm
        sub   r14, 8                    # Make room on stack
        fisttp qword ptr[r14]
        nextm
datam

# re>f - Push a real number to the floating-point stack as an 80-bit
# extended precision floating-point number.

# : re>f   re>xp xp>f ;

head    "re>f", fpmo
retfp:  .quad docl0
        .quad retxp,xptf,semis

# f>re - Pop an 80-bit extended precision number from the floating
# point stack and convert it to a real number.

# : f>re   f>xp xp>re ;

head    "f>re", fpmo
fptre:  .quad docl0
        .quad ftxp,xptre,semis

# fl>f - Push a Forthx64 floating-point number to the floating-point stack
# as an 80-bit extended precision floating-point number.

# : fl>f   fl>xp xp>f ;

head    "fl>f", fpmo
fltfp:  .quad docl0
        .quad fltxp,xptf,semis

# f>fl - Pop an 80-bit extended precision number from the floating
# point stack and convert it to a Forthx64 floating-point number.

# : f>fl   f>xp xp>fl ;

head    "f>fl", fpmo
fptfl:  .quad docl0
        .quad ftxp,xptfl,semis

# fdepth - Put the number of FPU entries on the Forthx64 data stack.
# This word satisfies the requirements of the Forth 2012 Standard.

head    "fdepth", fpmo
fdpth:  .quad fdpth0
textm
fdpth0: fpcntm
        sub   r14, 8
        mov   [r14], rbx
        nextm
datam

# f,

head    "f,", cmpo
fcomma: .quad fcomm0
textm
fcomm0: mov   rdi, [rip+dspo0]          # Get dsp
        fpstm
        mov   qword ptr[rdi+8], 0       # Clear high order quad
        fstpt [rdi]                     # Pop value from FPU
        add   qword ptr[rip+dspo0], 16  # Advance dsp
        nextm
datam

# fliteral - compile a floating-point literal from the x87 FPU.

head    "fliteral", -hlfo
flite:  .quad docl0
        .quad pcomp,flit,fcomma,semis
flit:   .quad flit0
textm
flit0:  fpldm
        fldt  [r12]
        add   r12, 16                   # Advance IP past literal
        nextm
datam

# fconstant

head    "fconstant", hlfo
fcons:  .quad docl0
        .quad one,ckfps
        .quad dfine,lit,fpco,objcom,pcomm,fcon0
        .quad fcomma,oszcom,semis
textm
fcon0:  fpldm
        fldt  [r11+8]
        nextm
datam

# fvariable

head    "fvariable", hlfo
fvar:   .quad docl0
        .quad dfine,lit,fvro,objcom,pcomm,vari0
        .quad zero,zero,dcomma,oszcom,semis

# f@ - ( a-addr --- x1 x2 ) Fetch floating-point number
# from the passed address to the floating-point stack.

head    "f@", memo
fat:    .quad fat0
textm
fat0:   mov   rdi, [r14]
        add   r14, 8
        fpldm
        fldt  [rdi]
        nextm
datam

# f! - Pop top floating-point stack entry into the address
# passed on the data stack.

head    "f!", memo
fstor:  .quad fstor0
textm
fstor0: mov   rdi, [r14]
        add   r14, 8
        fpstm
        mov   qword ptr[rdi+8], 0       # Clear high order quad
        fstpt [rdi]                     # Pop value from FPU
        nextm
datam

# fdrop - Discard the top floating-point stack entry. The argument st0
# pops the floating-point stack.

head    "fdrop", fpmo
fdrop:  .quad fdrop0
textm
fdrop0: fpstm
        fstp  st
        nextm
datam

# fdup - Duplicate the top floating-point stack entry.

head    "fdup", fpmo
fdup:   .quad fdup0
textm
fdup0:  fp1opm
        fpldm
        fld   st
        nextm
datam

# fswap - Swap the top two floating-point stack entries.

head    "fswap", fpmo
fswap:  .quad fswap0
textm
fswap0: fp2opm
        fxch
        nextm
datam

# fover - Copy the 2nd floating-point stack entry to the top.
# This operator requires that there be at least two items on
# the stack and room for another.

head    "fover", fpmo
fover:  .quad fover0
textm
fover0: fp2opm
        fpldm
        fld   st(1)
        nextm
datam

# ftuck - Duplicate the 1st entry below the 2nd.

head    "ftuck", fpmo
ftuck:  .quad ftck0
textm
ftck0:  fp2opm
        fxch
        fpldm
        fld   st(1)
        nextm
datam

# frot - ( f1 f2 f3 --- f3 f1 f2 ) Rotate the top three stack entries left.

head    "frot", fpmo
frot:   .quad frot0
textm
frot0:  fp3opm
        fxch  st(1)
        fxch  st(2)
        nextm
datam

# fbrot - ( f1 f2 f3 --- f2 f3 f1 ) Rotate the top three stack entries right.

head    "fbrot", fpmo
fbrot:  .quad fbrot0
textm
fbrot0: fp3opm
        fxch  st(2)
        fxch  st(1)
        nextm
datam

# fnip - Overwrite the 2nd floating-point stack entry with the 1st

head    "fnip", fpmo
fnip:   .quad fnip0
textm
fnip0:  fp2opm
        fstp  st(1)
        dcntm
        nextm
datam

# fxchg - Exchange the top stack entry with the register whose
# number is passed on the data stack (1-7). Uses a jump table.

head    "fxchg", fpmo
fxchg:  .quad fxchg0
textm
fxchg0: mov   rax, [r14]                # Get index
        add   r14, 8                    # Pop stack
        mov   rcx, rax
        and   rax, 7                    # Test for 0
        jz    fxchg9                    # Throw index out of range
        cmp   rax, rcx                  # Text for >7
        jnz   fxchg9                    # Throw index out of range

        fpcntm                          # Get register count
        add   rax, 1                    # Add 1 to index
        cmp   rbx, rax
        jnc   fxchg1

        throwm 132

fxchg1: lea   rdi, [rip+fxchgj]         # Load jump table address
        jmp   [rdi+rax*8-16]

fxchg2: fxch  st(1)
        jmp   fxchg10
fxchg3: fxch  st(2)
        jmp   fxchg10
fxchg4: fxch  st(3)
        jmp   fxchg10
fxchg5: fxch  st(4)
        jmp   fxchg10
fxchg6: fxch  st(5)
        jmp   fxchg10
fxchg7: fxch  st(6)
        jmp   fxchg10
fxchg8: fxch  st(7)
        jmp   fxchg10

fxchg9: throwm 14

fxchg10: nextm
fxchgj: .quad fxchg2,fxchg3,fxchg4,fxchg5,fxchg6,fxchg7,fxchg8
datam

# finfinity - Put IEEE extended precision infinity in st0.
# This is an fconstant.

head    "finfinity", fpmo
finf:   .quad fcon0
        .quad 0x8000000000000000, 0x7FFF

# fzero - Put 0 in st0

head    "fzero", fpmo
fzero:  .quad fzero0
textm
fzero0: fpldm
        fldz
        nextm
datam

# fone - Put 1 in st0

head    "fone", fpmo
fone:   .quad fone0
textm
fone0:  fpldm
        fld1
        nextm
datam

# fpi - Put pi in st0

head    "fpi", fpmo
fpi:    .quad fpi0
textm
fpi0:   fpldm
        fldpi
        nextm
datam

# fe - Put e in st0. Precompiled constant.

head    "fe", fpco
feto:   .quad fcon0
        .quad 12535862302449814171,16384

# fx*2 - Double st0

head    "fx*2", fpmo
fxstt:  .quad fxst0
textm
fxst0:  fp1opm
        fadd  st(0), st(0)
        nextm
datam

# ftrunc - Round top stack number to integer (toward zero).

head    "ftrunc", fpmo
ftrnc:  .quad ftrnc0
textm
ftrnc0: fp1opm
        fintm
        nextm
datam

# ffrac - Return fractional part of top stack number.

head    "ffrac", fpmo
ffrac:  .quad ffrac0
textm
ffrac0: fp1opm
        fpldm
        fld   st(0)
        fintm
        fsubp
        dcntm
        nextm
datam

# fround - Round the value in st0 to the nearest integer.
# Note here that the default rounding mode (round to nearest)
# should always be in effect. Any operator that changes it
# should restore it upon completion.

head    "fround", fpmo
frnd:   .quad frnd0
textm
frnd0:  fp1opm
        frndint
        nextm
datam

# ffloor - The floor function rounds a number down (toward negative infinity)
# to an integer. The previous rounding mode is saved and restored.

head    "ffloor", fpmo
fflor:  .quad fflor0
textm
fflor0: fp1opm
        fstcw [rip+fcw0]
        fstcw [rip+fcw0+4]
        btr   word ptr[rip+fcw0], 11
        bts   word ptr[rip+fcw0], 10
        fldcw [rip+fcw0]
        frndint
        fldcw [rip+fcw0+4]
        nextm
datam

# fceil - The ceiling function rounds a number up (toward infinity)
# to an integer. The previous rounding mode is saved and restored.

head    "fceil", fpmo
fceil:  .quad fceil0
textm
fceil0: fp1opm
        fstcw [rip+fcw0]
        fstcw [rip+fcw0+4]
        bts   word ptr[rip+fcw0], 11
        btr   word ptr[rip+fcw0], 10
        fldcw [rip+fcw0]
        frndint
        fldcw [rip+fcw0+4]
        nextm
datam

# fabs - absolute value of st0

head    "fabs", fpmo
fabso:  .quad fabso0
textm
fabso0: fp1opm
        fabs
        nextm
datam

# fneg - Negate st0

head    "fneg", fpmo
fneg:   .quad fneg0
textm
fneg0:  fp1opm
        fchs
        nextm
datam

# fmin ( F: r1 r2 --- r3 ) r3 is the lesser of r1 and r2.

head    "fmin", fpmo
fmin:   .quad fmin0
textm
fmin0:  fp2opm
        fpstm

        fcom  st(1)                     # Compare top 2 entries

        fstsw ax                        # Get status word

        bt    ax, 8                     # Test for st0 < source
        jnc   fmin1                     # Branch if not less

        bt    ax, 14                    # Test for unordered ops
        jc    fmin1                     # Branch if unordered

# Overwrite 2nd floating-point stack entry with 1st.

        fstp  st(1)
        jmp   fmin2

# Drop top floating-point entry

fmin1:  fstp  st(0)
fmin2:  nextm
datam

# fmax ( F: r1 r2 --- r3 ) r3 is the greater of r1 and r2.

head    "fmax", fpmo
fmax:   .quad fmax0
textm
fmax0:  fp2opm
        fpstm

        fcom  st(1)                     # Compare top 2 entries

        fstsw ax                        # Get status word

        bt    ax, 8                     # Test for st0 < source
        jnc   fmax1                     # Branch if not less

        bt    ax, 14                    # Test for unordered ops
        jc    fmax1                     # Branch if unordered

# Drop top floating-point entry

        fstp  st(0)
        jmp   fmax2

# Overwrite 2nd floating-point stack entry with 1st.

fmax1:  fstp  st(1)
fmax2:  nextm
datam

# f+ - Add the top two floating-point stack values.

head    "f+", fpmo
fplus:  .quad fplus0
textm
fplus0: fp2opm
        dcntm
        faddp st(1)
        nextm
datam

# f- - Subtract the top entry from the 2nd.

head    "f-", fpmo
fmnus:  .quad fmnus0
textm
fmnus0: fp2opm
        dcntm
        fsubp
        nextm
datam

# f* - Multiply the top two stack entries.

head    "f*", fpmo
fmult:  .quad fmult0
textm
fmult0: fp2opm
        dcntm
        fmulp
        nextm
datam

# f/ - Divide the 2n stack entry by the top entry.

head    "f/", fpmo
fdivi:  .quad divi0
textm
divi0:  fp2opm
        dcntm
        fdivp
        fwait
        nextm
datam

# 1/f - Compute the inverse of top stack value.

head    "1/f", fpmo
finv:   .quad finv0
textm
finv0:  fp1opm
        fpldm
        fld1
        fxch
        fdivp
        dcntm                           # Decrement fpcnt
        fwait
        nextm
datam

# fsin

head    "fsin", fpmo
fsine:  .quad fsine0
textm
fsine0: fp1opm
        fsin
        nextm
datam

# fcos

head    "fcos", fpmo
fcsne:  .quad fcsne0
textm
fcsne0: fp1opm
        fcos
        nextm
datam

# fsincos

head    "fsincos", fpmo
fsncs:  .quad fsncs0
textm
fsncs0: fp1opm
        fpldm
        fsincos
        nextm
datam

# ftan - Divides sin by cos to obtain tan.

head    "ftan", fpmo
ftang:  .quad ftang0
textm
ftang0: fp1opm
        fpldm
        fsincos
        fdivp
        dcntm                           # Decrement fpcnt
        fwait
        nextm
datam

# fasin

head    "fasin", fpmo
fasin:  .quad fasin0
textm
fasin0: fp1opm
        fmul  st(0)
        fpldm
        fld1
        fsub  st(0), st(1)
        fdivp
        dcntm
        fwait
        fsqrt
        fwait
        fpldm
        fld1
        fpatan
        dcntm
        nextm
datam

# facos - Arccosine of x from acos(x) = atan(sqrt((1-x*x)/(x*x)))

head    "facos", fpmo
facos:  .quad facos0
textm
facos0: fp1opm
        fmul  st(0)
        fpldm
        fld1
        fxch
        fsub  st(1), st(0)
        fdivp
        dcntm
        fwait
        fsqrt
        fwait
        fpldm
        fld1
        fpatan
        dcntm
        nextm
datam

# fatan - Arctangent function

head    "fatan", fpmo
fatn:   .quad fatn0
textm
fatn0:  fp1opm                          # Make sure there is an argument
        fpldm
        fld1
        fpatan
        dcntm                           # Decrement fpcnt
        nextm
datam

# fx**2 - Square x.

head    "fx**2", fpmo
fxsq:   .quad fxsq0
textm
fxsq0:  fp1opm
        fmul  st(0)
        nextm
datam

# f2**x - Uses the identitiy 2**x = 2**int(x) * 2**frac(x).

head    "f2**x", fpmo
fttx:   .quad fttx0
textm
fttx0:  fp1opm
        ftwttxm
        nextm
datam

# f10**x - Uses the identitiy 10**x = 2**(x * lg(10)).

head    "f10**x", fpmo
ftntx:  .quad ftntx0
textm
ftntx0: fp1opm
        fpldm
        fldl2t
        fmulp
        dcntm
        ftwttxm
        nextm
datam

# falog - Forth 2012 Standard synonym for `f10**x`.
# Uses the identitiy 10**x = 2**(x * lg(10)).

head    "falog", fpmo
falog:  .quad falg0
textm
falg0:  fp1opm
        fpldm
        fldl2t
        fmulp
        dcntm
        ftwttxm
        nextm
datam

# fe**x - Uses the identitiy e**x = 2**(x * lg(e)).

head    "fe**x", fpmo
fettx:  .quad fetx0
textm
fetx0:  fp1opm
        fpldm
        fldl2e
        fmulp
        dcntm
        ftwttxm
        nextm
datam

# fexp - Forth 2012 Standard synonym for `fe**x`.
# Uses the identitiy e**x = 2**(x * lg(e)).

head    "fexp", fpmo
fexp:   .quad fexp0
textm
fexp0:  fp1opm
        fpldm
        fldl2e
        fmulp
        dcntm
        ftwttxm
        nextm
datam

# fexpm1 - Raise e to the power of x and subtract 1.
# Uses the identitiy e**x = 2**(x * lg(e)).

head    "fexpm1", fpmo
fexmo:  .quad fexmo0
textm
fexmo0: fp1opm
        fpldm
        fldl2e
        fmulp
        dcntm
        ftwttxm
        fpldm
        fld1
        dcntm
        fsubp
        nextm
datam

# fy**x - ( x y --- x^y )

head    "fy**x", fpmo
fyttx:  .quad fyttx0
textm
fyttx0: fp2opm
        fxch
        fyl2x

# Note here how the pop of fyl2x is immediately compensated by the load,
# so fpcnt doesn't have to be changed.

        fld   st(0)
        fintm
        fpldm
        fld1
        fscale
        fxch  st(2)
        fsubrp
        dcntm                           # Decrement fpcnt
        f2xm1
        fpldm
        fld1
        faddp
        dcntm
        fmulp
        dcntm
        nextm
datam

# f** - ( x y --- x^y ) Forth 2012 Standard synonym for `fy**x`.

head    "f**", fpmo
fstst:  .quad docl0
        .quad fyttx,semis

# fsqrt - square root via the FPU. ( ieeex --- ieeex ).

head    "fsqrt", fpmo
fsqt:   .quad fsqt0
textm
fsqt0:  fp1opm
        fsqrt
        fwait
        nextm
datam

# fcbrt - cube root via the FPU. ( ieeex --- ieeex ).

head    "fcbrt", fpmo
fcbrt:  .quad docl0
        .quad fone,fone,fplus,fone
        .quad fplus,finv,fstst,semis

# flog - Base 10 logarithm of st0
# log(x) = lg(x)/lg(10), x>0
# Compute 1*lg(x).
# Load lg(10).
# Compute lg(x)/lg(10).

head    "flog", fpmo
flog:   .quad flog0
textm
flog0:  fp1opm
        fpldm
        fld1
        fxch
        fyl2x
        dcntm                           # Decrement fpcnt
        fpldm
        fldl2t
        fdivp
        dcntm                           # Decrement fpcnt
        fwait
        nextm
datam

# fln - Base e logarithm of st0
# ln(x) = lg(x)/lg(e), x>0
# Compute 1*lg(x).
# Load lg(e).
# Compute lg(x)/lg(10).

head    "fln", fpmo
fln:    .quad fln0
textm
fln0:   fp1opm
        flnm
        nextm
datam

# fln1p - Base e logarithm of st0+1
# ln(x) = lg(x)/lg(e), x>0
# Compute 1*lg(x).
# Load lg(e).
# Compute lg(x)/lg(10).

head    "fln1p", fpmo
flnpo:  .quad flnpo0
textm
flnpo0: fp1opm
        fln1pm
        nextm
datam

# fsinh - Hyperbolic sine of x. From (e**x - e**-x)/2

head    "fsinh", fpmo
fsinh:  .quad fsinh0
textm
fsinh0: fp1opm                          # Test for operand
        fpldm                           # Prepare to duplicate st0
        fld   st(0)                     # Dup st0
        fpldm                           # Prepare to load constant
        fldl2e                          # Load log2(e)
        fmulp
        dcntm                           # Increment stack count
        ftwttxm                         # Calculate e**x

        fxch                            # Get x
        fchs                            # Negate it
        
        fpldm                           # Prepare to load constant
        fldl2e                          # Load log2(e)
        fmulp
        dcntm
        ftwttxm                         # Calculate e**-x
        
        dcntm                           # Prepare to subtract and pop
        fsubp
        fpldm                           # Prepare to load constant
        fld1                            # Load 1
        fadd  st(0), st(0)              # Double it
        dcntm                           # Prepare to divide and pop
        fdivp
        fwait
        nextm
datam

# fcosh - Hyperbolic cosine of x. From (e**x + e**-x)/2

head    "fcosh", fpmo
fcosh:  .quad fcosh0
textm
fcosh0: fp1opm                          # Test for operand
        fpldm                           # Prepare to duplicate st0
        fld   st(0)                     # Dup st0
        fpldm                           # Prepare to load constant
        fldl2e                          # Load log2(e)
        fmulp
        dcntm                           # Increment stack count
        ftwttxm                         # Calculate e**x

        fxch                            # Get x
        fchs                            # Negate it
        
        fpldm                           # Prepare to load constant
        fldl2e                          # Load log2(e)
        fmulp
        dcntm
        ftwttxm                         # Calculate e**-x

        dcntm                           # Prepare to add and pop
        faddp st(1)
        fpldm                           # Prepare to load constant
        fld1                            # Load 1
        fadd  st(0), st(0)              # Double it
        dcntm                           # Prepare to divide and pop
        fdivp
        fwait
        nextm
datam

# ftanh - Hyperbolic tangent of x. (e**x - e**-x)/(e**x + e**-x)

head    "ftanh", fpmo
ftanh:  .quad ftanh0
textm
ftanh0: fp1opm                          # Test for operand
        fpldm                           # Prepare to duplicate st0
        fld   st(0)                     # Dup st0
        fpldm                           # Prepare to load constant
        fldl2e                          # Load log2(e)
        fmulp
        dcntm                           # Increment stack count
        ftwttxm                         # Calculate e**x

        fxch                            # Get x
        fchs                            # Negate it
        
        fpldm                           # Prepare to load constant
        fldl2e                          # Load log2(e)
        fmulp
        dcntm
        ftwttxm                         # Calculate e**-x
        
        fpldm                           # fover
        fld   st(1)
        fpldm                           # fover
        fld   st(1)
        dcntm                           # Prepare to subtract and pop
        fsubp
        fxch  st(2)                     # fbrot
        fxch  st(1)
        dcntm                           # Prepare to add and pop
        faddp st(1)
        dcntm                           # Prepare to divide and pop
        fdivp
        fwait
        nextm
datam

# fasinh - Inverse hyperbolic sine of x. From asinh(x) = ln(x+sqrt(x**2+1))

head    "fasinh", fpmo
fasinh: .quad fash0
textm
fash0:  fp1opm                          # Test for operand
        fpldm                           # Prepare to duplicate st0
        fld   st(0)                     # Dup st0
        fmul  st(0)                     # st0**2
        fpldm
        fld1                            # Load 1
        dcntm                           # Prepare to add and pop
        faddp st(1)                     # Add 1
        fsqrt
        dcntm                           # Prepare to add and pop
        faddp st(1)                     # Add x

        flnm                            # Find ln st0

        nextm
datam

# facosh - Inverse hyperbolic cosine of x. From acosh(x) = ln(x+sqrt(x**2-1))

head    "facosh", fpmo
facosh: .quad fach0
textm
fach0:  fp1opm                          # Test for operand
        fpldm                           # Prepare to duplicate st0
        fld   st(0)                     # Dup st0
        fmul  st(0)                     # st0**2
        fpldm
        fld1                            # Load 1
        dcntm                           # Prepare to subtract and pop
        fsubp                           # Subtract 1
        fsqrt
        dcntm                           # Prepare to add and pop
        faddp st(1)                     # Add x

        flnm                            # Find ln st0

        nextm
datam

# fatanh - Inverse hyperbolic tangent of x. From fatanh(x) = ln((1+x)/(1-x))/2 

head    "fatanh", fpmo
fatanh: .quad fatnh0
textm
fatnh0: fp1opm                          # Test for operand
        fpldm                           # Prepare to duplicate st0
        fld   st(0)                     # Dup st0
        fpldm                           # Prepare to load constant
        fld1                            # Load 1
        fadd  st(1), st(0)              # Add 1
        fxch  st(1)                     # frot
        fxch  st(2)
        dcntm                           # Prepare to subtract and pop
        fsubp                           # Subtract x from 1
        dcntm                           # Prepare to divide and pop
        fdivp
        fwait

        flnm                            # Find ln st0

        fpldm                           # Prepare to load constant
        fld1                            # Load 1
        fadd  st(0), st(0)              # Double it
        dcntm                           # Prepare to divide and pop
        fdivp
        fwait
        nextm
datam

# f0= ( --- flag ): ( F: r --- ) Flag is true if and only if
# r is equal to zero.

head    "f0=", fpmo
fzeq:   .quad fzeq0
textm
fzeq0:  fp1opm
        ftst
        fstsw ax
        fpstm
        fstp  st(0)

        sub   r14, 8

        bt    ax, 8
        jc    fzeq2

        bt    ax, 14
        jnc   fzeq2

fzeq1:  mov   qword ptr[r14], -1
        jmp   fzeq3

fzeq2:  mov   qword ptr[r14], 0
fzeq3:  nextm
datam

# f0< ( --- flag ): ( F: r --- ) Flag is true if and only if
# r is less than zero.

head    "f0<", fpmo
fzles:  .quad fzles0
textm
fzles0: fp1opm
        ftst
        fstsw ax
        fpstm
        fstp  st(0)

        sub   r14, 8

        bt    ax, 8
        jnc   fzles2

        bt    ax, 14
        jc    fzles2

fzles1: mov   qword ptr[r14], -1
        jmp   fzles3

fzles2: mov   qword ptr[r14], 0
fzles3: nextm
datam

# f< ( --- flag ): ( F: r1 r2 --- ) Flag is true if and only if
# r1 is less than r2.

head    "f<", fpmo
fless:  .quad fless0
textm
fless0: fp2opm

# Prepare to pop two floating-point stack entries

        sub   byte ptr[rip+flgs0+4], 2

        fcompp                          # Compare and pop 2 entries

        fstsw ax                        # Get status word

        sub   r14, 8                    # Make room for flag on stack

        bt    ax, 8
        jnc   fless2

        bt    ax, 14
        jc    fless2

fless1: mov   qword ptr[r14], -1
        jmp   fless3

fless2: mov   qword ptr[r14], 0
fless3: nextm
datam

# f. - Pop the top value from the floating-point stack and display
# it as a real number.

# : f.   f>re .re ;

head    "f.", fpmo
fdot:   .quad docl0
        .quad fptre,dotre,semis

# Seed values for `sqrtre` and `cbrtre` functions

# srFPUseed - Obtain seed for `sqrtre` from x87.

head    "srFPUseed"
srFPsd: .quad docl0
        .quad retfp,fsqt,fptre,semis

# crFPUseed - Obtain seed for `cbrtre` from x87.

head    "crFPUseed"
crFPsd: .quad docl0
        .quad retfp,fcbrt,fptre,semis


# rcpFPUseed - Obtain seed for `rcpre` from x87.

head    "rcpFPUseed"
rcFPsd: .quad docl0
        .quad retfp,finv,fptre,semis
