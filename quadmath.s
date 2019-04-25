# Quad-precision Math Module for Forthx64
#
# (c) Copyright 2018 by John F. Healy. All rights reserved.
#
# quadmath.s is distributed under the terms of the 2-clause BSD License.
# Copyright (c) 2013-2018, John F. Healy <healyjohnf@gmail.com>
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
#
# C Quad-Precision Math Library Functions
#
# Function names end with "q" to indicate that they accept
# and return quad-precision floating-point operands.
# These operands occupy two quads on the stack.
# Note that error processing is not yet included with these
# functions.
#
# This module is included by default in the Forthx64 source file in the section
# titled "Include external modules here". To exclude it, simply comment out
# the line:
#
#   .include "quadmath.s"
#
# If quadmath.s is included, add -lquadmath to the compile command line to link
# the library.

# C Quad-precision Math Library Functions
#
# For the GNU assembler it is not necessary to declare these
# functions in the source file. The references are automatically
# resolved by gcc and ld. They are listed here for reference.

#    __addtf3, __subtf3, __multf3, __divtf3
#    truncq, roundq, fabsq, ceilq, floorq, fminq, fmaxq
#    sqrtq,cbrtq, powq, hypotq, fmodq, modfq
#    sinq, cosq, sincosq, tanq, asinq, acosq, atanq, atan2q
#    sinhq, coshq, tanhq, asinhq, acoshq, atanhq
#    expq, expm1q, logq, log2q, log10q
#    log1pq

# Macro for Quad Math Function Calls - single operand

.macro  QMFm    op
        movdqu  xmm0, [r14]
        sub     rsp, 8

        call    \op

        add     rsp, 8
        movdqu  [r14], xmm0
.endm

# Macro for Quad Math Function Calls - two operands

.macro  QMF2m   op
        movdqu  xmm0, [r14+16]
        movdqu  xmm1, [r14]
        add     r14, 16
        sub     rsp, 8

        call    \op

        add     rsp, 8
        movdqu  [r14], xmm0
.endm

# Precompiled Constants

# zeroq - Put quad-precision zero on stack.

head    "zeroq", qpco
zroq:   .quad dcon0
        .quad 0,0

# oneq - Put quad-precision one on stack.

head    "oneq", qpco
oneq:   .quad dcon0
        .quad 0,4611404543450677248

# piq - Put quad-precision pi on stack.

head    "piq", qpco
piqp:   .quad dcon0
        .quad 9541308523256152504,4611846683310179025

# eq - Put quad-precision e on stack.

head    "eq", qpco
etoq:   .quad dcon0
        .quad 10751604932185443962,4611787107607856502

# Arithmetic Operators

# x*2q ( x --- 2*x )

head    "x*2q", qpmo
xttq:   .quad xttq0
textm
xttq0:  movdqu  xmm0, [r14]
        movdqu  xmm1, xmm0
        sub     rsp, 8

        call    __addtf3

        add     rsp, 8
        movdqu  [r14], xmm0
        nextm
datam

# truncq ( x --- Chop to integer )

head    "truncq", qpmo
trncq:  .quad trncq0
textm
trncq0: QMFm  truncq
        nextm
datam

# fracq ( x --- Fractional part of x )

head    "fracq", qpmo
fracq:  .quad fracq0
textm
fracq0: movdqu  xmm0, [r14]
        mov   rdi, r14
        sub   rsp, 8

        call  modfq

        add   rsp, 8
        movdqu  [r14], xmm0
        nextm
datam

# roundq ( x --- nearest integer to x )

head    "roundq", qpmo
rondq:  .quad rondq0
textm
rondq0: QMFm  roundq
        nextm
datam

# floorq ( x --- floor x )

head    "floorq", qpmo
florq:  .quad florq0
textm
florq0: QMFm  floorq
        nextm
datam

# ceilq ( x --- ceil x )

head    "ceilq", qpmo
celq:   .quad celq0
textm
celq0:  QMFm  ceilq
        nextm
datam

# absq ( x --- abs x )

head    "absq", qpmo
absq:   .quad absq0
textm
absq0:  QMFm  fabsq
        nextm
datam

# negq ( x --- -x )

head    "negq", qpmo
negq:   .quad negq0
textm
negq0:  btc   qword ptr[r14+8], 63
        nextm
datam

# minq ( x y  --- min )

head    "minq", qpmo
minq:   .quad minq0
textm
minq0:  QMF2m fminq
        nextm
datam

# maxq ( x y  --- max )

head    "maxq", qpmo
maxq:   .quad maxq0
textm
maxq0:  QMF2m fmaxq
        nextm
datam

# +q ( x y  --- x+y )

head    "+q", qpmo
plusq:  .quad plusq0
textm
plusq0: QMF2m __addtf3
        nextm
datam

# -q ( x y  --- x-y )

head    "-q", qpmo
mnusq:  .quad mnusq0
textm
mnusq0: QMF2m __subtf3
        nextm
datam

# *q ( x y  --- x*y )

head    "*q", qpmo
starq:  .quad starq0
textm
starq0: QMF2m __multf3
        nextm
datam

# /q ( x y  --- x/y )

head    "/q", qpmo
slshq:  .quad slshq0
textm
slshq0: QMF2m __divtf3
        nextm
datam

# modq ( x y --- x mod y )

head    "modq", qpmo
modq:   .quad modq0
textm
modq0:  QMF2m fmodq
        nextm
datam

# 1/q ( x --- 1/x )

head    "1/q", qpmo
invsq:  .quad invsq0
textm
invsq0: movdqu  xmm0, [rip+num0]
        movdqu  xmm1, [r14]
        sub     rsp, 8

        call    __divtf3

        add     rsp, 8
        movdqu  [r14], xmm0
        nextm
num0:   .quad 0,4611404543450677248
datam

# Transcendental Functions

# sinq ( x --- sin x )

head    "sinq", qpmo
sineq:  .quad sineq0
textm
sineq0: QMFm  sinq
        nextm
datam

# cosq ( x --- cos x )

head    "cosq", qpmo
cosnq:  .quad cosn0
textm
cosn0:  QMFm  cosq
        nextm
datam

# sincosq ( x --- cos[x] sin[x] )

head    "sincosq", qpmo
sncsq:  .quad sncsq0
textm
sncsq0: movdqu  xmm0, [r14]
        mov   rsi, r14
        sub   r14, 16
        mov   rdi, r14
        sub   rsp, 8

        call  sincosq

        add   rsp, 8
        nextm
datam

# hypotq ( a b --- sqrt[a^2 + b^2] )

head    "hypotq", qpmo
hyptq:  .quad hyptq0
textm
hyptq0: QMF2m hypotq
        nextm
datam

# tanq ( x --- tan x )

head    "tanq", qpmo
tangq:  .quad tingq0
textm
tingq0: QMFm  tanq
        nextm
datam

# asinq ( x --- asin x )

head    "asinq", qpmo
asnq:   .quad asnq0
textm
asnq0:  QMFm  asinq
        nextm
datam

# acosq ( x --- acos x )

head    "acosq", qpmo
acsnq:  .quad acsnq0
textm
acsnq0: QMFm  acosq
        nextm
datam

# atanq ( x --- atan x )

head    "atanq", qpmo
atngq:  .quad atngq0
textm
atngq0: QMFm  atanq
        nextm
datam

# atan2q ( x --- atan2 x )

head    "atan2q", qpmo
atntq:  .quad atntq0
textm
atntq0: QMFm  atan2q
        nextm
datam

# x**2q ( x --- x*x )

head    "x**2q", qpmo
xsqq:   .quad xsqq0
textm
xsqq0:  movdqu  xmm0, [r14]
        movdqu  xmm1, xmm0
        sub     rsp, 8

        call    __multf3

        add     rsp, 8
        movdqu  [r14], xmm0
        nextm
datam

# expq ( x --- exp x )

head    "expq", qpmo
expnq:  .quad expnq0
textm
expnq0: QMFm  expq
        nextm
datam

# expm1q ( x --- exp x - 1 )

head    "expm1q", qpmo
expmoq: .quad xpmoq0
textm
xpmoq0: QMFm  expm1q
        nextm
datam

# y**xq ( y x --- y^x )

head    "y**xq", qpmo
yttxq:  .quad yttxq0
textm
yttxq0: QMF2m powq
        nextm
datam

# sqrtq ( x --- x^1/2 )

head    "sqrtq", qpmo
sqtq:   .quad sqtq0
textm
sqtq0:  QMFm  sqrtq
        nextm
datam

# cbrtq ( x --- x^1/3 )

head    "cbrtq", qpmo
cbtq:   .quad cbtq0
textm
cbtq0:  QMFm  cbrtq
        nextm
datam

# log2q ( x --- log2 x )

head    "log2q", qpmo
logtq:  .quad logtq0
textm
logtq0: QMFm  log2q
        nextm
datam

# logq ( x --- log10 x )

head    "logq", qpmo
lgtnq:  .quad lgtnq0
textm
lgtnq0: QMFm  log10q
        nextm
datam

# lnq ( x --- loge x )

head    "lnq", qpmo
lnq:    .quad lnq0
textm
lnq0:   QMFm  logq
        nextm
datam

# ln1pq ( x --- 1+loge x )

head    "ln1pq", qpmo
lnpoq:  .quad lnpoq0
textm
lnpoq0: QMFm  log1pq
        nextm
datam

# sinhq ( x --- sinh x )

head    "sinhq", qpmo
snhq:   .quad snhq0
textm
snhq0:  QMFm  sinhq
        nextm
datam

# coshq ( x --- cosh x )

head    "coshq", qpmo
cshq:   .quad cshq0
textm
cshq0: QMFm  coshq
        nextm
datam

# tanhq ( x --- tanh x )

head    "tanhq", qpmo
tnhq:   .quad tnhq0
textm
tnhq0: QMFm  tanhq
        nextm
datam

# asinhq ( x --- asinh x )

head    "asinhq", qpmo
asnhq:  .quad asnhq0
textm
asnhq0: QMFm  asinhq
        nextm
datam

# acoshq ( x --- acosh x )

head    "acoshq", qpmo
acshq:  .quad acshq0
textm
acshq0: QMFm  acoshq
        nextm
datam

# atanhq ( x --- atanh x )

head    "atanhq", qpmo
atnhq:  .quad atnhq0
textm
atnhq0: QMFm  atanhq
        nextm
datam

# Seed values for `sqrtre` and `cbrtre` functions

# srqseed - Obtain seed for `sqrtre` from quadmath functions.

head    "srqseed"
srqsd:  .quad docl0
        .quad retqp,sqtq,qptre,semis

# crqseed - Obtain seed for `cbrtre` from quadmath functions.

head    "crqseed"
crqsd:  .quad docl0
        .quad retqp,cbtq,qptre,semis

# rcpqseed - Obtain seed for `rcpre` from quadmath functions.

head    "rcpqseed"
rcpqsd: .quad docl0
        .quad retqp,invsq,qptre,semis
