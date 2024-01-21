.TH ZUNMHR l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH ZUNMHR l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
ZUNMHR - overwrite the general complex M-by-N matrix C with   SIDE = 'L' SIDE = 'R' TRANS = 'N'
.SH SYNOPSIS
.TP 19
SUBROUTINE ZUNMHR(
SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C,
LDC, WORK, LWORK, INFO )
.TP 19
.ti +4
CHARACTER
SIDE, TRANS
.TP 19
.ti +4
INTEGER
IHI, ILO, INFO, LDA, LDC, LWORK, M, N
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), C( LDC, * ), TAU( * ),
WORK( LWORK )
.SH PURPOSE
ZUNMHR overwrites the general complex M-by-N matrix C with
TRANS = 'C':      Q**H * C       C * Q**H
.br

where Q is a complex unitary matrix of order nq, with nq = m if
SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
IHI-ILO elementary reflectors, as returned by ZGEHRD:
.br

Q = H(ilo) H(ilo+1) . . . H(ihi-1).
.br

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= 'L': apply Q or Q**H from the Left;
.br
= 'R': apply Q or Q**H from the Right.
.TP 8
TRANS   (input) CHARACTER*1
.br
= 'N': apply Q  (No transpose)
.br
= 'C': apply Q**H (Conjugate transpose)
.TP 8
M       (input) INTEGER
The number of rows of the matrix C. M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix C. N >= 0.
.TP 8
ILO     (input) INTEGER
IHI     (input) INTEGER
ILO and IHI must have the same values as in the previous call
of ZGEHRD. Q is equal to the unit matrix except in the
submatrix Q(ilo+1:ihi,ilo+1:ihi).
If SIDE = 'L', then 1 <= ILO <= IHI <= M, if M > 0, and
ILO = 1 and IHI = 0, if M = 0;
if SIDE = 'R', then 1 <= ILO <= IHI <= N, if N > 0, and
ILO = 1 and IHI = 0, if N = 0.
.TP 8
A       (input) COMPLEX*16 array, dimension
(LDA,M) if SIDE = 'L'
(LDA,N) if SIDE = 'R'
The vectors which define the elementary reflectors, as
returned by ZGEHRD.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.
LDA >= max(1,M) if SIDE = 'L'; LDA >= max(1,N) if SIDE = 'R'.
.TP 8
TAU     (input) COMPLEX*16 array, dimension
(M-1) if SIDE = 'L'
(N-1) if SIDE = 'R'
TAU(i) must contain the scalar factor of the elementary
reflector H(i), as returned by ZGEHRD.
.TP 8
C       (input/output) COMPLEX*16 array, dimension (LDC,N)
On entry, the M-by-N matrix C.
On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >= max(1,M).
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
If SIDE = 'L', LWORK >= max(1,N);
if SIDE = 'R', LWORK >= max(1,M).
For optimum performance LWORK >= N*NB if SIDE = 'L', and
LWORK >= M*NB if SIDE = 'R', where NB is the optimal
blocksize.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
