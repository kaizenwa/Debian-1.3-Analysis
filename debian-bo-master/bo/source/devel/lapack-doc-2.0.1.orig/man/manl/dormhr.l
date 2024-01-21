.TH DORMHR l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH DORMHR l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DORMHR - overwrite the general real M-by-N matrix C with   SIDE = 'L' SIDE = 'R' TRANS = 'N'
.SH SYNOPSIS
.TP 19
SUBROUTINE DORMHR(
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
DOUBLE
PRECISION A( LDA, * ), C( LDC, * ), TAU( * ),
WORK( LWORK )
.SH PURPOSE
DORMHR overwrites the general real M-by-N matrix C with
TRANS = 'T':      Q**T * C       C * Q**T
.br

where Q is a real orthogonal matrix of order nq, with nq = m if
SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
IHI-ILO elementary reflectors, as returned by DGEHRD:
.br

Q = H(ilo) H(ilo+1) . . . H(ihi-1).
.br

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= 'L': apply Q or Q**T from the Left;
.br
= 'R': apply Q or Q**T from the Right.
.TP 8
TRANS   (input) CHARACTER*1
.br
= 'N':  No transpose, apply Q;
.br
= 'T':  Transpose, apply Q**T.
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
of DGEHRD. Q is equal to the unit matrix except in the
submatrix Q(ilo+1:ihi,ilo+1:ihi).
If SIDE = 'L', then 1 <= ILO <= IHI <= M, if M > 0, and
ILO = 1 and IHI = 0, if M = 0;
if SIDE = 'R', then 1 <= ILO <= IHI <= N, if N > 0, and
ILO = 1 and IHI = 0, if N = 0.
.TP 8
A       (input) DOUBLE PRECISION array, dimension
(LDA,M) if SIDE = 'L'
(LDA,N) if SIDE = 'R'
The vectors which define the elementary reflectors, as
returned by DGEHRD.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.
LDA >= max(1,M) if SIDE = 'L'; LDA >= max(1,N) if SIDE = 'R'.
.TP 8
TAU     (input) DOUBLE PRECISION array, dimension
(M-1) if SIDE = 'L'
(N-1) if SIDE = 'R'
TAU(i) must contain the scalar factor of the elementary
reflector H(i), as returned by DGEHRD.
.TP 8
C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
On entry, the M-by-N matrix C.
On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >= max(1,M).
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
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
