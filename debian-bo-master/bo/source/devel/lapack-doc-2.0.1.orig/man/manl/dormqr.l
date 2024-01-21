.TH DORMQR l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH DORMQR l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DORMQR - overwrite the general real M-by-N matrix C with   SIDE = 'L' SIDE = 'R' TRANS = 'N'
.SH SYNOPSIS
.TP 19
SUBROUTINE DORMQR(
SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
WORK, LWORK, INFO )
.TP 19
.ti +4
CHARACTER
SIDE, TRANS
.TP 19
.ti +4
INTEGER
INFO, K, LDA, LDC, LWORK, M, N
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), C( LDC, * ), TAU( * ),
WORK( LWORK )
.SH PURPOSE
DORMQR overwrites the general real M-by-N matrix C with
TRANS = 'T':      Q**T * C       C * Q**T
.br

where Q is a real orthogonal matrix defined as the product of k
elementary reflectors
.br

      Q = H(1) H(2) . . . H(k)
.br

as returned by DGEQRF. Q is of order M if SIDE = 'L' and of order N
if SIDE = 'R'.
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
K       (input) INTEGER
The number of elementary reflectors whose product defines
the matrix Q.
If SIDE = 'L', M >= K >= 0;
if SIDE = 'R', N >= K >= 0.
.TP 8
A       (input) DOUBLE PRECISION array, dimension (LDA,K)
The i-th column must contain the vector which defines the
elementary reflector H(i), for i = 1,2,...,k, as returned by
DGEQRF in the first k columns of its array argument A.
A is modified by the routine but restored on exit.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.
If SIDE = 'L', LDA >= max(1,M);
if SIDE = 'R', LDA >= max(1,N).
.TP 8
TAU     (input) DOUBLE PRECISION array, dimension (K)
TAU(i) must contain the scalar factor of the elementary
reflector H(i), as returned by DGEQRF.
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
