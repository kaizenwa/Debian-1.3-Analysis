.TH DOPMTR l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH DOPMTR l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DOPMTR - overwrite the general real M-by-N matrix C with   SIDE = 'L' SIDE = 'R' TRANS = 'N'
.SH SYNOPSIS
.TP 19
SUBROUTINE DOPMTR(
SIDE, UPLO, TRANS, M, N, AP, TAU, C, LDC, WORK,
INFO )
.TP 19
.ti +4
CHARACTER
SIDE, TRANS, UPLO
.TP 19
.ti +4
INTEGER
INFO, LDC, M, N
.TP 19
.ti +4
DOUBLE
PRECISION AP( * ), C( LDC, * ), TAU( * ), WORK( * )
.SH PURPOSE
DOPMTR overwrites the general real M-by-N matrix C with
TRANS = 'T':      Q**T * C       C * Q**T
.br

where Q is a real orthogonal matrix of order nq, with nq = m if
SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
nq-1 elementary reflectors, as returned by DSPTRD using packed
storage:
.br

if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
.br

if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
.br

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= 'L': apply Q or Q**T from the Left;
.br
= 'R': apply Q or Q**T from the Right.
.TP 8
UPLO    (input) CHARACTER*1
.br
= 'U': Upper triangular packed storage used in previous
call to DSPTRD;
= 'L': Lower triangular packed storage used in previous
call to DSPTRD.
.TP 8
TRANS   (input) CHARACTER*1
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
AP      (input) DOUBLE PRECISION array, dimension
(M*(M+1)/2) if SIDE = 'L'
(N*(N+1)/2) if SIDE = 'R'
The vectors which define the elementary reflectors, as
returned by DSPTRD.  AP is modified by the routine but
restored on exit.
.TP 8
TAU     (input) DOUBLE PRECISION array, dimension (M-1) if SIDE = 'L'
or (N-1) if SIDE = 'R'
TAU(i) must contain the scalar factor of the elementary
reflector H(i), as returned by DSPTRD.
.TP 8
C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
On entry, the M-by-N matrix C.
On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >= max(1,M).
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension
(N) if SIDE = 'L'
(M) if SIDE = 'R'
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
