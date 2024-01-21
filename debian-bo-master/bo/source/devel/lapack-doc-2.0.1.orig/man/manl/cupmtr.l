.TH CUPMTR l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH CUPMTR l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CUPMTR - overwrite the general complex M-by-N matrix C with   SIDE = 'L' SIDE = 'R' TRANS = 'N'
.SH SYNOPSIS
.TP 19
SUBROUTINE CUPMTR(
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
COMPLEX
AP( * ), C( LDC, * ), TAU( * ), WORK( * )
.SH PURPOSE
CUPMTR overwrites the general complex M-by-N matrix C with
TRANS = 'C':      Q**H * C       C * Q**H
.br

where Q is a complex unitary matrix of order nq, with nq = m if
SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
nq-1 elementary reflectors, as returned by CHPTRD using packed
storage:
.br

if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
.br

if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
.br

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= 'L': apply Q or Q**H from the Left;
.br
= 'R': apply Q or Q**H from the Right.
.TP 8
UPLO    (input) CHARACTER*1
.br
= 'U': Upper triangular packed storage used in previous
call to CHPTRD;
= 'L': Lower triangular packed storage used in previous
call to CHPTRD.
.TP 8
TRANS   (input) CHARACTER*1
= 'N':  No transpose, apply Q;
.br
= 'C':  Conjugate transpose, apply Q**H.
.TP 8
M       (input) INTEGER
The number of rows of the matrix C. M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix C. N >= 0.
.TP 8
AP      (input) COMPLEX array, dimension
(M*(M+1)/2) if SIDE = 'L'
(N*(N+1)/2) if SIDE = 'R'
The vectors which define the elementary reflectors, as
returned by CHPTRD.  AP is modified by the routine but
restored on exit.
.TP 8
TAU     (input) COMPLEX array, dimension (M-1) if SIDE = 'L'
or (N-1) if SIDE = 'R'
TAU(i) must contain the scalar factor of the elementary
reflector H(i), as returned by CHPTRD.
.TP 8
C       (input/output) COMPLEX array, dimension (LDC,N)
On entry, the M-by-N matrix C.
On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >= max(1,M).
.TP 8
WORK    (workspace) COMPLEX array, dimension
(N) if SIDE = 'L'
(M) if SIDE = 'R'
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
