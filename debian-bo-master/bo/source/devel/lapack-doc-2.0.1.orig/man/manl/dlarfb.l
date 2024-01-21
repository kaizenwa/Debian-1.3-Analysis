.TH DLARFB l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH DLARFB l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DLARFB - applie a real block reflector H or its transpose H' to a real m by n matrix C, from either the left or the right
.SH SYNOPSIS
.TP 19
SUBROUTINE DLARFB(
SIDE, TRANS, DIRECT, STOREV, M, N, K, V, LDV,
T, LDT, C, LDC, WORK, LDWORK )
.TP 19
.ti +4
CHARACTER
DIRECT, SIDE, STOREV, TRANS
.TP 19
.ti +4
INTEGER
K, LDC, LDT, LDV, LDWORK, M, N
.TP 19
.ti +4
DOUBLE
PRECISION C( LDC, * ), T( LDT, * ), V( LDV, * ),
WORK( LDWORK, * )
.SH PURPOSE
DLARFB applies a real block reflector H or its transpose H' to a
real m by n matrix C, from either the left or the right.

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= 'L': apply H or H' from the Left
.br
= 'R': apply H or H' from the Right
.TP 8
TRANS   (input) CHARACTER*1
.br
= 'N': apply H (No transpose)
.br
= 'T': apply H' (Transpose)
.TP 8
DIRECT  (input) CHARACTER*1
Indicates how H is formed from a product of elementary
reflectors
= 'F': H = H(1) H(2) . . . H(k) (Forward)
.br
= 'B': H = H(k) . . . H(2) H(1) (Backward)
.TP 8
STOREV  (input) CHARACTER*1
Indicates how the vectors which define the elementary
reflectors are stored:
.br
= 'C': Columnwise
.br
= 'R': Rowwise
.TP 8
M       (input) INTEGER
The number of rows of the matrix C.
.TP 8
N       (input) INTEGER
The number of columns of the matrix C.
.TP 8
K       (input) INTEGER
The order of the matrix T (= the number of elementary
reflectors whose product defines the block reflector).
.TP 8
V       (input) DOUBLE PRECISION array, dimension
(LDV,K) if STOREV = 'C'
(LDV,M) if STOREV = 'R' and SIDE = 'L'
(LDV,N) if STOREV = 'R' and SIDE = 'R'
The matrix V. See further details.
.TP 8
LDV     (input) INTEGER
The leading dimension of the array V.
If STOREV = 'C' and SIDE = 'L', LDV >= max(1,M);
if STOREV = 'C' and SIDE = 'R', LDV >= max(1,N);
if STOREV = 'R', LDV >= K.
.TP 8
T       (input) DOUBLE PRECISION array, dimension (LDT,K)
The triangular k by k matrix T in the representation of the
block reflector.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T. LDT >= K.
.TP 8
C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
On entry, the m by n matrix C.
On exit, C is overwritten by H*C or H'*C or C*H or C*H'.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDA >= max(1,M).
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (LDWORK,K)
.TP 8
LDWORK  (input) INTEGER
The leading dimension of the array WORK.
If SIDE = 'L', LDWORK >= max(1,N);
if SIDE = 'R', LDWORK >= max(1,M).
