.TH SLARFT l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH SLARFT l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SLARFT - form the triangular factor T of a real block reflector H of order n, which is defined as a product of k elementary reflectors
.SH SYNOPSIS
.TP 19
SUBROUTINE SLARFT(
DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
.TP 19
.ti +4
CHARACTER
DIRECT, STOREV
.TP 19
.ti +4
INTEGER
K, LDT, LDV, N
.TP 19
.ti +4
REAL
T( LDT, * ), TAU( * ), V( LDV, * )
.SH PURPOSE
SLARFT forms the triangular factor T of a real block reflector H
of order n, which is defined as a product of k elementary reflectors.

If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;

If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.

If STOREV = 'C', the vector which defines the elementary reflector
H(i) is stored in the i-th column of the array V, and
.br

   H  =  I - V * T * V'
.br

If STOREV = 'R', the vector which defines the elementary reflector
H(i) is stored in the i-th row of the array V, and
.br

   H  =  I - V' * T * V
.br

.SH ARGUMENTS
.TP 8
DIRECT  (input) CHARACTER*1
Specifies the order in which the elementary reflectors are
multiplied to form the block reflector:
.br
= 'F': H = H(1) H(2) . . . H(k) (Forward)
.br
= 'B': H = H(k) . . . H(2) H(1) (Backward)
.TP 8
STOREV  (input) CHARACTER*1
Specifies how the vectors which define the elementary
reflectors are stored (see also Further Details):
.br
= 'R': rowwise
.TP 8
N       (input) INTEGER
The order of the block reflector H. N >= 0.
.TP 8
K       (input) INTEGER
The order of the triangular factor T (= the number of
elementary reflectors). K >= 1.
.TP 8
V       (input/output) REAL array, dimension
(LDV,K) if STOREV = 'C'
(LDV,N) if STOREV = 'R'
The matrix V. See further details.
.TP 8
LDV     (input) INTEGER
The leading dimension of the array V.
If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.
.TP 8
TAU     (input) REAL array, dimension (K)
TAU(i) must contain the scalar factor of the elementary
reflector H(i).
.TP 8
T       (output) REAL array, dimension (LDT,K)
The k by k triangular factor T of the block reflector.
If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is
lower triangular. The rest of the array is not used.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T. LDT >= K.
.SH FURTHER DETAILS
The shape of the matrix V and the storage of the vectors which define
the H(i) is best illustrated by the following example with n = 5 and
k = 3. The elements equal to 1 are not stored; the corresponding
array elements are modified but restored on exit. The rest of the
array is not used.
.br

DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R':

             V = (  1       )                 V = (  1 v1 v1 v1 v1 )
                 ( v1  1    )                     (     1 v2 v2 v2 )
                 ( v1 v2  1 )                     (        1 v3 v3 )
                 ( v1 v2 v3 )
.br
                 ( v1 v2 v3 )
.br

DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R':

             V = ( v1 v2 v3 )                 V = ( v1 v1  1       )
                 ( v1 v2 v3 )                     ( v2 v2 v2  1    )
                 (  1 v2 v3 )                     ( v3 v3 v3 v3  1 )
                 (     1 v3 )
.br
                 (        1 )
.br

