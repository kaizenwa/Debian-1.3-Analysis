.TH CGGHRD l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH CGGHRD l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CGGHRD - reduce a pair of complex matrices (A,B) to generalized upper Hessenberg form using unitary transformations, where A is a general matrix and B is upper triangular
.SH SYNOPSIS
.TP 19
SUBROUTINE CGGHRD(
COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB, Q,
LDQ, Z, LDZ, INFO )
.TP 19
.ti +4
CHARACTER
COMPQ, COMPZ
.TP 19
.ti +4
INTEGER
IHI, ILO, INFO, LDA, LDB, LDQ, LDZ, N
.TP 19
.ti +4
COMPLEX
A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
Z( LDZ, * )
.SH PURPOSE
CGGHRD reduces a pair of complex matrices (A,B) to generalized upper
Hessenberg form using unitary transformations, where A is a
general matrix and B is upper triangular:  Q' * A * Z = H and
Q' * B * Z = T, where H is upper Hessenberg, T is upper triangular,
and Q and Z are unitary, and ' means conjugate transpose.

The unitary matrices Q and Z are determined as products of Givens
rotations.  They may either be formed explicitly, or they may be
postmultiplied into input matrices Q1 and Z1, so that
.br

     Q1 * A * Z1' = (Q1*Q) * H * (Z1*Z)'
.br
     Q1 * B * Z1' = (Q1*Q) * T * (Z1*Z)'
.br

.SH ARGUMENTS
.TP 8
COMPQ   (input) CHARACTER*1
= 'N': do not compute Q;
.br
= 'I': Q is initialized to the unit matrix, and the
unitary matrix Q is returned;
= 'V': Q must contain a unitary matrix Q1 on entry,
and the product Q1*Q is returned.
.TP 8
COMPZ   (input) CHARACTER*1
= 'N': do not compute Q;
.br
= 'I': Q is initialized to the unit matrix, and the
unitary matrix Q is returned;
= 'V': Q must contain a unitary matrix Q1 on entry,
and the product Q1*Q is returned.
.TP 8
N       (input) INTEGER
The order of the matrices A and B.  N >= 0.
.TP 8
ILO     (input) INTEGER
IHI     (input) INTEGER
It is assumed that A is already upper triangular in rows and
columns 1:ILO-1 and IHI+1:N.  ILO and IHI are normally set
by a previous call to CGGBAL; otherwise they should be set
to 1 and N respectively.
1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA, N)
On entry, the N-by-N general matrix to be reduced.
On exit, the upper triangle and the first subdiagonal of A
are overwritten with the upper Hessenberg matrix H, and the
rest is set to zero.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
B       (input/output) COMPLEX array, dimension (LDB, N)
On entry, the N-by-N upper triangular matrix B.
On exit, the upper triangular matrix T = Q' B Z.  The
elements below the diagonal are set to zero.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
Q       (input/output) COMPLEX array, dimension (LDQ, N)
If COMPQ='N':  Q is not referenced.
.br
If COMPQ='I':  on entry, Q need not be set, and on exit it
contains the unitary matrix Q, where Q'
is the product of the Givens transformations
which are applied to A and B on the left.
If COMPQ='V':  on entry, Q must contain a unitary matrix
Q1, and on exit this is overwritten by Q1*Q.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.
LDQ >= N if COMPQ='V' or 'I'; LDQ >= 1 otherwise.
.TP 8
Z       (input/output) COMPLEX array, dimension (LDZ, N)
If COMPZ='N':  Z is not referenced.
.br
If COMPZ='I':  on entry, Z need not be set, and on exit it
contains the unitary matrix Z, which is
the product of the Givens transformations
which are applied to A and B on the right.
If COMPZ='V':  on entry, Z must contain a unitary matrix
Z1, and on exit this is overwritten by Z1*Z.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.
LDZ >= N if COMPZ='V' or 'I'; LDZ >= 1 otherwise.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.SH FURTHER DETAILS
This routine reduces A to Hessenberg and B to triangular form by
an unblocked reduction, as described in _Matrix_Computations_,
by Golub and van Loan (Johns Hopkins Press).
.br

