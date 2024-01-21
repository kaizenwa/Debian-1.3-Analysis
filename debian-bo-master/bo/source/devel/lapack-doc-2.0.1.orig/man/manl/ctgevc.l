.TH CTGEVC l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH CTGEVC l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CTGEVC - compute some or all of the right and/or left generalized eigenvectors of a pair of complex upper triangular matrices (A,B)
.SH SYNOPSIS
.TP 19
SUBROUTINE CTGEVC(
SIDE, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,
LDVL, VR, LDVR, MM, M, WORK, RWORK, INFO )
.TP 19
.ti +4
CHARACTER
HOWMNY, SIDE
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, LDVL, LDVR, M, MM, N
.TP 19
.ti +4
LOGICAL
SELECT( * )
.TP 19
.ti +4
REAL
RWORK( * )
.TP 19
.ti +4
COMPLEX
A( LDA, * ), B( LDB, * ), VL( LDVL, * ),
VR( LDVR, * ), WORK( * )
.SH PURPOSE
CTGEVC computes some or all of the right and/or left generalized
eigenvectors of a pair of complex upper triangular matrices (A,B).

The right generalized eigenvector x and the left generalized
eigenvector y of (A,B) corresponding to a generalized eigenvalue
w are defined by:
.br

        (A - wB) * x = 0  and  y**H * (A - wB) = 0
.br

where y**H denotes the conjugate tranpose of y.
.br

If an eigenvalue w is determined by zero diagonal elements of both A
and B, a unit vector is returned as the corresponding eigenvector.

If all eigenvectors are requested, the routine may either return
the matrices X and/or Y of right or left eigenvectors of (A,B), or
the products Z*X and/or Q*Y, where Z and Q are input unitary
matrices.  If (A,B) was obtained from the generalized Schur
factorization of an original pair of matrices
.br
   (A0,B0) = (Q*A*Z**H,Q*B*Z**H),
.br
then Z*X and Q*Y are the matrices of right or left eigenvectors of
A.
.br

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= 'R': compute right eigenvectors only;
.br
= 'L': compute left eigenvectors only;
.br
= 'B': compute both right and left eigenvectors.
.TP 8
HOWMNY  (input) CHARACTER*1
.br
= 'A': compute all right and/or left eigenvectors;
.br
= 'B': compute all right and/or left eigenvectors, and
backtransform them using the input matrices supplied
in VR and/or VL;
= 'S': compute selected right and/or left eigenvectors,
specified by the logical array SELECT.
.TP 8
SELECT  (input) LOGICAL array, dimension (N)
If HOWMNY='S', SELECT specifies the eigenvectors to be
computed.
If HOWMNY='A' or 'B', SELECT is not referenced.
To select the eigenvector corresponding to the j-th
eigenvalue, SELECT(j) must be set to .TRUE..
.TP 8
N       (input) INTEGER
The order of the matrices A and B.  N >= 0.
.TP 8
A       (input) COMPLEX array, dimension (LDA,N)
The upper triangular matrix A.
.TP 8
LDA     (input) INTEGER
The leading dimension of array A.  LDA >= max(1,N).
.TP 8
B       (input) COMPLEX array, dimension (LDB,N)
The upper triangular matrix B.  B must have real diagonal
elements.
.TP 8
LDB     (input) INTEGER
The leading dimension of array B.  LDB >= max(1,N).
.TP 8
VL      (input/output) COMPLEX array, dimension (LDVL,MM)
On entry, if SIDE = 'L' or 'B' and HOWMNY = 'B', VL must
contain an N-by-N matrix Q (usually the unitary matrix Q
of left Schur vectors returned by CHGEQZ).
On exit, if SIDE = 'L' or 'B', VL contains:
if HOWMNY = 'A', the matrix Y of left eigenvectors of (A,B);
if HOWMNY = 'B', the matrix Q*Y;
if HOWMNY = 'S', the left eigenvectors of (A,B) specified by
SELECT, stored consecutively in the columns of
VL, in the same order as their eigenvalues.
If SIDE = 'R', VL is not referenced.
.TP 8
LDVL    (input) INTEGER
The leading dimension of array VL.
LDVL >= max(1,N) if SIDE = 'L' or 'B'; LDVL >= 1 otherwise.
.TP 8
VR      (input/output) COMPLEX array, dimension (LDVR,MM)
On entry, if SIDE = 'R' or 'B' and HOWMNY = 'B', VR must
contain an N-by-N matrix Q (usually the unitary matrix Z
of right Schur vectors returned by CHGEQZ).
On exit, if SIDE = 'R' or 'B', VR contains:
if HOWMNY = 'A', the matrix X of right eigenvectors of (A,B);
if HOWMNY = 'B', the matrix Z*X;
if HOWMNY = 'S', the right eigenvectors of (A,B) specified by
SELECT, stored consecutively in the columns of
VR, in the same order as their eigenvalues.
If SIDE = 'L', VR is not referenced.
.TP 8
LDVR    (input) INTEGER
The leading dimension of the array VR.
LDVR >= max(1,N) if SIDE = 'R' or 'B'; LDVR >= 1 otherwise.
.TP 8
MM      (input) INTEGER
The leading dimension of the array VR.
LDVR >= max(1,N) if SIDE = 'R' or 'B'; LDVR >= 1 otherwise.
.TP 8
MM      (input) INTEGER
The number of columns in the arrays VL and/or VR. MM >= M.
.TP 8
M       (output) INTEGER
The number of columns in the arrays VL and/or VR actually
used to store the eigenvectors.  If HOWMNY = 'A' or 'B', M
is set to N.  Each selected eigenvector occupies one column.
.TP 8
WORK    (workspace) COMPLEX array, dimension (2*N)
.TP 8
RWORK   (workspace) REAL array, dimension (2*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
