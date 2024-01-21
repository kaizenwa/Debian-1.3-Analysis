.TH DTREVC l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH DTREVC l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DTREVC - compute some or all of the right and/or left eigenvectors of a real upper quasi-triangular matrix T
.SH SYNOPSIS
.TP 19
SUBROUTINE DTREVC(
SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,
LDVR, MM, M, WORK, INFO )
.TP 19
.ti +4
CHARACTER
HOWMNY, SIDE
.TP 19
.ti +4
INTEGER
INFO, LDT, LDVL, LDVR, M, MM, N
.TP 19
.ti +4
LOGICAL
SELECT( * )
.TP 19
.ti +4
DOUBLE
PRECISION T( LDT, * ), VL( LDVL, * ), VR( LDVR, * ),
WORK( * )
.SH PURPOSE
DTREVC computes some or all of the right and/or left eigenvectors of
a real upper quasi-triangular matrix T.

The right eigenvector x and the left eigenvector y of T corresponding
to an eigenvalue w are defined by:
.br

             T*x = w*x,     y'*T = w*y'
.br

where y' denotes the conjugate transpose of the vector y.

If all eigenvectors are requested, the routine may either return the
matrices X and/or Y of right or left eigenvectors of T, or the
products Q*X and/or Q*Y, where Q is an input orthogonal
.br
matrix. If T was obtained from the real-Schur factorization of an
original matrix A = Q*T*Q', then Q*X and Q*Y are the matrices of
right or left eigenvectors of A.
.br

T must be in Schur canonical form (as returned by DHSEQR), that is,
block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
2-by-2 diagonal block has its diagonal elements equal and its
off-diagonal elements of opposite sign.  Corresponding to each 2-by-2
diagonal block is a complex conjugate pair of eigenvalues and
eigenvectors; only one eigenvector of the pair is computed, namely
the one corresponding to the eigenvalue with positive imaginary part.


.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= 'R':  compute right eigenvectors only;
.br
= 'L':  compute left eigenvectors only;
.br
= 'B':  compute both right and left eigenvectors.
.TP 8
HOWMNY  (input) CHARACTER*1
.br
= 'A':  compute all right and/or left eigenvectors;
.br
= 'B':  compute all right and/or left eigenvectors,
and backtransform them using the input matrices
supplied in VR and/or VL;
= 'S':  compute selected right and/or left eigenvectors,
specified by the logical array SELECT.
.TP 8
SELECT  (input/output) LOGICAL array, dimension (N)
If HOWMNY = 'S', SELECT specifies the eigenvectors to be
computed.
If HOWMNY = 'A' or 'B', SELECT is not referenced.
To select the real eigenvector corresponding to a real
eigenvalue w(j), SELECT(j) must be set to .TRUE..  To select
the complex eigenvector corresponding to a complex conjugate
pair w(j) and w(j+1), either SELECT(j) or SELECT(j+1) must be
set to .TRUE.; then on exit SELECT(j) is .TRUE. and
SELECT(j+1) is .FALSE..
.TP 8
N       (input) INTEGER
The order of the matrix T. N >= 0.
.TP 8
T       (input) DOUBLE PRECISION array, dimension (LDT,N)
The upper quasi-triangular matrix T in Schur canonical form.
.TP 8
LDT     (input) INTEGER
The leading dimension of the array T. LDT >= max(1,N).
.TP 8
VL      (input/output) DOUBLE PRECISION array, dimension (LDVL,MM)
On entry, if SIDE = 'L' or 'B' and HOWMNY = 'B', VL must
contain an N-by-N matrix Q (usually the orthogonal matrix Q
of Schur vectors returned by DHSEQR).
On exit, if SIDE = 'L' or 'B', VL contains:
if HOWMNY = 'A', the matrix Y of left eigenvectors of T;
if HOWMNY = 'B', the matrix Q*Y;
if HOWMNY = 'S', the left eigenvectors of T specified by
SELECT, stored consecutively in the columns
of VL, in the same order as their
eigenvalues.
A complex eigenvector corresponding to a complex eigenvalue
is stored in two consecutive columns, the first holding the
real part, and the second the imaginary part.
If SIDE = 'R', VL is not referenced.
.TP 8
LDVL    (input) INTEGER
The leading dimension of the array VL.  LDVL >= max(1,N) if
SIDE = 'L' or 'B'; LDVL >= 1 otherwise.
.TP 8
VR      (input/output) DOUBLE PRECISION array, dimension (LDVR,MM)
On entry, if SIDE = 'R' or 'B' and HOWMNY = 'B', VR must
contain an N-by-N matrix Q (usually the orthogonal matrix Q
of Schur vectors returned by DHSEQR).
On exit, if SIDE = 'R' or 'B', VR contains:
if HOWMNY = 'A', the matrix X of right eigenvectors of T;
if HOWMNY = 'B', the matrix Q*X;
if HOWMNY = 'S', the right eigenvectors of T specified by
SELECT, stored consecutively in the columns
of VR, in the same order as their
eigenvalues.
A complex eigenvector corresponding to a complex eigenvalue
is stored in two consecutive columns, the first holding the
real part and the second the imaginary part.
If SIDE = 'L', VR is not referenced.
.TP 8
LDVR    (input) INTEGER
The leading dimension of the array VR.  LDVR >= max(1,N) if
SIDE = 'R' or 'B'; LDVR >= 1 otherwise.
.TP 8
MM      (input) INTEGER
The number of columns in the arrays VL and/or VR. MM >= M.
.TP 8
M       (output) INTEGER
The number of columns in the arrays VL and/or VR actually
used to store the eigenvectors.
If HOWMNY = 'A' or 'B', M is set to N.
Each selected real eigenvector occupies one column and each
selected complex eigenvector occupies two columns.
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
The algorithm used in this program is basically backward (forward)
substitution, with scaling to make the the code robust against
possible overflow.
.br

Each eigenvector is normalized so that the element of largest
magnitude has magnitude 1; here the magnitude of a complex number
(x,y) is taken to be |x| + |y|.
.br

