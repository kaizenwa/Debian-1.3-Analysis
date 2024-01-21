.TH DTGEVC l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH DTGEVC l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DTGEVC - compute some or all of the right and/or left generalized eigenvectors of a pair of real upper triangular matrices (A,B)
.SH SYNOPSIS
.TP 19
SUBROUTINE DTGEVC(
SIDE, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,
LDVL, VR, LDVR, MM, M, WORK, INFO )
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
DOUBLE
PRECISION A( LDA, * ), B( LDB, * ), VL( LDVL, * ),
VR( LDVR, * ), WORK( * )
.SH PURPOSE
DTGEVC computes some or all of the right and/or left generalized
eigenvectors of a pair of real upper triangular matrices (A,B).

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
the products Z*X and/or Q*Y, where Z and Q are input orthogonal
matrices.  If (A,B) was obtained from the generalized real-Schur
factorization of an original pair of matrices
.br
   (A0,B0) = (Q*A*Z**H,Q*B*Z**H),
.br
then Z*X and Q*Y are the matrices of right or left eigenvectors of
A.
.br

A must be block upper triangular, with 1-by-1 and 2-by-2 diagonal
blocks.  Corresponding to each 2-by-2 diagonal block is a complex
conjugate pair of eigenvalues and eigenvectors; only one
.br
eigenvector of the pair is computed, namely the one corresponding
to the eigenvalue with positive imaginary part.
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
To select the real eigenvector corresponding to the real
eigenvalue w(j), SELECT(j) must be set to .TRUE.  To select
the complex eigenvector corresponding to a complex conjugate
pair w(j) and w(j+1), either SELECT(j) or SELECT(j+1) must
be set to .TRUE..
.TP 8
N       (input) INTEGER
The order of the matrices A and B.  N >= 0.
.TP 8
A       (input) DOUBLE PRECISION array, dimension (LDA,N)
The upper quasi-triangular matrix A.
.TP 8
LDA     (input) INTEGER
The leading dimension of array A.  LDA >= max(1, N).
.TP 8
B       (input) DOUBLE PRECISION array, dimension (LDB,N)
The upper triangular matrix B.  If A has a 2-by-2 diagonal
block, then the corresponding 2-by-2 block of B must be
diagonal with positive elements.
.TP 8
LDB     (input) INTEGER
The leading dimension of array B.  LDB >= max(1,N).
.TP 8
VL      (input/output) DOUBLE PRECISION array, dimension (LDVL,MM)
On entry, if SIDE = 'L' or 'B' and HOWMNY = 'B', VL must
contain an N-by-N matrix Q (usually the orthogonal matrix Q
of left Schur vectors returned by DHGEQZ).
On exit, if SIDE = 'L' or 'B', VL contains:
if HOWMNY = 'A', the matrix Y of left eigenvectors of (A,B);
if HOWMNY = 'B', the matrix Q*Y;
if HOWMNY = 'S', the left eigenvectors of (A,B) specified by
SELECT, stored consecutively in the columns of
VL, in the same order as their eigenvalues.
If SIDE = 'R', VL is not referenced.

A complex eigenvector corresponding to a complex eigenvalue
is stored in two consecutive columns, the first holding the
real part, and the second the imaginary part.
.TP 8
LDVL    (input) INTEGER
The leading dimension of array VL.
LDVL >= max(1,N) if SIDE = 'L' or 'B'; LDVL >= 1 otherwise.
.TP 8
VR      (input/output) COMPLEX*16 array, dimension (LDVR,MM)
On entry, if SIDE = 'R' or 'B' and HOWMNY = 'B', VR must
contain an N-by-N matrix Q (usually the orthogonal matrix Z
of right Schur vectors returned by DHGEQZ).
On exit, if SIDE = 'R' or 'B', VR contains:
if HOWMNY = 'A', the matrix X of right eigenvectors of (A,B);
if HOWMNY = 'B', the matrix Z*X;
if HOWMNY = 'S', the right eigenvectors of (A,B) specified by
SELECT, stored consecutively in the columns of
VR, in the same order as their eigenvalues.
If SIDE = 'L', VR is not referenced.

A complex eigenvector corresponding to a complex eigenvalue
is stored in two consecutive columns, the first holding the
real part and the second the imaginary part.
.TP 8
LDVR    (input) INTEGER
The leading dimension of the array VR.
LDVR >= max(1,N) if SIDE = 'R' or 'B'; LDVR >= 1 otherwise.
.TP 8
MM      (input) INTEGER
The number of columns in the arrays VL and/or VR. MM >= M.
.TP 8
M       (output) INTEGER
The number of columns in the arrays VL and/or VR actually
used to store the eigenvectors.  If HOWMNY = 'A' or 'B', M
is set to N.  Each selected real eigenvector occupies one
column and each selected complex eigenvector occupies two
columns.
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (6*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  the 2-by-2 block (INFO:INFO+1) does not have a complex
eigenvalue.
.SH FURTHER DETAILS
Allocation of workspace:
.br
---------- -- ---------
.br

   WORK( j ) = 1-norm of j-th column of A, above the diagonal
   WORK( N+j ) = 1-norm of j-th column of B, above the diagonal
   WORK( 2*N+1:3*N ) = real part of eigenvector
.br
   WORK( 3*N+1:4*N ) = imaginary part of eigenvector
.br
   WORK( 4*N+1:5*N ) = real part of back-transformed eigenvector
   WORK( 5*N+1:6*N ) = imaginary part of back-transformed eigenvector

Rowwise vs. columnwise solution methods:
.br
------- --  ---------- -------- -------
.br

Finding a generalized eigenvector consists basically of solving the
singular triangular system
.br

 (A - w B) x = 0     (for right) or:   (A - w B)**H y = 0  (for left)

Consider finding the i-th right eigenvector (assume all eigenvalues
are real). The equation to be solved is:
.br
     n                   i
.br
0 = sum  C(j,k) v(k)  = sum  C(j,k) v(k)     for j = i,. . .,1
    k=j                 k=j
.br

where  C = (A - w B)  (The components v(i+1:n) are 0.)
.br

The "rowwise" method is:
.br

(1)  v(i) := 1
.br
for j = i-1,. . .,1:
.br
                        i
.br
    (2) compute  s = - sum C(j,k) v(k)   and
.br
                      k=j+1
.br

    (3) v(j) := s / C(j,j)
.br

Step 2 is sometimes called the "dot product" step, since it is an
inner product between the j-th row and the portion of the eigenvector
that has been computed so far.
.br

The "columnwise" method consists basically in doing the sums
for all the rows in parallel.  As each v(j) is computed, the
contribution of v(j) times the j-th column of C is added to the
partial sums.  Since FORTRAN arrays are stored columnwise, this has
the advantage that at each step, the elements of C that are accessed
are adjacent to one another, whereas with the rowwise method, the
elements accessed at a step are spaced LDA (and LDB) words apart.

When finding left eigenvectors, the matrix in question is the
transpose of the one in storage, so the rowwise method then
actually accesses columns of A and B at each step, and so is the
preferred method.
.br

