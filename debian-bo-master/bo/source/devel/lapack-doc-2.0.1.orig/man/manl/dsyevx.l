.TH DSYEVX l "08 October 1994" "LAPACK version 2.0" "LAPACK driver routine (version 2.0)"
.TH DSYEVX l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DSYEVX - compute selected eigenvalues and, optionally, eigenvectors of a real symmetric matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE DSYEVX(
JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU,
ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK,
IFAIL, INFO )
.TP 19
.ti +4
CHARACTER
JOBZ, RANGE, UPLO
.TP 19
.ti +4
INTEGER
IL, INFO, IU, LDA, LDZ, LWORK, M, N
.TP 19
.ti +4
DOUBLE
PRECISION ABSTOL, VL, VU
.TP 19
.ti +4
INTEGER
IFAIL( * ), IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), W( * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
DSYEVX computes selected eigenvalues and, optionally, eigenvectors
of a real symmetric matrix A.  Eigenvalues and eigenvectors can be
selected by specifying either a range of values or a range of indices
for the desired eigenvalues.
.br

.SH ARGUMENTS
.TP 8
JOBZ    (input) CHARACTER*1
= 'N':  Compute eigenvalues only;
.br
= 'V':  Compute eigenvalues and eigenvectors.
.TP 8
RANGE   (input) CHARACTER*1
.br
= 'A': all eigenvalues will be found.
.br
= 'V': all eigenvalues in the half-open interval (VL,VU]
will be found.
= 'I': the IL-th through IU-th eigenvalues will be found.
.TP 8
UPLO    (input) CHARACTER*1
.br
= 'U':  Upper triangle of A is stored;
.br
= 'L':  Lower triangle of A is stored.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
On entry, the symmetric matrix A.  If UPLO = 'U', the
leading N-by-N upper triangular part of A contains the
upper triangular part of the matrix A.  If UPLO = 'L',
the leading N-by-N lower triangular part of A contains
the lower triangular part of the matrix A.
On exit, the lower triangle (if UPLO='L') or the upper
triangle (if UPLO='U') of A, including the diagonal, is
destroyed.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
VL      (input) DOUBLE PRECISION
VU      (input) DOUBLE PRECISION
If RANGE='V', the lower and upper bounds of the interval to
be searched for eigenvalues. VL < VU.
Not referenced if RANGE = 'A' or 'I'.
.TP 8
IL      (input) INTEGER
IU      (input) INTEGER
If RANGE='I', the indices (in ascending order) of the
smallest and largest eigenvalues to be returned.
1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
Not referenced if RANGE = 'A' or 'V'.
.TP 8
ABSTOL  (input) DOUBLE PRECISION
The absolute error tolerance for the eigenvalues.
An approximate eigenvalue is accepted as converged
when it is determined to lie in an interval [a,b]
of width less than or equal to

ABSTOL + EPS *   max( |a|,|b| ) ,

where EPS is the machine precision.  If ABSTOL is less than
or equal to zero, then  EPS*|T|  will be used in its place,
where |T| is the 1-norm of the tridiagonal matrix obtained
by reducing A to tridiagonal form.

Eigenvalues will be computed most accurately when ABSTOL is
set to twice the underflow threshold 2*DLAMCH('S'), not zero.
If this routine returns with INFO>0, indicating that some
eigenvectors did not converge, try setting ABSTOL to
2*DLAMCH('S').

See "Computing Small Singular Values of Bidiagonal Matrices
with Guaranteed High Relative Accuracy," by Demmel and
Kahan, LAPACK Working Note #3.
.TP 8
M       (output) INTEGER
The total number of eigenvalues found.  0 <= M <= N.
If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
.TP 8
W       (output) DOUBLE PRECISION array, dimension (N)
On normal exit, the first M elements contain the selected
eigenvalues in ascending order.
.TP 8
Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M))
If JOBZ = 'V', then if INFO = 0, the first M columns of Z
contain the orthonormal eigenvectors of the matrix A
corresponding to the selected eigenvalues, with the i-th
column of Z holding the eigenvector associated with W(i).
If an eigenvector fails to converge, then that column of Z
contains the latest approximation to the eigenvector, and the
index of the eigenvector is returned in IFAIL.
If JOBZ = 'N', then Z is not referenced.
Note: the user must ensure that at least max(1,M) columns are
supplied in the array Z; if RANGE = 'V', the exact value of M
is not known in advance and an upper bound must be used.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = 'V', LDZ >= max(1,N).
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The length of the array WORK.  LWORK >= max(1,8*N).
For optimal efficiency, LWORK >= (NB+3)*N,
where NB is the blocksize for DSYTRD returned by ILAENV.
.TP 8
IWORK   (workspace) INTEGER array, dimension (5*N)
.TP 8
IFAIL   (output) INTEGER array, dimension (N)
If JOBZ = 'V', then if INFO = 0, the first M elements of
IFAIL are zero.  If INFO > 0, then IFAIL contains the
indices of the eigenvectors that failed to converge.
If JOBZ = 'N', then IFAIL is not referenced.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, then i eigenvectors failed to converge.
Their indices are stored in array IFAIL.
