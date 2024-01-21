.TH SSTEVX l "08 October 1994" "LAPACK version 2.0" "LAPACK driver routine (version 2.0)"
.TH SSTEVX l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SSTEVX - compute selected eigenvalues and, optionally, eigenvectors of a real symmetric tridiagonal matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE SSTEVX(
JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL,
M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO )
.TP 19
.ti +4
CHARACTER
JOBZ, RANGE
.TP 19
.ti +4
INTEGER
IL, INFO, IU, LDZ, M, N
.TP 19
.ti +4
REAL
ABSTOL, VL, VU
.TP 19
.ti +4
INTEGER
IFAIL( * ), IWORK( * )
.TP 19
.ti +4
REAL
D( * ), E( * ), W( * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
SSTEVX computes selected eigenvalues and, optionally, eigenvectors
of a real symmetric tridiagonal matrix A.  Eigenvalues and
eigenvectors can be selected by specifying either a range of values
or a range of indices for the desired eigenvalues.
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
N       (input) INTEGER
The order of the matrix.  N >= 0.
.TP 8
D       (input/output) REAL array, dimension (N)
On entry, the n diagonal elements of the tridiagonal matrix
A.
On exit, D may be multiplied by a constant factor chosen
to avoid over/underflow in computing the eigenvalues.
.TP 8
E       (input/output) REAL array, dimension (N)
On entry, the (n-1) subdiagonal elements of the tridiagonal
matrix A in elements 1 to N-1 of E; E(N) need not be set.
On exit, E may be multiplied by a constant factor chosen
to avoid over/underflow in computing the eigenvalues.
.TP 8
VL      (input) REAL
VU      (input) REAL
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
ABSTOL  (input) REAL
The absolute error tolerance for the eigenvalues.
An approximate eigenvalue is accepted as converged
when it is determined to lie in an interval [a,b]
of width less than or equal to

ABSTOL + EPS *   max( |a|,|b| ) ,

where EPS is the machine precision.  If ABSTOL is less
than or equal to zero, then  EPS*|T|  will be used in
its place, where |T| is the 1-norm of the tridiagonal
matrix.

Eigenvalues will be computed most accurately when ABSTOL is
set to twice the underflow threshold 2*SLAMCH('S'), not zero.
If this routine returns with INFO>0, indicating that some
eigenvectors did not converge, try setting ABSTOL to 
2*SLAMCH('S').

See "Computing Small Singular Values of Bidiagonal Matrices
with Guaranteed High Relative Accuracy," by Demmel and
Kahan, LAPACK Working Note #3.
.TP 8
M       (output) INTEGER
The total number of eigenvalues found.  0 <= M <= N.
If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
.TP 8
W       (output) REAL array, dimension (N)
The first M elements contain the selected eigenvalues in
ascending order.
.TP 8
Z       (output) REAL array, dimension (LDZ, max(1,M) )
If JOBZ = 'V', then if INFO = 0, the first M columns of Z
contain the orthonormal eigenvectors of the matrix A
corresponding to the selected eigenvalues, with the i-th
column of Z holding the eigenvector associated with W(i).
If an eigenvector fails to converge (INFO > 0), then that
column of Z contains the latest approximation to the
eigenvector, and the index of the eigenvector is returned
in IFAIL.  If JOBZ = 'N', then Z is not referenced.
Note: the user must ensure that at least max(1,M) columns are
supplied in the array Z; if RANGE = 'V', the exact value of M
is not known in advance and an upper bound must be used.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = 'V', LDZ >= max(1,N).
.TP 8
WORK    (workspace) REAL array, dimension (5*N)
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
