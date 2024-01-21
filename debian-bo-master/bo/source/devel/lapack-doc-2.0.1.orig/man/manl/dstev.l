.TH DSTEV l "08 October 1994" "LAPACK version 2.0" "LAPACK driver routine (version 2.0)"
.TH DSTEV l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DSTEV - compute all eigenvalues and, optionally, eigenvectors of a real symmetric tridiagonal matrix A
.SH SYNOPSIS
.TP 18
SUBROUTINE DSTEV(
JOBZ, N, D, E, Z, LDZ, WORK, INFO )
.TP 18
.ti +4
CHARACTER
JOBZ
.TP 18
.ti +4
INTEGER
INFO, LDZ, N
.TP 18
.ti +4
DOUBLE
PRECISION D( * ), E( * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
DSTEV computes all eigenvalues and, optionally, eigenvectors of a
real symmetric tridiagonal matrix A.

.SH ARGUMENTS
.TP 8
JOBZ    (input) CHARACTER*1
= 'N':  Compute eigenvalues only;
.br
= 'V':  Compute eigenvalues and eigenvectors.
.TP 8
N       (input) INTEGER
The order of the matrix.  N >= 0.
.TP 8
D       (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the n diagonal elements of the tridiagonal matrix
A.
On exit, if INFO = 0, the eigenvalues in ascending order.
.TP 8
E       (input/output) DOUBLE PRECISION array, dimension (N)
On entry, the (n-1) subdiagonal elements of the tridiagonal
matrix A, stored in elements 1 to N-1 of E; E(N) need not
be set, but is used by the routine.
On exit, the contents of E are destroyed.
.TP 8
Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
If JOBZ = 'V', then if INFO = 0, Z contains the orthonormal
eigenvectors of the matrix A, with the i-th column of Z
holding the eigenvector associated with D(i).
If JOBZ = 'N', then Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = 'V', LDZ >= max(1,N).
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (max(1,2*N-2))
If JOBZ = 'N', WORK is not referenced.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the algorithm failed to converge; i
off-diagonal elements of E did not converge to zero.
