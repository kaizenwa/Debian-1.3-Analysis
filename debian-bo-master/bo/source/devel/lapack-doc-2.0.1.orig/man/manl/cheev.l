.TH CHEEV l "08 October 1994" "LAPACK version 2.0" "LAPACK driver routine (version 2.0)"
.TH CHEEV l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CHEEV - compute all eigenvalues and, optionally, eigenvectors of a complex Hermitian matrix A
.SH SYNOPSIS
.TP 18
SUBROUTINE CHEEV(
JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK,
INFO )
.TP 18
.ti +4
CHARACTER
JOBZ, UPLO
.TP 18
.ti +4
INTEGER
INFO, LDA, LWORK, N
.TP 18
.ti +4
REAL
RWORK( * ), W( * )
.TP 18
.ti +4
COMPLEX
A( LDA, * ), WORK( * )
.SH PURPOSE
CHEEV computes all eigenvalues and, optionally, eigenvectors of a
complex Hermitian matrix A.

.SH ARGUMENTS
.TP 8
JOBZ    (input) CHARACTER*1
= 'N':  Compute eigenvalues only;
.br
= 'V':  Compute eigenvalues and eigenvectors.
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
A       (input/output) COMPLEX array, dimension (LDA, N)
On entry, the Hermitian matrix A.  If UPLO = 'U', the
leading N-by-N upper triangular part of A contains the
upper triangular part of the matrix A.  If UPLO = 'L',
the leading N-by-N lower triangular part of A contains
the lower triangular part of the matrix A.
On exit, if JOBZ = 'V', then if INFO = 0, A contains the
orthonormal eigenvectors of the matrix A.
If JOBZ = 'N', then on exit the lower triangle (if UPLO='L')
or the upper triangle (if UPLO='U') of A, including the
diagonal, is destroyed.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
W       (output) REAL array, dimension (N)
If INFO = 0, the eigenvalues in ascending order.
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The length of the array WORK.  LWORK >= max(1,2*N-1).
For optimal efficiency, LWORK >= (NB+1)*N,
where NB is the blocksize for CHETRD returned by ILAENV.
.TP 8
RWORK   (workspace) REAL array, dimension (max(1, 3*N-2))
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the algorithm failed to converge; i
off-diagonal elements of an intermediate tridiagonal
form did not converge to zero.
