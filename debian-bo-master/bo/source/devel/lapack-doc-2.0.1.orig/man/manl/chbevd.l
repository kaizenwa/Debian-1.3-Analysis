.TH CHBEVD l "08 October 1994" "LAPACK version 2.0" "LAPACK driver routine (version 2.0)"
.TH CHBEVD l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CHBEVD - compute all the eigenvalues and, optionally, eigenvectors of a complex Hermitian band matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE CHBEVD(
JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK,
LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO )
.TP 19
.ti +4
CHARACTER
JOBZ, UPLO
.TP 19
.ti +4
INTEGER
INFO, KD, LDAB, LDZ, LIWORK, LRWORK, LWORK, N
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
REAL
RWORK( * ), W( * )
.TP 19
.ti +4
COMPLEX
AB( LDAB, * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
CHBEVD computes all the eigenvalues and, optionally, eigenvectors of
a complex Hermitian band matrix A.  If eigenvectors are desired, it
uses a divide and conquer algorithm.
.br

The divide and conquer algorithm makes very mild assumptions about
floating point arithmetic. It will work on machines with a guard
digit in add/subtract, or on those binary machines without guard
digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
Cray-2. It could conceivably fail on hexadecimal or decimal machines
without guard digits, but we know of none.
.br

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
KD      (input) INTEGER
The number of superdiagonals of the matrix A if UPLO = 'U',
or the number of subdiagonals if UPLO = 'L'.  KD >= 0.
.TP 8
AB      (input/output) COMPLEX array, dimension (LDAB, N)
On entry, the upper or lower triangle of the Hermitian band
matrix A, stored in the first KD+1 rows of the array.  The
j-th column of A is stored in the j-th column of the array AB
as follows:
if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).

On exit, AB is overwritten by values generated during the
reduction to tridiagonal form.  If UPLO = 'U', the first
superdiagonal and the diagonal of the tridiagonal matrix T
are returned in rows KD and KD+1 of AB, and if UPLO = 'L',
the diagonal and first subdiagonal of T are returned in the
first two rows of AB.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KD + 1.
.TP 8
W       (output) REAL array, dimension (N)
If INFO = 0, the eigenvalues in ascending order.
.TP 8
Z       (output) COMPLEX array, dimension (LDZ, N)
If JOBZ = 'V', then if INFO = 0, Z contains the orthonormal
eigenvectors of the matrix A, with the i-th column of Z
holding the eigenvector associated with W(i).
If JOBZ = 'N', then Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = 'V', LDZ >= max(1,N).
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (LWORK)
On exit, if LWORK > 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
If N <= 1,               LWORK must be at least 1.
If JOBZ = 'N' and N > 1, LWORK must be at least N.
If JOBZ = 'V' and N > 1, LWORK must be at least 2*N**2.
.TP 8
RWORK   (workspace/output) REAL array,
dimension (LRWORK)
On exit, if LRWORK > 0, RWORK(1) returns the optimal LRWORK.
.TP 8
LRWORK  (input) INTEGER
The dimension of array RWORK.
If N <= 1,               LRWORK must be at least 1.
If JOBZ = 'N' and N > 1, LRWORK must be at least N.
If JOBZ = 'V' and N > 1, LRWORK must be at least
1 + 4*N + 2*N*lg N + 3*N**2 ,
where lg( N ) = smallest integer k such
that 2**k >= N .
.TP 8
IWORK   (workspace/output) INTEGER array, dimension (LIWORK)
On exit, if LIWORK > 0, IWORK(1) returns the optimal LIWORK.
.TP 8
LIWORK  (input) INTEGER
The dimension of array IWORK.
If JOBZ = 'N' or N <= 1, LIWORK must be at least 1.
If JOBZ = 'V' and N > 1, LIWORK must be at least 2 + 5*N .
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if INFO = i, the algorithm failed to converge; i
off-diagonal elements of an intermediate tridiagonal
form did not converge to zero.
