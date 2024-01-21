.TH SSPEVD l "08 October 1994" "LAPACK version 2.0" "LAPACK driver routine (version 2.0)"
.TH SSPEVD l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SSPEVD - compute all the eigenvalues and, optionally, eigenvectors of a real symmetric matrix A in packed storage
.SH SYNOPSIS
.TP 19
SUBROUTINE SSPEVD(
JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, LWORK,
IWORK, LIWORK, INFO )
.TP 19
.ti +4
CHARACTER
JOBZ, UPLO
.TP 19
.ti +4
INTEGER
INFO, LDZ, LIWORK, LWORK, N
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
REAL
AP( * ), W( * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
SSPEVD computes all the eigenvalues and, optionally, eigenvectors
of a real symmetric matrix A in packed storage. If eigenvectors are
desired, it uses a divide and conquer algorithm.
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
AP      (input/output) REAL array, dimension (N*(N+1)/2)
On entry, the upper or lower triangle of the symmetric matrix
A, packed columnwise in a linear array.  The j-th column of A
is stored in the array AP as follows:
if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = 'L', AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n.

On exit, AP is overwritten by values generated during the
reduction to tridiagonal form.  If UPLO = 'U', the diagonal
and first superdiagonal of the tridiagonal matrix T overwrite
the corresponding elements of A, and if UPLO = 'L', the
diagonal and first subdiagonal of T overwrite the
corresponding elements of A.
.TP 8
W       (output) REAL array, dimension (N)
If INFO = 0, the eigenvalues in ascending order.
.TP 8
Z       (output) REAL array, dimension (LDZ, N)
If JOBZ = 'V', then if INFO = 0, Z contains the orthonormal
eigenvectors of the matrix A, with the i-th column of Z
holding the eigenvector associated with W(i).
If JOBZ = 'N', then Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = 'V', LDZ >= max(1,N).
.TP 8
WORK    (workspace/output) REAL array,
dimension (LWORK)
On exit, if LWORK > 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
If N <= 1,               LWORK must be at least 1.
If JOBZ = 'N' and N > 1, LWORK must be at least 2*N.
If JOBZ = 'V' and N > 1, LWORK must be at least
( 1 + 5*N + 2*N*lg N + 2*N**2 ),
where lg( N ) = smallest integer k such
that 2**k >= N.
.TP 8
IWORK   (workspace/output) INTEGER array, dimension (LIWORK)
On exit, if LIWORK > 0, IWORK(1) returns the optimal LIWORK.
.TP 8
LIWORK  (input) INTEGER
The dimension of the array IWORK.
If JOBZ  = 'N' or N <= 1, LIWORK must be at least 1.
If JOBZ  = 'V' and N > 1, LIWORK must be at least 2 + 5*N.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if INFO = i, the algorithm failed to converge; i
off-diagonal elements of an intermediate tridiagonal
form did not converge to zero.
