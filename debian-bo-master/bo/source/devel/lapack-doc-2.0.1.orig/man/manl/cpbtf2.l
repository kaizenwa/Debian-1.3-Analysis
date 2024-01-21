.TH CPBTF2 l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH CPBTF2 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CPBTF2 - compute the Cholesky factorization of a complex Hermitian positive definite band matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE CPBTF2(
UPLO, N, KD, AB, LDAB, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, KD, LDAB, N
.TP 19
.ti +4
COMPLEX
AB( LDAB, * )
.SH PURPOSE
CPBTF2 computes the Cholesky factorization of a complex Hermitian
positive definite band matrix A.

The factorization has the form
.br
   A = U' * U ,  if UPLO = 'U', or
.br
   A = L  * L',  if UPLO = 'L',
.br
where U is an upper triangular matrix, U' is the conjugate transpose
of U, and L is lower triangular.
.br

This is the unblocked version of the algorithm, calling Level 2 BLAS.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the upper or lower triangular part of the
Hermitian matrix A is stored:
.br
= 'U':  Upper triangular
.br
= 'L':  Lower triangular
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
KD      (input) INTEGER
The number of super-diagonals of the matrix A if UPLO = 'U',
or the number of sub-diagonals if UPLO = 'L'.  KD >= 0.
.TP 8
AB      (input/output) COMPLEX array, dimension (LDAB,N)
On entry, the upper or lower triangle of the Hermitian band
matrix A, stored in the first KD+1 rows of the array.  The
j-th column of A is stored in the j-th column of the array AB
as follows:
if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).

On exit, if INFO = 0, the triangular factor U or L from the
Cholesky factorization A = U'*U or A = L*L' of the band
matrix A, in the same storage format as A.
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KD+1.
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -k, the k-th argument had an illegal value
.br
> 0: if INFO = k, the leading minor of order k is not
positive definite, and the factorization could not be
completed.
.SH FURTHER DETAILS
The band storage scheme is illustrated by the following example, when
N = 6, KD = 2, and UPLO = 'U':
.br

On entry:                       On exit:
.br

    *    *   a13  a24  a35  a46      *    *   u13  u24  u35  u46
    *   a12  a23  a34  a45  a56      *   u12  u23  u34  u45  u56
   a11  a22  a33  a44  a55  a66     u11  u22  u33  u44  u55  u66

Similarly, if UPLO = 'L' the format of A is as follows:
.br

On entry:                       On exit:
.br

   a11  a22  a33  a44  a55  a66     l11  l22  l33  l44  l55  l66
   a21  a32  a43  a54  a65   *      l21  l32  l43  l54  l65   *
   a31  a42  a53  a64   *    *      l31  l42  l53  l64   *    *

Array elements marked * are not used by the routine.
.br

