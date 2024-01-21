.TH DPOTF2 l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH DPOTF2 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DPOTF2 - compute the Cholesky factorization of a real symmetric positive definite matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE DPOTF2(
UPLO, N, A, LDA, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, N
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * )
.SH PURPOSE
DPOTF2 computes the Cholesky factorization of a real symmetric
positive definite matrix A.

The factorization has the form
.br
   A = U' * U ,  if UPLO = 'U', or
.br
   A = L  * L',  if UPLO = 'L',
.br
where U is an upper triangular matrix and L is lower triangular.

This is the unblocked version of the algorithm, calling Level 2 BLAS.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the upper or lower triangular part of the
symmetric matrix A is stored.
= 'U':  Upper triangular
.br
= 'L':  Lower triangular
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the symmetric matrix A.  If UPLO = 'U', the leading
n by n upper triangular part of A contains the upper
triangular part of the matrix A, and the strictly lower
triangular part of A is not referenced.  If UPLO = 'L', the
leading n by n lower triangular part of A contains the lower
triangular part of the matrix A, and the strictly upper
triangular part of A is not referenced.

On exit, if INFO = 0, the factor U or L from the Cholesky
factorization A = U'*U  or A = L*L'.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -k, the k-th argument had an illegal value
.br
> 0: if INFO = k, the leading minor of order k is not
positive definite, and the factorization could not be
completed.
