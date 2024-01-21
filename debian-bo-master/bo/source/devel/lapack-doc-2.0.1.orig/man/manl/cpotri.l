.TH CPOTRI l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH CPOTRI l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CPOTRI - compute the inverse of a complex Hermitian positive definite matrix A using the Cholesky factorization A = U**H*U or A = L*L**H computed by CPOTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE CPOTRI(
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
COMPLEX
A( LDA, * )
.SH PURPOSE
CPOTRI computes the inverse of a complex Hermitian positive definite
matrix A using the Cholesky factorization A = U**H*U or A = L*L**H
computed by CPOTRF.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= 'U':  Upper triangle of A is stored;
.br
= 'L':  Lower triangle of A is stored.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the triangular factor U or L from the Cholesky
factorization A = U**H*U or A = L*L**H, as computed by
CPOTRF.
On exit, the upper or lower triangle of the (Hermitian)
inverse of A, overwriting the input factor U or L.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the (i,i) element of the factor U or L is
zero, and the inverse could not be computed.
