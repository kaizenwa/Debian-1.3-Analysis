.TH ZPPTRI l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH ZPPTRI l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
ZPPTRI - compute the inverse of a complex Hermitian positive definite matrix A using the Cholesky factorization A = U**H*U or A = L*L**H computed by ZPPTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE ZPPTRI(
UPLO, N, AP, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, N
.TP 19
.ti +4
COMPLEX*16
AP( * )
.SH PURPOSE
ZPPTRI computes the inverse of a complex Hermitian positive definite
matrix A using the Cholesky factorization A = U**H*U or A = L*L**H
computed by ZPPTRF.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= 'U':  Upper triangular factor is stored in AP;
.br
= 'L':  Lower triangular factor is stored in AP.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
AP      (input/output) COMPLEX*16 array, dimension (N*(N+1)/2)
On entry, the triangular factor U or L from the Cholesky
factorization A = U**H*U or A = L*L**H, packed columnwise as
a linear array.  The j-th column of U or L is stored in the
array AP as follows:
if UPLO = 'U', AP(i + (j-1)*j/2) = U(i,j) for 1<=i<=j;
if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = L(i,j) for j<=i<=n.

On exit, the upper or lower triangle of the (Hermitian)
inverse of A, overwriting the input factor U or L.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the (i,i) element of the factor U or L is
zero, and the inverse could not be computed.
