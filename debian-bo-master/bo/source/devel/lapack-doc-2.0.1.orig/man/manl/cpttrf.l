.TH CPTTRF l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH CPTTRF l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CPTTRF - compute the factorization of a complex Hermitian positive definite tridiagonal matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE CPTTRF(
N, D, E, INFO )
.TP 19
.ti +4
INTEGER
INFO, N
.TP 19
.ti +4
REAL
D( * )
.TP 19
.ti +4
COMPLEX
E( * )
.SH PURPOSE
CPTTRF computes the factorization of a complex Hermitian positive
definite tridiagonal matrix A.

If the subdiagonal elements of A are supplied in the array E, the
factorization has the form A = L*D*L**H, where D is diagonal and L
is unit lower bidiagonal; if the superdiagonal elements of A are
supplied, it has the form A = U**H*D*U, where U is unit upper
bidiagonal.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
D       (input/output) REAL array, dimension (N)
On entry, the n diagonal elements of the tridiagonal matrix
A.  On exit, the n diagonal elements of the diagonal matrix
D from the L*D*L**H factorization of A.
.TP 8
E       (input/output) COMPLEX array, dimension (N-1)
On entry, the (n-1) off-diagonal elements of the tridiagonal
matrix A.  On exit, the (n-1) off-diagonal elements of the
unit bidiagonal factor L or U from the factorization of A.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the leading minor of order i is not
positive definite; if i < N, the factorization could
not be completed, while if i = N, the factorization was
completed, but D(N) = 0.
