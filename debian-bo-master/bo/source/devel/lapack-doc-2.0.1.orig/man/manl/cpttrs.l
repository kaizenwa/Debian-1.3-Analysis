.TH CPTTRS l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH CPTTRS l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CPTTRS - solve a system of linear equations A * X = B with a Hermitian positive definite tridiagonal matrix A using the factorization A = U**H*D*U or A = L*D*L**H computed by CPTTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE CPTTRS(
UPLO, N, NRHS, D, E, B, LDB, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDB, N, NRHS
.TP 19
.ti +4
REAL
D( * )
.TP 19
.ti +4
COMPLEX
B( LDB, * ), E( * )
.SH PURPOSE
CPTTRS solves a system of linear equations A * X = B with a
Hermitian positive definite tridiagonal matrix A using the
factorization A = U**H*D*U or A = L*D*L**H computed by CPTTRF.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the superdiagonal or the subdiagonal
of the tridiagonal matrix A is stored and the form of the
factorization:
.br
= 'U':  E is the superdiagonal of U, and A = U'*D*U;
.br
= 'L':  E is the subdiagonal of L, and A = L*D*L'.
(The two forms are equivalent if A is real.)
.TP 8
N       (input) INTEGER
The order of the tridiagonal matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
D       (input) REAL array, dimension (N)
The n diagonal elements of the diagonal matrix D from the
factorization computed by CPTTRF.
.TP 8
E       (input) COMPLEX array, dimension (N-1)
The (n-1) off-diagonal elements of the unit bidiagonal
factor U or L from the factorization computed by CPTTRF
(see UPLO).
.TP 8
B       (input/output) REAL array, dimension (LDB,NRHS)
On entry, the right hand side matrix B.
On exit, the solution matrix X.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
