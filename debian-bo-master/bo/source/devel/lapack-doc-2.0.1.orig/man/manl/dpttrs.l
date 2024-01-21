.TH DPTTRS l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH DPTTRS l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DPTTRS - solve a system of linear equations A * X = B with a symmetric positive definite tridiagonal matrix A using the factorization A = L*D*L**T or A = U**T*D*U computed by DPTTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE DPTTRS(
N, NRHS, D, E, B, LDB, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDB, N, NRHS
.TP 19
.ti +4
DOUBLE
PRECISION B( LDB, * ), D( * ), E( * )
.SH PURPOSE
DPTTRS solves a system of linear equations A * X = B with a
symmetric positive definite tridiagonal matrix A using the
factorization A = L*D*L**T or A = U**T*D*U computed by DPTTRF.
(The two forms are equivalent if A is real.)
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The order of the tridiagonal matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
D       (input) DOUBLE PRECISION array, dimension (N)
The n diagonal elements of the diagonal matrix D from the
factorization computed by DPTTRF.
.TP 8
E       (input) DOUBLE PRECISION array, dimension (N-1)
The (n-1) off-diagonal elements of the unit bidiagonal factor
U or L from the factorization computed by DPTTRF.
.TP 8
B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
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
