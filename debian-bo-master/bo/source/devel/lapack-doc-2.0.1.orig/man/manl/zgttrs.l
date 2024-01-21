.TH ZGTTRS l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH ZGTTRS l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
ZGTTRS - solve one of the systems of equations  A * X = B, A**T * X = B, or A**H * X = B,
.SH SYNOPSIS
.TP 19
SUBROUTINE ZGTTRS(
TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,
INFO )
.TP 19
.ti +4
CHARACTER
TRANS
.TP 19
.ti +4
INTEGER
INFO, LDB, N, NRHS
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
COMPLEX*16
B( LDB, * ), D( * ), DL( * ), DU( * ), DU2( * )
.SH PURPOSE
ZGTTRS solves one of the systems of equations
   A * X = B,  A**T * X = B,  or  A**H * X = B,
with a tridiagonal matrix A using the LU factorization computed
by ZGTTRF.
.br

.SH ARGUMENTS
.TP 8
TRANS   (input) CHARACTER
Specifies the form of the system of equations:
.br
= 'N':  A * X = B     (No transpose)
.br
= 'T':  A**T * X = B  (Transpose)
.br
= 'C':  A**H * X = B  (Conjugate transpose)
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
DL      (input) COMPLEX*16 array, dimension (N-1)
The (n-1) multipliers that define the matrix L from the
LU factorization of A.
.TP 8
D       (input) COMPLEX*16 array, dimension (N)
The n diagonal elements of the upper triangular matrix U from
the LU factorization of A.
.TP 8
DU      (input) COMPLEX*16 array, dimension (N-1)
The (n-1) elements of the first superdiagonal of U.
.TP 8
DU2     (input) COMPLEX*16 array, dimension (N-2)
The (n-2) elements of the second superdiagonal of U.
.TP 8
IPIV    (input) INTEGER array, dimension (N)
The pivot indices; for 1 <= i <= n, row i of the matrix was
interchanged with row IPIV(i).  IPIV(i) will always be either
i or i+1; IPIV(i) = i indicates a row interchange was not
required.
.TP 8
B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
On entry, the right hand side matrix B.
On exit, B is overwritten by the solution matrix X.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
