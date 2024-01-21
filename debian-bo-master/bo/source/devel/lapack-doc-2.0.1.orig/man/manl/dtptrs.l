.TH DTPTRS l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH DTPTRS l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DTPTRS - solve a triangular system of the form   A * X = B or A**T * X = B,
.SH SYNOPSIS
.TP 19
SUBROUTINE DTPTRS(
UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, INFO )
.TP 19
.ti +4
CHARACTER
DIAG, TRANS, UPLO
.TP 19
.ti +4
INTEGER
INFO, LDB, N, NRHS
.TP 19
.ti +4
DOUBLE
PRECISION AP( * ), B( LDB, * )
.SH PURPOSE
DTPTRS solves a triangular system of the form

where A is a triangular matrix of order N stored in packed format,
and B is an N-by-NRHS matrix.  A check is made to verify that A is
nonsingular.
.br

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= 'U':  A is upper triangular;
.br
= 'L':  A is lower triangular.
.TP 8
TRANS   (input) CHARACTER*1
.br
Specifies the form of the system of equations:
.br
= 'N':  A * X = B  (No transpose)
.br
= 'T':  A**T * X = B  (Transpose)
.br
= 'C':  A**H * X = B  (Conjugate transpose = Transpose)
.TP 8
DIAG    (input) CHARACTER*1
.br
= 'N':  A is non-unit triangular;
.br
= 'U':  A is unit triangular.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
AP      (input) DOUBLE PRECISION array, dimension (N*(N+1)/2)
The upper or lower triangular matrix A, packed columnwise in
a linear array.  The j-th column of A is stored in the array
AP as follows:
if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = 'L', AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n.
.TP 8
B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
On entry, the right hand side matrix B.
On exit, if INFO = 0, the solution matrix X.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the i-th diagonal element of A is zero,
indicating that the matrix is singular and the
solutions X have not been computed.
