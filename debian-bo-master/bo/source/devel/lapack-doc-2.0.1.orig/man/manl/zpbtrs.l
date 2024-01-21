.TH ZPBTRS l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH ZPBTRS l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
ZPBTRS - solve a system of linear equations A*X = B with a Hermitian positive definite band matrix A using the Cholesky factorization A = U**H*U or A = L*L**H computed by ZPBTRF
.SH SYNOPSIS
.TP 19
SUBROUTINE ZPBTRS(
UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, KD, LDAB, LDB, N, NRHS
.TP 19
.ti +4
COMPLEX*16
AB( LDAB, * ), B( LDB, * )
.SH PURPOSE
ZPBTRS solves a system of linear equations A*X = B with a Hermitian
positive definite band matrix A using the Cholesky factorization
A = U**H*U or A = L*L**H computed by ZPBTRF.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= 'U':  Upper triangular factor stored in AB;
.br
= 'L':  Lower triangular factor stored in AB.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
KD      (input) INTEGER
The number of superdiagonals of the matrix A if UPLO = 'U',
or the number of subdiagonals if UPLO = 'L'.  KD >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrix B.  NRHS >= 0.
.TP 8
AB      (input) COMPLEX*16 array, dimension (LDAB,N)
The triangular factor U or L from the Cholesky factorization
A = U**H*U or A = L*L**H of the band matrix A, stored in the
first KD+1 rows of the array.  The j-th column of U or L is
stored in the j-th column of the array AB as follows:
if UPLO ='U', AB(kd+1+i-j,j) = U(i,j) for max(1,j-kd)<=i<=j;
if UPLO ='L', AB(1+i-j,j)    = L(i,j) for j<=i<=min(n,j+kd).
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= KD+1.
.TP 8
B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
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
