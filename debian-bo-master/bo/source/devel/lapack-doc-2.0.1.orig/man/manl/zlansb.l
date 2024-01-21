.TH ZLANSB l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH ZLANSB l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
ZLANSB - return the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of an n by n symmetric band matrix A, with k super-diagonals
.SH SYNOPSIS
.TP 17
DOUBLE PRECISION
FUNCTION ZLANSB( NORM, UPLO, N, K, AB, LDAB,
WORK )
.TP 17
.ti +4
CHARACTER
NORM, UPLO
.TP 17
.ti +4
INTEGER
K, LDAB, N
.TP 17
.ti +4
DOUBLE
PRECISION WORK( * )
.TP 17
.ti +4
COMPLEX*16
AB( LDAB, * )
.SH PURPOSE
ZLANSB  returns the value of the one norm,  or the Frobenius norm, or
the  infinity norm,  or the element of  largest absolute value  of an
n by n symmetric band matrix A,  with k super-diagonals.

.SH DESCRIPTION
ZLANSB returns the value
.br

   ZLANSB = ( max(abs(A(i,j))), NORM = 'M' or 'm'
.br
            (
.br
            ( norm1(A),         NORM = '1', 'O' or 'o'
.br
            (
.br
            ( normI(A),         NORM = 'I' or 'i'
.br
            (
.br
            ( normF(A),         NORM = 'F', 'f', 'E' or 'e'

where  norm1  denotes the  one norm of a matrix (maximum column sum),
normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
normF  denotes the  Frobenius norm of a matrix (square root of sum of
squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm.

.SH ARGUMENTS
.TP 8
NORM    (input) CHARACTER*1
Specifies the value to be returned in ZLANSB as described
above.
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the upper or lower triangular part of the
band matrix A is supplied.
= 'U':  Upper triangular part is supplied
.br
= 'L':  Lower triangular part is supplied
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.  When N = 0, ZLANSB is
set to zero.
.TP 8
K       (input) INTEGER
The number of super-diagonals or sub-diagonals of the
band matrix A.  K >= 0.
.TP 8
AB      (input) COMPLEX*16 array, dimension (LDAB,N)
The upper or lower triangle of the symmetric band matrix A,
stored in the first K+1 rows of AB.  The j-th column of A is
stored in the j-th column of the array AB as follows:
if UPLO = 'U', AB(k+1+i-j,j) = A(i,j) for max(1,j-k)<=i<=j;
if UPLO = 'L', AB(1+i-j,j)   = A(i,j) for j<=i<=min(n,j+k).
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= K+1.
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK),
where LWORK >= N when NORM = 'I' or '1' or 'O'; otherwise,
WORK is not referenced.
