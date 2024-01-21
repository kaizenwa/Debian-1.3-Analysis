.TH SLANSB l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH SLANSB l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SLANSB - return the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of an n by n symmetric band matrix A, with k super-diagonals
.SH SYNOPSIS
.TP 14
REAL FUNCTION
SLANSB( NORM, UPLO, N, K, AB, LDAB,
WORK )
.TP 14
.ti +4
CHARACTER
NORM, UPLO
.TP 14
.ti +4
INTEGER
K, LDAB, N
.TP 14
.ti +4
REAL
AB( LDAB, * ), WORK( * )
.SH PURPOSE
SLANSB  returns the value of the one norm,  or the Frobenius norm, or
the  infinity norm,  or the element of  largest absolute value  of an
n by n symmetric band matrix A,  with k super-diagonals.

.SH DESCRIPTION
SLANSB returns the value
.br

   SLANSB = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
Specifies the value to be returned in SLANSB as described
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
The order of the matrix A.  N >= 0.  When N = 0, SLANSB is
set to zero.
.TP 8
K       (input) INTEGER
The number of super-diagonals or sub-diagonals of the
band matrix A.  K >= 0.
.TP 8
AB      (input) REAL array, dimension (LDAB,N)
The upper or lower triangle of the symmetric band matrix A,
stored in the first K+1 rows of AB.  The j-th column of A is
stored in the j-th column of the array AB as follows:
if UPLO = 'U', AB(k+1+i-j,j) = A(i,j) for max(1,j-k)<=i<=j;
if UPLO = 'L', AB(1+i-j,j)   = A(i,j) for j<=i<=min(n,j+k).
.TP 8
LDAB    (input) INTEGER
The leading dimension of the array AB.  LDAB >= K+1.
.TP 8
WORK    (workspace) REAL array, dimension (LWORK),
where LWORK >= N when NORM = 'I' or '1' or 'O'; otherwise,
WORK is not referenced.
