.TH DLANTP l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH DLANTP l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DLANTP - return the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of a triangular matrix A, supplied in packed form
.SH SYNOPSIS
.TP 17
DOUBLE PRECISION
FUNCTION DLANTP( NORM, UPLO, DIAG, N, AP, WORK )
.TP 17
.ti +4
CHARACTER
DIAG, NORM, UPLO
.TP 17
.ti +4
INTEGER
N
.TP 17
.ti +4
DOUBLE
PRECISION AP( * ), WORK( * )
.SH PURPOSE
DLANTP  returns the value of the one norm,  or the Frobenius norm, or
the  infinity norm,  or the  element of  largest absolute value  of a
triangular matrix A, supplied in packed form.

.SH DESCRIPTION
DLANTP returns the value
.br

   DLANTP = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
Specifies the value to be returned in DLANTP as described
above.
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the matrix A is upper or lower triangular.
= 'U':  Upper triangular
.br
= 'L':  Lower triangular
.TP 8
DIAG    (input) CHARACTER*1
Specifies whether or not the matrix A is unit triangular.
= 'N':  Non-unit triangular
.br
= 'U':  Unit triangular
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.  When N = 0, DLANTP is
set to zero.
.TP 8
AP      (input) DOUBLE PRECISION array, dimension (N*(N+1)/2)
The upper or lower triangular matrix A, packed columnwise in
a linear array.  The j-th column of A is stored in the array
AP as follows:
if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
Note that when DIAG = 'U', the elements of the array AP
corresponding to the diagonal elements of the matrix A are
not referenced, but are assumed to be one.
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK),
where LWORK >= N when NORM = 'I'; otherwise, WORK is not
referenced.
