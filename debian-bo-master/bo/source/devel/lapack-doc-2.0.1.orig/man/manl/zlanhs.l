.TH ZLANHS l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH ZLANHS l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
ZLANHS - return the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of a Hessenberg matrix A
.SH SYNOPSIS
.TP 17
DOUBLE PRECISION
FUNCTION ZLANHS( NORM, N, A, LDA, WORK )
.TP 17
.ti +4
CHARACTER
NORM
.TP 17
.ti +4
INTEGER
LDA, N
.TP 17
.ti +4
DOUBLE
PRECISION WORK( * )
.TP 17
.ti +4
COMPLEX*16
A( LDA, * )
.SH PURPOSE
ZLANHS  returns the value of the one norm,  or the Frobenius norm, or
the  infinity norm,  or the  element of  largest absolute value  of a
Hessenberg matrix A.

.SH DESCRIPTION
ZLANHS returns the value
.br

   ZLANHS = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
Specifies the value to be returned in ZLANHS as described
above.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.  When N = 0, ZLANHS is
set to zero.
.TP 8
A       (input) COMPLEX*16 array, dimension (LDA,N)
The n by n upper Hessenberg matrix A; the part of A below the
first sub-diagonal is not referenced.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(N,1).
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK),
where LWORK >= N when NORM = 'I'; otherwise, WORK is not
referenced.
