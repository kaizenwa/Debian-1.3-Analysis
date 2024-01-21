.TH CLANGE l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH CLANGE l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CLANGE - return the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of a complex matrix A
.SH SYNOPSIS
.TP 14
REAL FUNCTION
CLANGE( NORM, M, N, A, LDA, WORK )
.TP 14
.ti +4
CHARACTER
NORM
.TP 14
.ti +4
INTEGER
LDA, M, N
.TP 14
.ti +4
REAL
WORK( * )
.TP 14
.ti +4
COMPLEX
A( LDA, * )
.SH PURPOSE
CLANGE  returns the value of the one norm,  or the Frobenius norm, or
the  infinity norm,  or the  element of  largest absolute value  of a
complex matrix A.

.SH DESCRIPTION
CLANGE returns the value
.br

   CLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
Specifies the value to be returned in CLANGE as described
above.
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.  When M = 0,
CLANGE is set to zero.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.  When N = 0,
CLANGE is set to zero.
.TP 8
A       (input) COMPLEX array, dimension (LDA,N)
The m by n matrix A.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(M,1).
.TP 8
WORK    (workspace) REAL array, dimension (LWORK),
where LWORK >= M when NORM = 'I'; otherwise, WORK is not
referenced.
