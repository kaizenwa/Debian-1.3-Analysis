.TH STPTRI l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH STPTRI l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
STPTRI - compute the inverse of a real upper or lower triangular matrix A stored in packed format
.SH SYNOPSIS
.TP 19
SUBROUTINE STPTRI(
UPLO, DIAG, N, AP, INFO )
.TP 19
.ti +4
CHARACTER
DIAG, UPLO
.TP 19
.ti +4
INTEGER
INFO, N
.TP 19
.ti +4
REAL
AP( * )
.SH PURPOSE
STPTRI computes the inverse of a real upper or lower triangular
matrix A stored in packed format.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= 'U':  A is upper triangular;
.br
= 'L':  A is lower triangular.
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
AP      (input/output) REAL array, dimension (N*(N+1)/2)
On entry, the upper or lower triangular matrix A, stored
columnwise in a linear array.  The j-th column of A is stored
in the array AP as follows:
if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = 'L', AP(i + (j-1)*((2*n-j)/2) = A(i,j) for j<=i<=n.
See below for further details.
On exit, the (triangular) inverse of the original matrix, in
the same packed storage format.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, A(i,i) is exactly zero.  The triangular
matrix is singular and its inverse can not be computed.
.SH FURTHER DETAILS
A triangular matrix A can be transferred to packed storage using one
of the following program segments:
.br

UPLO = 'U':                      UPLO = 'L':
.br

      JC = 1                           JC = 1
.br
      DO 2 J = 1, N                    DO 2 J = 1, N
.br
         DO 1 I = 1, J                    DO 1 I = J, N
.br
            AP(JC+I-1) = A(I,J)              AP(JC+I-J) = A(I,J)
    1    CONTINUE                    1    CONTINUE
.br
         JC = JC + J                      JC = JC + N - J + 1
    2 CONTINUE                       2 CONTINUE
.br

