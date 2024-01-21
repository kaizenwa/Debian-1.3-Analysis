.TH DLACPY l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH DLACPY l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DLACPY - copie all or part of a two-dimensional matrix A to another matrix B
.SH SYNOPSIS
.TP 19
SUBROUTINE DLACPY(
UPLO, M, N, A, LDA, B, LDB )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
LDA, LDB, M, N
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), B( LDB, * )
.SH PURPOSE
DLACPY copies all or part of a two-dimensional matrix A to another
matrix B.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies the part of the matrix A to be copied to B.
= 'U':      Upper triangular part
.br
= 'L':      Lower triangular part
.br
Otherwise:  All of the matrix A
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
A       (input) DOUBLE PRECISION array, dimension (LDA,N)
The m by n matrix A.  If UPLO = 'U', only the upper triangle
or trapezoid is accessed; if UPLO = 'L', only the lower
triangle or trapezoid is accessed.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
B       (output) DOUBLE PRECISION array, dimension (LDB,N)
On exit, B = A in the locations specified by UPLO.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,M).
