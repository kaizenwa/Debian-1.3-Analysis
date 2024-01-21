.TH DTRTI2 l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH DTRTI2 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DTRTI2 - compute the inverse of a real upper or lower triangular matrix
.SH SYNOPSIS
.TP 19
SUBROUTINE DTRTI2(
UPLO, DIAG, N, A, LDA, INFO )
.TP 19
.ti +4
CHARACTER
DIAG, UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, N
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * )
.SH PURPOSE
DTRTI2 computes the inverse of a real upper or lower triangular
matrix.

This is the Level 2 BLAS version of the algorithm.
.br

.SH ARGUMENTS
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
The order of the matrix A.  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the triangular matrix A.  If UPLO = 'U', the
leading n by n upper triangular part of the array A contains
the upper triangular matrix, and the strictly lower
triangular part of A is not referenced.  If UPLO = 'L', the
leading n by n lower triangular part of the array A contains
the lower triangular matrix, and the strictly upper
triangular part of A is not referenced.  If DIAG = 'U', the
diagonal elements of A are also not referenced and are
assumed to be 1.

On exit, the (triangular) inverse of the original matrix, in
the same storage format.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -k, the k-th argument had an illegal value
