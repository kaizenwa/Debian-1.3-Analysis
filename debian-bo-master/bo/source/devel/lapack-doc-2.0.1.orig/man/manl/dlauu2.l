.TH DLAUU2 l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH DLAUU2 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DLAUU2 - compute the product U * U' or L' * L, where the triangular factor U or L is stored in the upper or lower triangular part of the array A
.SH SYNOPSIS
.TP 19
SUBROUTINE DLAUU2(
UPLO, N, A, LDA, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, N
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * )
.SH PURPOSE
DLAUU2 computes the product U * U' or L' * L, where the triangular
factor U or L is stored in the upper or lower triangular part of
the array A.

If UPLO = 'U' or 'u' then the upper triangle of the result is stored,
overwriting the factor U in A.
.br
If UPLO = 'L' or 'l' then the lower triangle of the result is stored,
overwriting the factor L in A.
.br

This is the unblocked form of the algorithm, calling Level 2 BLAS.

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
Specifies whether the triangular factor stored in the array A
is upper or lower triangular:
.br
= 'U':  Upper triangular
.br
= 'L':  Lower triangular
.TP 8
N       (input) INTEGER
The order of the triangular factor U or L.  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the triangular factor U or L.
On exit, if UPLO = 'U', the upper triangle of A is
overwritten with the upper triangle of the product U * U';
if UPLO = 'L', the lower triangle of A is overwritten with
the lower triangle of the product L' * L.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -k, the k-th argument had an illegal value
