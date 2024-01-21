.TH CTRCON l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH CTRCON l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CTRCON - estimate the reciprocal of the condition number of a triangular matrix A, in either the 1-norm or the infinity-norm
.SH SYNOPSIS
.TP 19
SUBROUTINE CTRCON(
NORM, UPLO, DIAG, N, A, LDA, RCOND, WORK,
RWORK, INFO )
.TP 19
.ti +4
CHARACTER
DIAG, NORM, UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, N
.TP 19
.ti +4
REAL
RCOND
.TP 19
.ti +4
REAL
RWORK( * )
.TP 19
.ti +4
COMPLEX
A( LDA, * ), WORK( * )
.SH PURPOSE
CTRCON estimates the reciprocal of the condition number of a
triangular matrix A, in either the 1-norm or the infinity-norm.

The norm of A is computed and an estimate is obtained for
norm(inv(A)), then the reciprocal of the condition number is
computed as
.br
   RCOND = 1 / ( norm(A) * norm(inv(A)) ).
.br

.SH ARGUMENTS
.TP 8
NORM    (input) CHARACTER*1
Specifies whether the 1-norm condition number or the
infinity-norm condition number is required:
.br
= '1' or 'O':  1-norm;
.br
= 'I':         Infinity-norm.
.TP 8
UPLO    (input) CHARACTER*1
.br
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
A       (input) COMPLEX array, dimension (LDA,N)
The triangular matrix A.  If UPLO = 'U', the leading N-by-N
upper triangular part of the array A contains the upper
triangular matrix, and the strictly lower triangular part of
A is not referenced.  If UPLO = 'L', the leading N-by-N lower
triangular part of the array A contains the lower triangular
matrix, and the strictly upper triangular part of A is not
referenced.  If DIAG = 'U', the diagonal elements of A are
also not referenced and are assumed to be 1.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
RCOND   (output) REAL
The reciprocal of the condition number of the matrix A,
computed as RCOND = 1/(norm(A) * norm(inv(A))).
.TP 8
WORK    (workspace) COMPLEX array, dimension (2*N)
.TP 8
RWORK   (workspace) REAL array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
