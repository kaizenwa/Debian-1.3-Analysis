.TH DGECON l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH DGECON l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DGECON - estimate the reciprocal of the condition number of a general real matrix A, in either the 1-norm or the infinity-norm, using the LU factorization computed by DGETRF
.SH SYNOPSIS
.TP 19
SUBROUTINE DGECON(
NORM, N, A, LDA, ANORM, RCOND, WORK, IWORK,
INFO )
.TP 19
.ti +4
CHARACTER
NORM
.TP 19
.ti +4
INTEGER
INFO, LDA, N
.TP 19
.ti +4
DOUBLE
PRECISION ANORM, RCOND
.TP 19
.ti +4
INTEGER
IWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), WORK( * )
.SH PURPOSE
DGECON estimates the reciprocal of the condition number of a general
real matrix A, in either the 1-norm or the infinity-norm, using
the LU factorization computed by DGETRF.

An estimate is obtained for norm(inv(A)), and the reciprocal of the
condition number is computed as
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
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
A       (input) DOUBLE PRECISION array, dimension (LDA,N)
The factors L and U from the factorization A = P*L*U
as computed by DGETRF.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
ANORM   (input) DOUBLE PRECISION
If NORM = '1' or 'O', the 1-norm of the original matrix A.
If NORM = 'I', the infinity-norm of the original matrix A.
.TP 8
RCOND   (output) DOUBLE PRECISION
The reciprocal of the condition number of the matrix A,
computed as RCOND = 1/(norm(A) * norm(inv(A))).
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (4*N)
.TP 8
IWORK   (workspace) INTEGER array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
