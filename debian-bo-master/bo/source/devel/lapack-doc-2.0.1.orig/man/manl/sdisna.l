.TH SDISNA l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH SDISNA l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SDISNA - compute the reciprocal condition numbers for the eigenvectors of a real symmetric or complex Hermitian matrix or for the left or right singular vectors of a general m-by-n matrix
.SH SYNOPSIS
.TP 19
SUBROUTINE SDISNA(
JOB, M, N, D, SEP, INFO )
.TP 19
.ti +4
CHARACTER
JOB
.TP 19
.ti +4
INTEGER
INFO, M, N
.TP 19
.ti +4
REAL
D( * ), SEP( * )
.SH PURPOSE
SDISNA computes the reciprocal condition numbers for the eigenvectors
of a real symmetric or complex Hermitian matrix or for the left or
right singular vectors of a general m-by-n matrix. The reciprocal
condition number is the 'gap' between the corresponding eigenvalue or
singular value and the nearest other one.
.br

The bound on the error, measured by angle in radians, in the I-th
computed vector is given by
.br

       SLAMCH( 'E' ) * ( ANORM / SEP( I ) )
.br

where ANORM = 2-norm(A) = max( abs( D(j) ) ).  SEP(I) is not allowed
to be smaller than SLAMCH( 'E' )*ANORM in order to limit the size of
the error bound.
.br

SDISNA may also be used to compute error bounds for eigenvectors of
the generalized symmetric definite eigenproblem.
.br

.SH ARGUMENTS
.TP 8
JOB     (input) CHARACTER*1
Specifies for which problem the reciprocal condition numbers
should be computed:
.br
= 'E':  the eigenvectors of a symmetric/Hermitian matrix;
.br
= 'L':  the left singular vectors of a general matrix;
.br
= 'R':  the right singular vectors of a general matrix.
.TP 8
M       (input) INTEGER
The number of rows of the matrix. M >= 0.
.TP 8
N       (input) INTEGER
If JOB = 'L' or 'R', the number of columns of the matrix,
in which case N >= 0. Ignored if JOB = 'E'.
.TP 8
D       (input) REAL array, dimension (M) if JOB = 'E'
dimension (min(M,N)) if JOB = 'L' or 'R'
The eigenvalues (if JOB = 'E') or singular values (if JOB =
'L' or 'R') of the matrix, in either increasing or decreasing
order. If singular values, they must be non-negative.
.TP 8
SEP     (output) REAL array, dimension (M) if JOB = 'E'
dimension (min(M,N)) if JOB = 'L' or 'R'
The reciprocal condition numbers of the vectors.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
