.TH SLASQ4 l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH SLASQ4 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SLASQ4 - SLASQ4 estimates TAU, the smallest eigenvalue of a matrix
.SH SYNOPSIS
.TP 19
SUBROUTINE SLASQ4(
N, Q, E, TAU, SUP )
.TP 19
.ti +4
INTEGER
N
.TP 19
.ti +4
REAL
SUP, TAU
.TP 19
.ti +4
REAL
E( * ), Q( * )
.SH PURPOSE
   SLASQ4 estimates TAU, the smallest eigenvalue of a matrix. This
   routine improves the input value of SUP which is an upper bound
   for the smallest eigenvalue for this matrix .
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
On entry, N specifies the number of rows and columns
in the matrix. N must be at least 0.
.TP 8
Q       (input) REAL array, dimension (N)
Q array
.TP 8
E       (input) REAL array, dimension (N)
E array
.TP 8
TAU     (output) REAL
Estimate of the shift
.TP 8
SUP     (input/output) REAL
Upper bound for the smallest singular value
