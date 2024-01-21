.TH SLASQ3 l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH SLASQ3 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SLASQ3 - SLASQ3 is the workhorse of the whole bidiagonal SVD algorithm
.SH SYNOPSIS
.TP 19
SUBROUTINE SLASQ3(
N, Q, E, QQ, EE, SUP, SIGMA, KEND, OFF, IPHASE,
ICONV, EPS, TOL2, SMALL2 )
.TP 19
.ti +4
INTEGER
ICONV, IPHASE, KEND, N, OFF
.TP 19
.ti +4
REAL
EPS, SIGMA, SMALL2, SUP, TOL2
.TP 19
.ti +4
REAL
E( * ), EE( * ), Q( * ), QQ( * )
.SH PURPOSE
   SLASQ3 is the workhorse of the whole bidiagonal SVD algorithm.
   This can be described as the differential qd with shifts.

.SH ARGUMENTS
.TP 8
N       (input/output) INTEGER
On entry, N specifies the number of rows and columns
in the matrix. N must be at least 3.
On exit N is non-negative and less than the input value.
.TP 8
Q       (input/output) REAL array, dimension (N)
Q array in ping (see IPHASE below)
.TP 8
E       (input/output) REAL array, dimension (N)
E array in ping (see IPHASE below)
.TP 8
QQ      (input/output) REAL array, dimension (N)
Q array in pong (see IPHASE below)
.TP 8
EE      (input/output) REAL array, dimension (N)
E array in pong (see IPHASE below)
.TP 8
SUP     (input/output) REAL
Upper bound for the smallest eigenvalue
.TP 8
SIGMA   (input/output) REAL
Accumulated shift for the present submatrix
.TP 8
KEND    (input/output) INTEGER
Index where minimum D(i) occurs in recurrence for
splitting criterion
.TP 8
OFF     (input/output) INTEGER
Offset for arrays
.TP 8
IPHASE  (input/output) INTEGER
If IPHASE = 1 (ping) then data is in Q and E arrays
If IPHASE = 2 (pong) then data is in QQ and EE arrays
.TP 8
ICONV   (input) INTEGER
If ICONV = 0 a bottom part of a matrix (with a split)
If ICONV =-3 a top part of a matrix (with a split)
.TP 8
EPS     (input) REAL
Machine epsilon
.TP 8
TOL2    (input) REAL
Square of the relative tolerance TOL as defined in SLASQ1
.TP 8
SMALL2  (input) REAL
A threshold value as defined in SLASQ1
