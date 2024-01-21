.TH DLASQ3 l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH DLASQ3 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DLASQ3 - DLASQ3 is the workhorse of the whole bidiagonal SVD algorithm
.SH SYNOPSIS
.TP 19
SUBROUTINE DLASQ3(
N, Q, E, QQ, EE, SUP, SIGMA, KEND, OFF, IPHASE,
ICONV, EPS, TOL2, SMALL2 )
.TP 19
.ti +4
INTEGER
ICONV, IPHASE, KEND, N, OFF
.TP 19
.ti +4
DOUBLE
PRECISION EPS, SIGMA, SMALL2, SUP, TOL2
.TP 19
.ti +4
DOUBLE
PRECISION E( * ), EE( * ), Q( * ), QQ( * )
.SH PURPOSE
   DLASQ3 is the workhorse of the whole bidiagonal SVD algorithm.
   This can be described as the differential qd with shifts.

.SH ARGUMENTS
.TP 8
N       (input/output) INTEGER
On entry, N specifies the number of rows and columns
in the matrix. N must be at least 3.
On exit N is non-negative and less than the input value.
.TP 8
Q       (input/output) DOUBLE PRECISION array, dimension (N)
Q array in ping (see IPHASE below)
.TP 8
E       (input/output) DOUBLE PRECISION array, dimension (N)
E array in ping (see IPHASE below)
.TP 8
QQ      (input/output) DOUBLE PRECISION array, dimension (N)
Q array in pong (see IPHASE below)
.TP 8
EE      (input/output) DOUBLE PRECISION array, dimension (N)
E array in pong (see IPHASE below)
.TP 8
SUP     (input/output) DOUBLE PRECISION
Upper bound for the smallest eigenvalue
.TP 8
SIGMA   (input/output) DOUBLE PRECISION
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
EPS     (input) DOUBLE PRECISION
Machine epsilon
.TP 8
TOL2    (input) DOUBLE PRECISION
Square of the relative tolerance TOL as defined in DLASQ1
.TP 8
SMALL2  (input) DOUBLE PRECISION
A threshold value as defined in DLASQ1
