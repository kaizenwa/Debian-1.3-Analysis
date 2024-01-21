.TH DLAPY2 l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH DLAPY2 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DLAPY2 - return sqrt(x**2+y**2), taking care not to cause unnecessary overflow
.SH SYNOPSIS
.TP 17
DOUBLE PRECISION
FUNCTION DLAPY2( X, Y )
.TP 17
.ti +4
DOUBLE
PRECISION X, Y
.SH PURPOSE
DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
overflow.

.SH ARGUMENTS
.TP 8
X       (input) DOUBLE PRECISION
Y       (input) DOUBLE PRECISION
X and Y specify the values x and y.
