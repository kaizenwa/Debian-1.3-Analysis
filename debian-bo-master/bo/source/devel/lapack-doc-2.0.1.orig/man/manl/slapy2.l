.TH SLAPY2 l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH SLAPY2 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SLAPY2 - return sqrt(x**2+y**2), taking care not to cause unnecessary overflow
.SH SYNOPSIS
.TP 14
REAL FUNCTION
SLAPY2( X, Y )
.TP 14
.ti +4
REAL
X, Y
.SH PURPOSE
SLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
overflow.

.SH ARGUMENTS
.TP 8
X       (input) REAL
Y       (input) REAL
X and Y specify the values x and y.
