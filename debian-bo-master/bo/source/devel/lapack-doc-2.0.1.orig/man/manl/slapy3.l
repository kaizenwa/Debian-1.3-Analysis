.TH SLAPY3 l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH SLAPY3 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SLAPY3 - return sqrt(x**2+y**2+z**2), taking care not to cause unnecessary overflow
.SH SYNOPSIS
.TP 14
REAL FUNCTION
SLAPY3( X, Y, Z )
.TP 14
.ti +4
REAL
X, Y, Z
.SH PURPOSE
SLAPY3 returns sqrt(x**2+y**2+z**2), taking care not to cause
unnecessary overflow.

.SH ARGUMENTS
.TP 8
X       (input) REAL
Y       (input) REAL
Z       (input) REAL
X, Y and Z specify the values x, y and z.
