.TH ZLADIV l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH ZLADIV l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
ZLADIV - := X / Y, where X and Y are complex
.SH SYNOPSIS
.TP 15
DOUBLE COMPLEX
FUNCTION ZLADIV( X, Y )
.TP 15
.ti +4
COMPLEX*16
X, Y
.SH PURPOSE
ZLADIV := X / Y, where X and Y are complex.  The computation of X / Y
will not overflow on an intermediary step unless the results
overflows.
.br

.SH ARGUMENTS
.TP 8
X       (input) COMPLEX*16
Y       (input) COMPLEX*16
The complex scalars X and Y.
