.TH CLADIV l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH CLADIV l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CLADIV - := X / Y, where X and Y are complex
.SH SYNOPSIS
.TP 17
COMPLEX FUNCTION
CLADIV( X, Y )
.TP 17
.ti +4
COMPLEX
X, Y
.SH PURPOSE
CLADIV := X / Y, where X and Y are complex.  The computation of X / Y
will not overflow on an intermediary step unless the results
overflows.
.br

.SH ARGUMENTS
.TP 8
X       (input) COMPLEX
Y       (input) COMPLEX
The complex scalars X and Y.
