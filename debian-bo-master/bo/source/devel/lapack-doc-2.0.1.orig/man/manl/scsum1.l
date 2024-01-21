.TH SCSUM1 l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH SCSUM1 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SCSUM1 - take the sum of the absolute values of a complex vector and returns a single precision result
.SH SYNOPSIS
.TP 14
REAL FUNCTION
SCSUM1( N, CX, INCX )
.TP 14
.ti +4
INTEGER
INCX, N
.TP 14
.ti +4
COMPLEX
CX( * )
.SH PURPOSE
SCSUM1 takes the sum of the absolute values of a complex
vector and returns a single precision result.

Based on SCASUM from the Level 1 BLAS.
.br
The change is to use the 'genuine' absolute value.
.br

Contributed by Nick Higham for use with CLACON.
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of elements in the vector CX.
.TP 8
CX      (input) COMPLEX array, dimension (N)
The vector whose elements will be summed.
.TP 8
INCX    (input) INTEGER
The spacing between successive values of CX.  INCX > 0.
