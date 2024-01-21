.TH ICMAX1 l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH ICMAX1 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
ICMAX1 - find the index of the element whose real part has maximum absolute value
.SH SYNOPSIS
.TP 17
INTEGER FUNCTION
ICMAX1( N, CX, INCX )
.TP 17
.ti +4
INTEGER
INCX, N
.TP 17
.ti +4
COMPLEX
CX( * )
.SH PURPOSE
ICMAX1 finds the index of the element whose real part has maximum
absolute value.

Based on ICAMAX from Level 1 BLAS.
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
The spacing between successive values of CX.  INCX >= 1.
