.TH ZLARGV l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH ZLARGV l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
ZLARGV - generate a vector of complex plane rotations with real cosines, determined by elements of the complex vectors x and y
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLARGV(
N, X, INCX, Y, INCY, C, INCC )
.TP 19
.ti +4
INTEGER
INCC, INCX, INCY, N
.TP 19
.ti +4
DOUBLE
PRECISION C( * )
.TP 19
.ti +4
COMPLEX*16
X( * ), Y( * )
.SH PURPOSE
ZLARGV generates a vector of complex plane rotations with real
cosines, determined by elements of the complex vectors x and y.
For i = 1,2,...,n
.br

   (        c(i)   s(i) ) ( x(i) ) = ( a(i) )
.br
   ( -conjg(s(i))  c(i) ) ( y(i) ) = (   0  )
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of plane rotations to be generated.
.TP 8
X       (input/output) COMPLEX*16 array, dimension (1+(N-1)*INCX)
On entry, the vector x.
On exit, x(i) is overwritten by a(i), for i = 1,...,n.
.TP 8
INCX    (input) INTEGER
The increment between elements of X. INCX > 0.
.TP 8
Y       (input/output) COMPLEX*16 array, dimension (1+(N-1)*INCY)
On entry, the vector y.
On exit, the sines of the plane rotations.
.TP 8
INCY    (input) INTEGER
The increment between elements of Y. INCY > 0.
.TP 8
C       (output) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
The cosines of the plane rotations.
.TP 8
INCC    (input) INTEGER
The increment between elements of C. INCC > 0.
