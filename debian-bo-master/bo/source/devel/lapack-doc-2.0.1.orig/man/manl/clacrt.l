.TH CLACRT l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH CLACRT l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CLACRT - applie a plane rotation, where the cos and sin (C and S) are complex and the vectors CX and CY are complex
.SH SYNOPSIS
.TP 19
SUBROUTINE CLACRT(
N, CX, INCX, CY, INCY, C, S )
.TP 19
.ti +4
INTEGER
INCX, INCY, N
.TP 19
.ti +4
COMPLEX
C, S
.TP 19
.ti +4
COMPLEX
CX( * ), CY( * )
.SH PURPOSE
CLACRT applies a plane rotation, where the cos and sin (C and S) are
complex and the vectors CX and CY are complex.

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of elements in the vectors CX and CY.
.TP 8
CX      (input/output) COMPLEX array, dimension (N)
On input, the vector X.
On output, CX is overwritten with C*X + S*Y.
.TP 8
INCX    (input) INTEGER
The increment between successive values of CY.  INCX <> 0.
.TP 8
CY      (input/output) COMPLEX array, dimension (N)
On input, the vector Y.
On output, CY is overwritten with -S*X + C*Y.
.TP 8
INCY    (input) INTEGER
The increment between successive values of CY.  INCX <> 0.
.TP 8
C       (input) COMPLEX
S       (input) COMPLEX
C and S define a complex rotation
[  C   S  ]
[ -S   C  ]
where C*C + S*S = 1.0.
