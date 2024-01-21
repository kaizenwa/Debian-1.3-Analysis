.TH SLARFX l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH SLARFX l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SLARFX - applie a real elementary reflector H to a real m by n matrix C, from either the left or the right
.SH SYNOPSIS
.TP 19
SUBROUTINE SLARFX(
SIDE, M, N, V, TAU, C, LDC, WORK )
.TP 19
.ti +4
CHARACTER
SIDE
.TP 19
.ti +4
INTEGER
LDC, M, N
.TP 19
.ti +4
REAL
TAU
.TP 19
.ti +4
REAL
C( LDC, * ), V( * ), WORK( * )
.SH PURPOSE
SLARFX applies a real elementary reflector H to a real m by n
matrix C, from either the left or the right. H is represented in the
form
.br

      H = I - tau * v * v'
.br

where tau is a real scalar and v is a real vector.
.br

If tau = 0, then H is taken to be the unit matrix
.br

This version uses inline code if H has order < 11.
.br

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
= 'L': form  H * C
.br
= 'R': form  C * H
.TP 8
M       (input) INTEGER
The number of rows of the matrix C.
.TP 8
N       (input) INTEGER
The number of columns of the matrix C.
.TP 8
V       (input) REAL array, dimension (M) if SIDE = 'L'
or (N) if SIDE = 'R'
The vector v in the representation of H.
.TP 8
TAU     (input) REAL
The value tau in the representation of H.
.TP 8
C       (input/output) REAL array, dimension (LDC,N)
On entry, the m by n matrix C.
On exit, C is overwritten by the matrix H * C if SIDE = 'L',
or C * H if SIDE = 'R'.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDA >= (1,M).
.TP 8
WORK    (workspace) REAL array, dimension
(N) if SIDE = 'L'
or (M) if SIDE = 'R'
WORK is not referenced if H has order < 11.
