.TH DLASQ2 l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH DLASQ2 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DLASQ2 - DLASQ2 computes the singular values of a real N-by-N unreduced  bidiagonal matrix with squared diagonal elements in Q and  squared off-diagonal elements in E
.SH SYNOPSIS
.TP 19
SUBROUTINE DLASQ2(
M, Q, E, QQ, EE, EPS, TOL2, SMALL2, SUP, KEND,
INFO )
.TP 19
.ti +4
INTEGER
INFO, KEND, M
.TP 19
.ti +4
DOUBLE
PRECISION EPS, SMALL2, SUP, TOL2
.TP 19
.ti +4
DOUBLE
PRECISION E( * ), EE( * ), Q( * ), QQ( * )
.SH PURPOSE
   DLASQ2 computes the singular values of a real N-by-N unreduced
   bidiagonal matrix with squared diagonal elements in Q and
   squared off-diagonal elements in E. The singular values are
   computed to relative accuracy TOL, barring over/underflow or
   denormalization.
.br

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows and columns in the matrix. M >= 0.
.TP 8
Q       (output) DOUBLE PRECISION array, dimension (M)
On normal exit, contains the squared singular values.
.TP 8
E       (workspace) DOUBLE PRECISION array, dimension (M)
.TP 8
QQ      (input/output) DOUBLE PRECISION array, dimension (M)
On entry, QQ contains the squared diagonal elements of the
bidiagonal matrix whose SVD is desired.
On exit, QQ is overwritten.
.TP 8
EE      (input/output) DOUBLE PRECISION array, dimension (M)
On entry, EE(1:N-1) contains the squared off-diagonal
elements of the bidiagonal matrix whose SVD is desired.
On exit, EE is overwritten.
.TP 8
EPS     (input) DOUBLE PRECISION
Machine epsilon.
.TP 8
TOL2    (input) DOUBLE PRECISION
Desired relative accuracy of computed eigenvalues
as defined in DLASQ1.
.TP 8
SMALL2  (input) DOUBLE PRECISION
A threshold value as defined in DLASQ1.
.TP 8
SUP     (input/output) DOUBLE PRECISION
Upper bound for the smallest eigenvalue.
.TP 8
KEND    (input/output) INTEGER
Index where minimum d occurs.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the algorithm did not converge;  i
specifies how many superdiagonals did not converge.
