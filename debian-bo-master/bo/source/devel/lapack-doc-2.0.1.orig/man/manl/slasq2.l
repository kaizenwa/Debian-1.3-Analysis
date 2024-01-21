.TH SLASQ2 l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH SLASQ2 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SLASQ2 - SLASQ2 computes the singular values of a real N-by-N unreduced  bidiagonal matrix with squared diagonal elements in Q and  squared off-diagonal elements in E
.SH SYNOPSIS
.TP 19
SUBROUTINE SLASQ2(
M, Q, E, QQ, EE, EPS, TOL2, SMALL2, SUP, KEND,
INFO )
.TP 19
.ti +4
INTEGER
INFO, KEND, M
.TP 19
.ti +4
REAL
EPS, SMALL2, SUP, TOL2
.TP 19
.ti +4
REAL
E( * ), EE( * ), Q( * ), QQ( * )
.SH PURPOSE
   SLASQ2 computes the singular values of a real N-by-N unreduced
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
Q       (output) REAL array, dimension (M)
On normal exit, contains the squared singular values.
.TP 8
E       (workspace) REAL array, dimension (M)
.TP 8
QQ      (input/output) REAL array, dimension (M)
On entry, QQ contains the squared diagonal elements of the
bidiagonal matrix whose SVD is desired.
On exit, QQ is overwritten.
.TP 8
EE      (input/output) REAL array, dimension (M)
On entry, EE(1:N-1) contains the squared off-diagonal
elements of the bidiagonal matrix whose SVD is desired.
On exit, EE is overwritten.
.TP 8
EPS     (input) REAL
Machine epsilon.
.TP 8
TOL2    (input) REAL
Desired relative accuracy of computed eigenvalues
as defined in SLASQ1.
.TP 8
SMALL2  (input) REAL
A threshold value as defined in SLASQ1.
.TP 8
SUP     (input/output) REAL
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
