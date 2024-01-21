.TH DLASQ1 l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH DLASQ1 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DLASQ1 - DLASQ1 computes the singular values of a real N-by-N bidiagonal  matrix with diagonal D and off-diagonal E
.SH SYNOPSIS
.TP 19
SUBROUTINE DLASQ1(
N, D, E, WORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, N
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), E( * ), WORK( * )
.SH PURPOSE
   DLASQ1 computes the singular values of a real N-by-N bidiagonal
   matrix with diagonal D and off-diagonal E. The singular values are
   computed to high relative accuracy, barring over/underflow or
   denormalization. The algorithm is described in
.br

   "Accurate singular values and differential qd algorithms," by
   K. V. Fernando and B. N. Parlett,
.br
   Numer. Math., Vol-67, No. 2, pp. 191-230,1994.
.br

   See also
.br
   "Implementation of differential qd algorithms," by
.br
   K. V. Fernando and B. N. Parlett, Technical Report,
.br
   Department of Mathematics, University of California at Berkeley,
   1994 (Under preparation).
.br

.SH ARGUMENTS
.TP 8
N       (input) INTEGER
The number of rows and columns in the matrix. N >= 0.
.TP 8
D       (input/output) DOUBLE PRECISION array, dimension (N)
On entry, D contains the diagonal elements of the
bidiagonal matrix whose SVD is desired. On normal exit,
D contains the singular values in decreasing order.
.TP 8
E       (input/output) DOUBLE PRECISION array, dimension (N)
On entry, elements E(1:N-1) contain the off-diagonal elements
of the bidiagonal matrix whose SVD is desired.
On exit, E is overwritten.
.TP 8
WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, the algorithm did not converge;  i
specifies how many superdiagonals did not converge.
