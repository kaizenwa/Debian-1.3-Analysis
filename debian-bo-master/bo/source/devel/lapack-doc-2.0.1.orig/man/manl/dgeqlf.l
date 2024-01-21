.TH DGEQLF l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH DGEQLF l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DGEQLF - compute a QL factorization of a real M-by-N matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE DGEQLF(
M, N, A, LDA, TAU, WORK, LWORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, LWORK, M, N
.TP 19
.ti +4
DOUBLE
PRECISION A( LDA, * ), TAU( * ), WORK( LWORK )
.SH PURPOSE
DGEQLF computes a QL factorization of a real M-by-N matrix A:
A = Q * L.
.br

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit,
if m >= n, the lower triangle of the subarray
A(m-n+1:m,1:n) contains the N-by-N lower triangular matrix L;
if m <= n, the elements on and below the (n-m)-th
superdiagonal contain the M-by-N lower trapezoidal matrix L;
the remaining elements, with the array TAU, represent the
orthogonal matrix Q as a product of elementary reflectors
(see Further Details).
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
The scalar factors of the elementary reflectors (see Further
Details).
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,N).
For optimum performance LWORK >= N*NB, where NB is the
optimal blocksize.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
The matrix Q is represented as a product of elementary reflectors

   Q = H(k) . . . H(2) H(1), where k = min(m,n).
.br

Each H(i) has the form
.br

   H(i) = I - tau * v * v'
.br

where tau is a real scalar, and v is a real vector with
.br
v(m-k+i+1:m) = 0 and v(m-k+i) = 1; v(1:m-k+i-1) is stored on exit in
A(1:m-k+i-1,n-k+i), and tau in TAU(i).
.br

