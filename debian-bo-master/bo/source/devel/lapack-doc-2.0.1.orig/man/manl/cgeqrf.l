.TH CGEQRF l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH CGEQRF l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CGEQRF - compute a QR factorization of a complex M-by-N matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE CGEQRF(
M, N, A, LDA, TAU, WORK, LWORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, LWORK, M, N
.TP 19
.ti +4
COMPLEX
A( LDA, * ), TAU( * ), WORK( LWORK )
.SH PURPOSE
CGEQRF computes a QR factorization of a complex M-by-N matrix A:
A = Q * R.
.br

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  N >= 0.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit, the elements on and above the diagonal of the array
contain the min(M,N)-by-N upper trapezoidal matrix R (R is
upper triangular if m >= n); the elements below the diagonal,
with the array TAU, represent the unitary matrix Q as a
product of min(m,n) elementary reflectors (see Further
Details).
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
TAU     (output) COMPLEX array, dimension (min(M,N))
The scalar factors of the elementary reflectors (see Further
Details).
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,N).
For optimum performance LWORK >= N*NB, where NB is
the optimal blocksize.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
The matrix Q is represented as a product of elementary reflectors

   Q = H(1) H(2) . . . H(k), where k = min(m,n).
.br

Each H(i) has the form
.br

   H(i) = I - tau * v * v'
.br

where tau is a complex scalar, and v is a complex vector with
v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
and tau in TAU(i).
.br

