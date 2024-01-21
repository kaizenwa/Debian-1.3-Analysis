.TH CGEQPF l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH CGEQPF l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CGEQPF - compute a QR factorization with column pivoting of a complex M-by-N matrix A
.SH SYNOPSIS
.TP 19
SUBROUTINE CGEQPF(
M, N, A, LDA, JPVT, TAU, WORK, RWORK, INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, M, N
.TP 19
.ti +4
INTEGER
JPVT( * )
.TP 19
.ti +4
REAL
RWORK( * )
.TP 19
.ti +4
COMPLEX
A( LDA, * ), TAU( * ), WORK( * )
.SH PURPOSE
CGEQPF computes a QR factorization with column pivoting of a
complex M-by-N matrix A: A*P = Q*R.

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A. M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A. N >= 0
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit, the upper triangle of the array contains the
min(M,N)-by-N upper triangular matrix R; the elements
below the diagonal, together with the array TAU,
represent the orthogonal matrix Q as a product of
min(m,n) elementary reflectors.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,M).
.TP 8
JPVT    (input/output) INTEGER array, dimension (N)
On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
to the front of A*P (a leading column); if JPVT(i) = 0,
the i-th column of A is a free column.
On exit, if JPVT(i) = k, then the i-th column of A*P
was the k-th column of A.
.TP 8
TAU     (output) COMPLEX array, dimension (min(M,N))
The scalar factors of the elementary reflectors.
.TP 8
WORK    (workspace) COMPLEX array, dimension (N)
.TP 8
RWORK   (workspace) REAL array, dimension (2*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
The matrix Q is represented as a product of elementary reflectors

   Q = H(1) H(2) . . . H(n)
.br

Each H(i) has the form
.br

   H = I - tau * v * v'
.br

where tau is a complex scalar, and v is a complex vector with
v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i).

The matrix P is represented in jpvt as follows: If
.br
   jpvt(j) = i
.br
then the jth column of P is the ith canonical unit vector.

