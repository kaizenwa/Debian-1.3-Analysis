.TH CUNGBR l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH CUNGBR l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CUNGBR - generate one of the complex unitary matrices Q or P**H determined by CGEBRD when reducing a complex matrix A to bidiagonal form
.SH SYNOPSIS
.TP 19
SUBROUTINE CUNGBR(
VECT, M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
.TP 19
.ti +4
CHARACTER
VECT
.TP 19
.ti +4
INTEGER
INFO, K, LDA, LWORK, M, N
.TP 19
.ti +4
COMPLEX
A( LDA, * ), TAU( * ), WORK( LWORK )
.SH PURPOSE
CUNGBR generates one of the complex unitary matrices Q or P**H
determined by CGEBRD when reducing a complex matrix A to bidiagonal
form: A = Q * B * P**H.  Q and P**H are defined as products of
elementary reflectors H(i) or G(i) respectively.
.br

If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
is of order M:
.br
if m >= k, Q = H(1) H(2) . . . H(k) and CUNGBR returns the first n
columns of Q, where m >= n >= k;
.br
if m < k, Q = H(1) H(2) . . . H(m-1) and CUNGBR returns Q as an
M-by-M matrix.
.br

If VECT = 'P', A is assumed to have been a K-by-N matrix, and P**H
is of order N:
.br
if k < n, P**H = G(k) . . . G(2) G(1) and CUNGBR returns the first m
rows of P**H, where n >= m >= k;
.br
if k >= n, P**H = G(n-1) . . . G(2) G(1) and CUNGBR returns P**H as
an N-by-N matrix.
.br

.SH ARGUMENTS
.TP 8
VECT    (input) CHARACTER*1
Specifies whether the matrix Q or the matrix P**H is
required, as defined in the transformation applied by CGEBRD:
.br
= 'Q':  generate Q;
.br
= 'P':  generate P**H.
.TP 8
M       (input) INTEGER
The number of rows of the matrix Q or P**H to be returned.
M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix Q or P**H to be returned.
N >= 0.
If VECT = 'Q', M >= N >= min(M,K);
if VECT = 'P', N >= M >= min(N,K).
.TP 8
K       (input) INTEGER
If VECT = 'Q', the number of columns in the original M-by-K
matrix reduced by CGEBRD.
If VECT = 'P', the number of rows in the original K-by-N
matrix reduced by CGEBRD.
K >= 0.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the vectors which define the elementary reflectors,
as returned by CGEBRD.
On exit, the M-by-N matrix Q or P**H.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= M.
.TP 8
TAU     (input) COMPLEX array, dimension
(min(M,K)) if VECT = 'Q'
(min(N,K)) if VECT = 'P'
TAU(i) must contain the scalar factor of the elementary
reflector H(i) or G(i), which determines Q or P**H, as
returned by CGEBRD in its array argument TAUQ or TAUP.
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= max(1,min(M,N)).
For optimum performance LWORK >= min(M,N)*NB, where NB
is the optimal blocksize.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
