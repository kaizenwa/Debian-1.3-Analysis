.TH CUNMBR l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH CUNMBR l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CUNMBR - VECT = 'Q', CUNMBR overwrites the general complex M-by-N matrix C with  SIDE = 'L' SIDE = 'R' TRANS = 'N'
.SH SYNOPSIS
.TP 19
SUBROUTINE CUNMBR(
VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C,
LDC, WORK, LWORK, INFO )
.TP 19
.ti +4
CHARACTER
SIDE, TRANS, VECT
.TP 19
.ti +4
INTEGER
INFO, K, LDA, LDC, LWORK, M, N
.TP 19
.ti +4
COMPLEX
A( LDA, * ), C( LDC, * ), TAU( * ),
WORK( LWORK )
.SH PURPOSE
If VECT = 'Q', CUNMBR overwrites the general complex M-by-N matrix C
with
                SIDE = 'L'     SIDE = 'R'
TRANS = 'N':      Q * C          C * Q
TRANS = 'C':      Q**H * C       C * Q**H
.br

If VECT = 'P', CUNMBR overwrites the general complex M-by-N matrix C
with
.br
                SIDE = 'L'     SIDE = 'R'
.br
TRANS = 'N':      P * C          C * P
.br
TRANS = 'C':      P**H * C       C * P**H
.br

Here Q and P**H are the unitary matrices determined by CGEBRD when
reducing a complex matrix A to bidiagonal form: A = Q * B * P**H. Q
and P**H are defined as products of elementary reflectors H(i) and
G(i) respectively.
.br

Let nq = m if SIDE = 'L' and nq = n if SIDE = 'R'. Thus nq is the
order of the unitary matrix Q or P**H that is applied.
.br

If VECT = 'Q', A is assumed to have been an NQ-by-K matrix:
if nq >= k, Q = H(1) H(2) . . . H(k);
.br
if nq < k, Q = H(1) H(2) . . . H(nq-1).
.br

If VECT = 'P', A is assumed to have been a K-by-NQ matrix:
if k < nq, P = G(1) G(2) . . . G(k);
.br
if k >= nq, P = G(1) G(2) . . . G(nq-1).
.br

.SH ARGUMENTS
.TP 8
VECT    (input) CHARACTER*1
= 'Q': apply Q or Q**H;
.br
= 'P': apply P or P**H.
.TP 8
SIDE    (input) CHARACTER*1
.br
= 'L': apply Q, Q**H, P or P**H from the Left;
.br
= 'R': apply Q, Q**H, P or P**H from the Right.
.TP 8
TRANS   (input) CHARACTER*1
.br
= 'N':  No transpose, apply Q or P;
.br
= 'C':  Conjugate transpose, apply Q**H or P**H.
.TP 8
M       (input) INTEGER
The number of rows of the matrix C. M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrix C. N >= 0.
.TP 8
K       (input) INTEGER
If VECT = 'Q', the number of columns in the original
matrix reduced by CGEBRD.
If VECT = 'P', the number of rows in the original
matrix reduced by CGEBRD.
K >= 0.
.TP 8
A       (input) COMPLEX array, dimension
(LDA,min(nq,K)) if VECT = 'Q'
(LDA,nq)        if VECT = 'P'
The vectors which define the elementary reflectors H(i) and
G(i), whose products determine the matrices Q and P, as
returned by CGEBRD.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.
If VECT = 'Q', LDA >= max(1,nq);
if VECT = 'P', LDA >= max(1,min(nq,K)).
.TP 8
TAU     (input) COMPLEX array, dimension (min(nq,K))
TAU(i) must contain the scalar factor of the elementary
reflector H(i) or G(i) which determines Q or P, as returned
by CGEBRD in the array argument TAUQ or TAUP.
.TP 8
C       (input/output) COMPLEX array, dimension (LDC,N)
On entry, the M-by-N matrix C.
On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q
or P*C or P**H*C or C*P or C*P**H.
.TP 8
LDC     (input) INTEGER
The leading dimension of the array C. LDC >= max(1,M).
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.
If SIDE = 'L', LWORK >= max(1,N);
if SIDE = 'R', LWORK >= max(1,M).
For optimum performance LWORK >= N*NB if SIDE = 'L', and
LWORK >= M*NB if SIDE = 'R', where NB is the optimal
blocksize.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
