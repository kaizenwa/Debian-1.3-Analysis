.TH ZGESVD l "08 October 1994" "LAPACK version 2.0" "LAPACK driver routine (version 2.0)"
.TH ZGESVD l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
ZGESVD - compute the singular value decomposition (SVD) of a complex M-by-N matrix A, optionally computing the left and/or right singular vectors
.SH SYNOPSIS
.TP 19
SUBROUTINE ZGESVD(
JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,
WORK, LWORK, RWORK, INFO )
.TP 19
.ti +4
CHARACTER
JOBU, JOBVT
.TP 19
.ti +4
INTEGER
INFO, LDA, LDU, LDVT, LWORK, M, N
.TP 19
.ti +4
DOUBLE
PRECISION RWORK( * ), S( * )
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), U( LDU, * ), VT( LDVT, * ),
WORK( * )
.SH PURPOSE
ZGESVD computes the singular value decomposition (SVD) of a complex
M-by-N matrix A, optionally computing the left and/or right singular
vectors. The SVD is written

     A = U * SIGMA * conjugate-transpose(V)
.br

where SIGMA is an M-by-N matrix which is zero except for its
min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
are the singular values of A; they are real and non-negative, and
are returned in descending order.  The first min(m,n) columns of
U and V are the left and right singular vectors of A.
.br

Note that the routine returns V**H, not V.
.br

.SH ARGUMENTS
.TP 8
JOBU    (input) CHARACTER*1
Specifies options for computing all or part of the matrix U:
.br
= 'A':  all M columns of U are returned in array U:
.br
= 'S':  the first min(m,n) columns of U (the left singular
vectors) are returned in the array U;
= 'O':  the first min(m,n) columns of U (the left singular
vectors) are overwritten on the array A;
= 'N':  no columns of U (no left singular vectors) are
computed.
.TP 8
JOBVT   (input) CHARACTER*1
Specifies options for computing all or part of the matrix
V**H:
.br
= 'A':  all N rows of V**H are returned in the array VT;
.br
= 'S':  the first min(m,n) rows of V**H (the right singular
vectors) are returned in the array VT;
= 'O':  the first min(m,n) rows of V**H (the right singular
vectors) are overwritten on the array A;
= 'N':  no rows of V**H (no right singular vectors) are
computed.

JOBVT and JOBU cannot both be 'O'.
.TP 8
M       (input) INTEGER
The number of rows of the input matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the input matrix A.  N >= 0.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit,
if JOBU = 'O',  A is overwritten with the first min(m,n)
columns of U (the left singular vectors,
stored columnwise);
if JOBVT = 'O', A is overwritten with the first min(m,n)
rows of V**H (the right singular vectors,
stored rowwise);
if JOBU .ne. 'O' and JOBVT .ne. 'O', the contents of A
are destroyed.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
.TP 8
S       (output) DOUBLE PRECISION array, dimension (min(M,N))
The singular values of A, sorted so that S(i) >= S(i+1).
.TP 8
U       (output) COMPLEX*16 array, dimension (LDU,UCOL)
(LDU,M) if JOBU = 'A' or (LDU,min(M,N)) if JOBU = 'S'.
If JOBU = 'A', U contains the M-by-M unitary matrix U;
if JOBU = 'S', U contains the first min(m,n) columns of U
(the left singular vectors, stored columnwise);
if JOBU = 'N' or 'O', U is not referenced.
.TP 8
LDU     (input) INTEGER
The leading dimension of the array U.  LDU >= 1; if
JOBU = 'S' or 'A', LDU >= M.
.TP 8
VT      (output) COMPLEX*16 array, dimension (LDVT,N)
If JOBVT = 'A', VT contains the N-by-N unitary matrix
V**H;
if JOBVT = 'S', VT contains the first min(m,n) rows of
V**H (the right singular vectors, stored rowwise);
if JOBVT = 'N' or 'O', VT is not referenced.
.TP 8
LDVT    (input) INTEGER
The leading dimension of the array VT.  LDVT >= 1; if
JOBVT = 'A', LDVT >= N; if JOBVT = 'S', LDVT >= min(M,N).
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= 1.
LWORK >=  2*MIN(M,N)+MAX(M,N).
For good performance, LWORK should generally be larger.
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension
(max(3*min(M,N),5*min(M,N)-4))
On exit, if INFO > 0, RWORK(1:MIN(M,N)-1) contains the
unconverged superdiagonal elements of an upper bidiagonal
matrix B whose diagonal is in S (not necessarily sorted).
B satisfies A = U * B * VT, so it has the same singular
values as A, and singular vectors related by U and VT.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if ZBDSQR did not converge, INFO specifies how many
superdiagonals of an intermediate bidiagonal form B
did not converge to zero. See the description of RWORK
above for details.
