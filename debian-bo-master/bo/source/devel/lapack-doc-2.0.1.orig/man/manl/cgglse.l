.TH CGGLSE l "08 October 1994" "LAPACK version 2.0" "LAPACK driver routine (version 2.0)"
.TH CGGLSE l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CGGLSE - solve the linear equality-constrained least squares (LSE) problem
.SH SYNOPSIS
.TP 19
SUBROUTINE CGGLSE(
M, N, P, A, LDA, B, LDB, C, D, X, WORK, LWORK,
INFO )
.TP 19
.ti +4
INTEGER
INFO, LDA, LDB, LWORK, M, N, P
.TP 19
.ti +4
COMPLEX
A( LDA, * ), B( LDB, * ), C( * ), D( * ),
WORK( * ), X( * )
.SH PURPOSE
CGGLSE solves the linear equality-constrained least squares (LSE)
problem:

        minimize || c - A*x ||_2   subject to   B*x = d
.br

where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
M-vector, and d is a given P-vector. It is assumed that
.br
P <= N <= M+P, and
.br

         rank(B) = P and  rank( ( A ) ) = N.
.br
                              ( ( B ) )
.br

These conditions ensure that the LSE problem has a unique solution,
which is obtained using a GRQ factorization of the matrices B and A.

.SH ARGUMENTS
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  M >= 0.
.TP 8
N       (input) INTEGER
The number of columns of the matrices A and B. N >= 0.
.TP 8
P       (input) INTEGER
The number of rows of the matrix B. 0 <= P <= N <= M+P.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the M-by-N matrix A.
On exit, A is destroyed.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A. LDA >= max(1,M).
.TP 8
B       (input/output) COMPLEX array, dimension (LDB,N)
On entry, the P-by-N matrix B.
On exit, B is destroyed.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B. LDB >= max(1,P).
.TP 8
C       (input/output) COMPLEX array, dimension (M)
On entry, C contains the right hand side vector for the
least squares part of the LSE problem.
On exit, the residual sum of squares for the solution
is given by the sum of squares of elements N-P+1 to M of
vector C.
.TP 8
D       (input/output) COMPLEX array, dimension (P)
On entry, D contains the right hand side vector for the
constrained equation.
On exit, D is destroyed.
.TP 8
X       (output) COMPLEX array, dimension (N)
On exit, X is the solution of the LSE problem.
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK. LWORK >= max(1,M+N+P).
For optimum performance LWORK >= P+min(M,N)+max(M,N)*NB,
where NB is an upper bound for the optimal blocksizes for
CGEQRF, CGERQF, CUNMQR and CUNMRQ.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
