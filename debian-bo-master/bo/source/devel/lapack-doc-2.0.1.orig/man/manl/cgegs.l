.TH CGEGS l "08 October 1994" "LAPACK version 2.0" "LAPACK driver routine (version 2.0)"
.TH CGEGS l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CGEGS - compute for a pair of N-by-N complex nonsymmetric matrices A,
.SH SYNOPSIS
.TP 18
SUBROUTINE CGEGS(
JOBVSL, JOBVSR, N, A, LDA, B, LDB, ALPHA, BETA,
VSL, LDVSL, VSR, LDVSR, WORK, LWORK, RWORK,
INFO )
.TP 18
.ti +4
CHARACTER
JOBVSL, JOBVSR
.TP 18
.ti +4
INTEGER
INFO, LDA, LDB, LDVSL, LDVSR, LWORK, N
.TP 18
.ti +4
REAL
RWORK( * )
.TP 18
.ti +4
COMPLEX
A( LDA, * ), ALPHA( * ), B( LDB, * ),
BETA( * ), VSL( LDVSL, * ), VSR( LDVSR, * ),
WORK( * )
.SH PURPOSE
SGEGS computes for a pair of N-by-N complex nonsymmetric matrices A,
B:  the generalized eigenvalues (alpha, beta), the complex Schur
form (A, B), and optionally left and/or right Schur vectors
(VSL and VSR).
.br

(If only the generalized eigenvalues are needed, use the driver CGEGV
instead.)
.br

A generalized eigenvalue for a pair of matrices (A,B) is, roughly
speaking, a scalar w or a ratio  alpha/beta = w, such that  A - w*B
is singular.  It is usually represented as the pair (alpha,beta),
as there is a reasonable interpretation for beta=0, and even for
both being zero.  A good beginning reference is the book, "Matrix
Computations", by G. Golub & C. van Loan (Johns Hopkins U. Press)

The (generalized) Schur form of a pair of matrices is the result of
multiplying both matrices on the left by one unitary matrix and
both on the right by another unitary matrix, these two unitary
matrices being chosen so as to bring the pair of matrices into
upper triangular form with the diagonal elements of B being
non-negative real numbers (this is also called complex Schur form.)

The left and right Schur vectors are the columns of VSL and VSR,
respectively, where VSL and VSR are the unitary matrices
.br
which reduce A and B to Schur form:
.br

Schur form of (A,B) = ( (VSL)**H A (VSR), (VSL)**H B (VSR) )

.SH ARGUMENTS
.TP 9
JOBVSL   (input) CHARACTER*1
= 'N':  do not compute the left Schur vectors;
.br
= 'V':  compute the left Schur vectors.
.TP 9
JOBVSR   (input) CHARACTER*1
.br
= 'N':  do not compute the right Schur vectors;
.br
= 'V':  compute the right Schur vectors.
.TP 8
N       (input) INTEGER
The order of the matrices A, B, VSL, and VSR.  N >= 0.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA, N)
On entry, the first of the pair of matrices whose generalized
eigenvalues and (optionally) Schur vectors are to be
computed.
On exit, the generalized Schur form of A.
.TP 8
LDA     (input) INTEGER
The leading dimension of A.  LDA >= max(1,N).
.TP 8
B       (input/output) COMPLEX array, dimension (LDB, N)
On entry, the second of the pair of matrices whose
generalized eigenvalues and (optionally) Schur vectors are
to be computed.
On exit, the generalized Schur form of B.
.TP 8
LDB     (input) INTEGER
The leading dimension of B.  LDB >= max(1,N).
.TP 8
ALPHA   (output) COMPLEX array, dimension (N)
BETA    (output) COMPLEX array, dimension (N)
On exit,  ALPHA(j)/BETA(j), j=1,...,N, will be the
generalized eigenvalues.  ALPHA(j), j=1,...,N  and  BETA(j),
j=1,...,N  are the diagonals of the complex Schur form (A,B)
output by CGEGS.  The  BETA(j) will be non-negative real.

Note: the quotients ALPHA(j)/BETA(j) may easily over- or
underflow, and BETA(j) may even be zero.  Thus, the user
should avoid naively computing the ratio alpha/beta.
However, ALPHA will be always less than and usually
comparable with norm(A) in magnitude, and BETA always less
than and usually comparable with norm(B).
.TP 8
VSL     (output) COMPLEX array, dimension (LDVSL,N)
If JOBVSL = 'V', VSL will contain the left Schur vectors.
(See "Purpose", above.)
Not referenced if JOBVSL = 'N'.
.TP 8
LDVSL   (input) INTEGER
The leading dimension of the matrix VSL. LDVSL >= 1, and
if JOBVSL = 'V', LDVSL >= N.
.TP 8
VSR     (output) COMPLEX array, dimension (LDVSR,N)
If JOBVSR = 'V', VSR will contain the right Schur vectors.
(See "Purpose", above.)
Not referenced if JOBVSR = 'N'.
.TP 8
LDVSR   (input) INTEGER
The leading dimension of the matrix VSR. LDVSR >= 1, and
if JOBVSR = 'V', LDVSR >= N.
.TP 8
WORK    (workspace/output) COMPLEX array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,2*N).
For good performance, LWORK must generally be larger.
To compute the optimal value of LWORK, call ILAENV to get
blocksizes (for CGEQRF, CUNMQR, and CUNGQR.)  Then compute:
NB  -- MAX of the blocksizes for CGEQRF, CUNMQR, and CUNGQR;
the optimal LWORK is N*(NB+1).
.TP 8
RWORK   (workspace) REAL array, dimension (3*N)
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
=1,...,N:
The QZ iteration failed.  (A,B) are not in Schur
form, but ALPHA(j) and BETA(j) should be correct for
j=INFO+1,...,N.
> N:  errors that usually indicate LAPACK problems:
.br
=N+1: error return from CGGBAL
.br
=N+2: error return from CGEQRF
.br
=N+3: error return from CUNMQR
.br
=N+4: error return from CUNGQR
.br
=N+5: error return from CGGHRD
.br
=N+6: error return from CHGEQZ (other than failed
iteration)
=N+7: error return from CGGBAK (computing VSL)
.br
=N+8: error return from CGGBAK (computing VSR)
.br
=N+9: error return from CLASCL (various places)
