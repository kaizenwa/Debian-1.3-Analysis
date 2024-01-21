.TH DGEGV l "08 October 1994" "LAPACK version 2.0" "LAPACK driver routine (version 2.0)"
.TH DGEGV l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DGEGV - compute for a pair of n-by-n real nonsymmetric matrices A and B, the generalized eigenvalues (alphar +/- alphai*i, beta), and optionally, the left and/or right generalized eigenvectors (VL and VR)
.SH SYNOPSIS
.TP 18
SUBROUTINE DGEGV(
JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHAR, ALPHAI,
BETA, VL, LDVL, VR, LDVR, WORK, LWORK, INFO )
.TP 18
.ti +4
CHARACTER
JOBVL, JOBVR
.TP 18
.ti +4
INTEGER
INFO, LDA, LDB, LDVL, LDVR, LWORK, N
.TP 18
.ti +4
DOUBLE
PRECISION A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
B( LDB, * ), BETA( * ), VL( LDVL, * ),
VR( LDVR, * ), WORK( * )
.SH PURPOSE
DGEGV computes for a pair of n-by-n real nonsymmetric matrices A and
B, the generalized eigenvalues (alphar +/- alphai*i, beta), and
optionally, the left and/or right generalized eigenvectors (VL and
VR).

A generalized eigenvalue for a pair of matrices (A,B) is, roughly
speaking, a scalar w or a ratio  alpha/beta = w, such that  A - w*B
is singular.  It is usually represented as the pair (alpha,beta),
as there is a reasonable interpretation for beta=0, and even for
both being zero.  A good beginning reference is the book, "Matrix
Computations", by G. Golub & C. van Loan (Johns Hopkins U. Press)

A right generalized eigenvector corresponding to a generalized
eigenvalue  w  for a pair of matrices (A,B) is a vector  r  such
that  (A - w B) r = 0 .  A left generalized eigenvector is a vector
l such that l**H * (A - w B) = 0, where l**H is the
.br
conjugate-transpose of l.
.br

Note: this routine performs "full balancing" on A and B -- see
"Further Details", below.
.br
.SH ARGUMENTS
.TP 8
JOBVL   (input) CHARACTER*1
= 'N':  do not compute the left generalized eigenvectors;
.br
= 'V':  compute the left generalized eigenvectors.
.TP 8
JOBVR   (input) CHARACTER*1
.br
= 'N':  do not compute the right generalized eigenvectors;
.br
= 'V':  compute the right generalized eigenvectors.
.TP 8
N       (input) INTEGER
The order of the matrices A, B, VL, and VR.  N >= 0.
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
On entry, the first of the pair of matrices whose
generalized eigenvalues and (optionally) generalized
eigenvectors are to be computed.
On exit, the contents will have been destroyed.  (For a
description of the contents of A on exit, see "Further
Details", below.)
.TP 8
LDA     (input) INTEGER
The leading dimension of A.  LDA >= max(1,N).
.TP 8
B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
On entry, the second of the pair of matrices whose
generalized eigenvalues and (optionally) generalized
eigenvectors are to be computed.
On exit, the contents will have been destroyed.  (For a
description of the contents of B on exit, see "Further
Details", below.)
.TP 8
LDB     (input) INTEGER
The leading dimension of B.  LDB >= max(1,N).
.TP 8
ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
BETA    (output) DOUBLE PRECISION array, dimension (N)
On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j=1,...,N, will
be the generalized eigenvalues.  If ALPHAI(j) is zero, then
the j-th eigenvalue is real; if positive, then the j-th and
(j+1)-st eigenvalues are a complex conjugate pair, with
ALPHAI(j+1) negative.

Note: the quotients ALPHAR(j)/BETA(j) and ALPHAI(j)/BETA(j)
may easily over- or underflow, and BETA(j) may even be zero.
Thus, the user should avoid naively computing the ratio
alpha/beta.  However, ALPHAR and ALPHAI will be always less
than and usually comparable with norm(A) in magnitude, and
BETA always less than and usually comparable with norm(B).
.TP 8
VL      (output) DOUBLE PRECISION array, dimension (LDVL,N)
If JOBVL = 'V', the left generalized eigenvectors.  (See
"Purpose", above.)  Real eigenvectors take one column,
complex take two columns, the first for the real part and
the second for the imaginary part.  Complex eigenvectors
correspond to an eigenvalue with positive imaginary part.
Each eigenvector will be scaled so the largest component
will have abs(real part) + abs(imag. part) = 1, *except*
that for eigenvalues with alpha=beta=0, a zero vector will
be returned as the corresponding eigenvector.
Not referenced if JOBVL = 'N'.
.TP 8
LDVL    (input) INTEGER
The leading dimension of the matrix VL. LDVL >= 1, and
if JOBVL = 'V', LDVL >= N.
.TP 8
VR      (output) DOUBLE PRECISION array, dimension (LDVR,N)
If JOBVL = 'V', the right generalized eigenvectors.  (See
"Purpose", above.)  Real eigenvectors take one column,
complex take two columns, the first for the real part and
the second for the imaginary part.  Complex eigenvectors
correspond to an eigenvalue with positive imaginary part.
Each eigenvector will be scaled so the largest component
will have abs(real part) + abs(imag. part) = 1, *except*
that for eigenvalues with alpha=beta=0, a zero vector will
be returned as the corresponding eigenvector.
Not referenced if JOBVR = 'N'.
.TP 8
LDVR    (input) INTEGER
The leading dimension of the matrix VR. LDVR >= 1, and
if JOBVR = 'V', LDVR >= N.
.TP 8
WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,8*N).
For good performance, LWORK must generally be larger.
To compute the optimal value of LWORK, call ILAENV to get
blocksizes (for DGEQRF, DORMQR, and DORGQR.)  Then compute:
NB  -- MAX of the blocksizes for DGEQRF, DORMQR, and DORGQR;
The optimal LWORK is:
2*N + MAX( 6*N, N*(NB+1) ).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
= 1,...,N:
The QZ iteration failed.  No eigenvectors have been
calculated, but ALPHAR(j), ALPHAI(j), and BETA(j)
should be correct for j=INFO+1,...,N.
> N:  errors that usually indicate LAPACK problems:
.br
=N+1: error return from DGGBAL
.br
=N+2: error return from DGEQRF
.br
=N+3: error return from DORMQR
.br
=N+4: error return from DORGQR
.br
=N+5: error return from DGGHRD
.br
=N+6: error return from DHGEQZ (other than failed
iteration)
=N+7: error return from DTGEVC
.br
=N+8: error return from DGGBAK (computing VL)
.br
=N+9: error return from DGGBAK (computing VR)
.br
=N+10: error return from DLASCL (various calls)
.SH FURTHER DETAILS
Balancing
.br
---------
.br

This driver calls DGGBAL to both permute and scale rows and columns
of A and B.  The permutations PL and PR are chosen so that PL*A*PR
and PL*B*R will be upper triangular except for the diagonal blocks
A(i:j,i:j) and B(i:j,i:j), with i and j as close together as
possible.  The diagonal scaling matrices DL and DR are chosen so
that the pair  DL*PL*A*PR*DR, DL*PL*B*PR*DR have elements close to
one (except for the elements that start out zero.)
.br

After the eigenvalues and eigenvectors of the balanced matrices
have been computed, DGGBAK transforms the eigenvectors back to what
they would have been (in perfect arithmetic) if they had not been
balanced.
.br

Contents of A and B on Exit
.br
-------- -- - --- - -- ----
.br

If any eigenvectors are computed (either JOBVL='V' or JOBVR='V' or
both), then on exit the arrays A and B will contain the real Schur
form[*] of the "balanced" versions of A and B.  If no eigenvectors
are computed, then only the diagonal blocks will be correct.

[*] See DHGEQZ, DGEGS, or read the book "Matrix Computations",
    by Golub & van Loan, pub. by Johns Hopkins U. Press.
.br

