.TH DLAED3 l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH DLAED3 l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DLAED3 - find the roots of the secular equation, as defined by the values in D, W, and RHO, between KSTART and KSTOP
.SH SYNOPSIS
.TP 19
SUBROUTINE DLAED3(
K, KSTART, KSTOP, N, D, Q, LDQ, RHO, CUTPNT,
DLAMDA, Q2, LDQ2, INDXC, CTOT, W, S, LDS,
INFO )
.TP 19
.ti +4
INTEGER
CUTPNT, INFO, K, KSTART, KSTOP, LDQ, LDQ2, LDS,
N
.TP 19
.ti +4
DOUBLE
PRECISION RHO
.TP 19
.ti +4
INTEGER
CTOT( * ), INDXC( * )
.TP 19
.ti +4
DOUBLE
PRECISION D( * ), DLAMDA( * ), Q( LDQ, * ),
Q2( LDQ2, * ), S( LDS, * ), W( * )
.SH PURPOSE
DLAED3 finds the roots of the secular equation, as defined by the
values in D, W, and RHO, between KSTART and KSTOP.  It makes the
appropriate calls to DLAED4 and then updates the eigenvectors by
multiplying the matrix of eigenvectors of the pair of eigensystems
being combined by the matrix of eigenvectors of the K-by-K system
which is solved here.
.br

This code makes very mild assumptions about floating point
arithmetic. It will work on machines with a guard digit in
add/subtract, or on those binary machines without guard digits
which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
It could conceivably fail on hexadecimal or decimal machines
without guard digits, but we know of none.
.br

.SH ARGUMENTS
.TP 8
K       (input) INTEGER
The number of terms in the rational function to be solved by
DLAED4.  K >= 0.
.TP 8
KSTART  (input) INTEGER
KSTOP   (input) INTEGER
The updated eigenvalues Lambda(I), KSTART <= I <= KSTOP
are to be computed.  1 <= KSTART <= KSTOP <= K.
.TP 8
N       (input) INTEGER
The number of rows and columns in the Q matrix.
N >= K (deflation may result in N>K).
.TP 8
D       (output) DOUBLE PRECISION array, dimension (N)
D(I) contains the updated eigenvalues for
KSTART <= I <= KSTOP.
.TP 8
Q       (output) DOUBLE PRECISION array, dimension (LDQ,N)
Initially the first K columns are used as workspace.
On output the columns KSTART to KSTOP contain
the updated eigenvectors.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.  LDQ >= max(1,N).
.TP 8
RHO     (input) DOUBLE PRECISION
The value of the parameter in the rank one update equation.
RHO >= 0 required.
.TP 8
CUTPNT  (input) INTEGER
The location of the last eigenvalue in the leading submatrix.
min(1,N) <= CUTPNT <= N.
.TP 8
DLAMDA  (input/output) DOUBLE PRECISION array, dimension (K)
The first K elements of this array contain the old roots
of the deflated updating problem.  These are the poles
of the secular equation. May be changed on output by
having lowest order bit set to zero on Cray X-MP, Cray Y-MP,
Cray-2, or Cray C-90, as described above.
.TP 8
Q2      (input) DOUBLE PRECISION array, dimension (LDQ2, N)
The first K columns of this matrix contain the non-deflated
eigenvectors for the split problem.
.TP 8
LDQ2    (input) INTEGER
The leading dimension of the array Q2.  LDQ2 >= max(1,N).
.TP 8
INDXC   (input) INTEGER array, dimension (N)
The permutation used to arrange the columns of the deflated
Q matrix into three groups:  the first group contains
non-zero elements only at and above CUTPNT, the second
contains non-zero elements only below CUTPNT, and the third
is dense.  The rows of the eigenvectors found by DLAED4
must be likewise permuted before the matrix multiply can take
place.
.TP 8
CTOT    (input) INTEGER array, dimension (4)
A count of the total number of the various types of columns
in Q, as described in INDXC.  The fourth column type is any
column which has been deflated.
.TP 8
W       (input/output) DOUBLE PRECISION array, dimension (K)
The first K elements of this array contain the components
of the deflation-adjusted updating vector. Destroyed on
output.
.TP 8
S       (workspace) DOUBLE PRECISION array, dimension (LDS, K)
Will contain the eigenvectors of the repaired matrix which
will be multiplied by the previously accumulated eigenvectors
to update the system.
.TP 8
LDS     (input) INTEGER
The leading dimension of S.  LDS >= max(1,K).
.TP 8
INFO    (output) INTEGER
= 0:  successful exit.
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if INFO = 1, an eigenvalue did not converge
