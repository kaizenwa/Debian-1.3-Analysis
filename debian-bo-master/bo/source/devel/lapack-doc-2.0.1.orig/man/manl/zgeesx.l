.TH ZGEESX l "08 October 1994" "LAPACK version 2.0" "LAPACK driver routine (version 2.0)"
.TH ZGEESX l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
ZGEESX - compute for an N-by-N complex nonsymmetric matrix A, the eigenvalues, the Schur form T, and, optionally, the matrix of Schur vectors Z
.SH SYNOPSIS
.TP 19
SUBROUTINE ZGEESX(
JOBVS, SORT, SELECT, SENSE, N, A, LDA, SDIM, W,
VS, LDVS, RCONDE, RCONDV, WORK, LWORK, RWORK,
BWORK, INFO )
.TP 19
.ti +4
CHARACTER
JOBVS, SENSE, SORT
.TP 19
.ti +4
INTEGER
INFO, LDA, LDVS, LWORK, N, SDIM
.TP 19
.ti +4
DOUBLE
PRECISION RCONDE, RCONDV
.TP 19
.ti +4
LOGICAL
BWORK( * )
.TP 19
.ti +4
DOUBLE
PRECISION RWORK( * )
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), VS( LDVS, * ), W( * ), WORK( * )
.TP 19
.ti +4
LOGICAL
SELECT
.TP 19
.ti +4
EXTERNAL
SELECT
.SH PURPOSE
ZGEESX computes for an N-by-N complex nonsymmetric matrix A, the
eigenvalues, the Schur form T, and, optionally, the matrix of Schur
vectors Z.  This gives the Schur factorization A = Z*T*(Z**H).

Optionally, it also orders the eigenvalues on the diagonal of the
Schur form so that selected eigenvalues are at the top left;
computes a reciprocal condition number for the average of the
selected eigenvalues (RCONDE); and computes a reciprocal condition
number for the right invariant subspace corresponding to the
selected eigenvalues (RCONDV).  The leading columns of Z form an
orthonormal basis for this invariant subspace.
.br

For further explanation of the reciprocal condition numbers RCONDE
and RCONDV, see Section 4.10 of the LAPACK Users' Guide (where
these quantities are called s and sep respectively).
.br

A complex matrix is in Schur form if it is upper triangular.

.SH ARGUMENTS
.TP 8
JOBVS   (input) CHARACTER*1
= 'N': Schur vectors are not computed;
.br
= 'V': Schur vectors are computed.
.TP 8
SORT    (input) CHARACTER*1
Specifies whether or not to order the eigenvalues on the
diagonal of the Schur form.
= 'N': Eigenvalues are not ordered;
.br
= 'S': Eigenvalues are ordered (see SELECT).
.TP 8
SELECT  (input) LOGICAL FUNCTION of one COMPLEX*16 argument
SELECT must be declared EXTERNAL in the calling subroutine.
If SORT = 'S', SELECT is used to select eigenvalues to order
to the top left of the Schur form.
If SORT = 'N', SELECT is not referenced.
An eigenvalue W(j) is selected if SELECT(W(j)) is true.
.TP 8
SENSE   (input) CHARACTER*1
Determines which reciprocal condition numbers are computed.
= 'N': None are computed;
.br
= 'E': Computed for average of selected eigenvalues only;
.br
= 'V': Computed for selected right invariant subspace only;
.br
= 'B': Computed for both.
If SENSE = 'E', 'V' or 'B', SORT must equal 'S'.
.TP 8
N       (input) INTEGER
The order of the matrix A. N >= 0.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA, N)
On entry, the N-by-N matrix A.
On exit, A is overwritten by its Schur form T.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
SDIM    (output) INTEGER
If SORT = 'N', SDIM = 0.
If SORT = 'S', SDIM = number of eigenvalues for which
SELECT is true.
.TP 8
W       (output) COMPLEX*16 array, dimension (N)
W contains the computed eigenvalues, in the same order
that they appear on the diagonal of the output Schur form T.
.TP 8
VS      (output) COMPLEX*16 array, dimension (LDVS,N)
If JOBVS = 'V', VS contains the unitary matrix Z of Schur
vectors.
If JOBVS = 'N', VS is not referenced.
.TP 8
LDVS    (input) INTEGER
The leading dimension of the array VS.  LDVS >= 1, and if
JOBVS = 'V', LDVS >= N.
.TP 8
RCONDE  (output) DOUBLE PRECISION
If SENSE = 'E' or 'B', RCONDE contains the reciprocal
condition number for the average of the selected eigenvalues.
Not referenced if SENSE = 'N' or 'V'.
.TP 8
RCONDV  (output) DOUBLE PRECISION
If SENSE = 'V' or 'B', RCONDV contains the reciprocal
condition number for the selected right invariant subspace.
Not referenced if SENSE = 'N' or 'E'.
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,2*N).
Also, if SENSE = 'E' or 'V' or 'B', LWORK >= 2*SDIM*(N-SDIM),
where SDIM is the number of selected eigenvalues computed by
this routine.  Note that 2*SDIM*(N-SDIM) <= N*N/2.
For good performance, LWORK must generally be larger.
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (N)
.TP 8
BWORK   (workspace) LOGICAL array, dimension (N)
Not referenced if SORT = 'N'.
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value.
.br
> 0: if INFO = i, and i is
.br
<= N: the QR algorithm failed to compute all the
.br
eigenvalues; elements 1:ILO-1 and i+1:N of W
contain those eigenvalues which have converged; if
JOBVS = 'V', VS contains the transformation which
reduces A to its partially converged Schur form.
= N+1: the eigenvalues could not be reordered because some
eigenvalues were too close to separate (the problem
is very ill-conditioned);
= N+2: after reordering, roundoff changed values of some
complex eigenvalues so that leading eigenvalues in
the Schur form no longer satisfy SELECT=.TRUE.  This
could also be caused by underflow due to scaling.
