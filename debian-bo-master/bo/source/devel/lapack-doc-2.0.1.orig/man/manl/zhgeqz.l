.TH ZHGEQZ l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH ZHGEQZ l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
ZHGEQZ - implement a single-shift version of the QZ method for finding the generalized eigenvalues w(i)=ALPHA(i)/BETA(i) of the equation   det( A - w(i) B ) = 0  If JOB='S', then the pair (A,B) is simultaneously reduced to Schur form (i.e., A and B are both upper triangular) by applying one unitary tranformation (usually called Q) on the left and another (usually called Z) on the right
.SH SYNOPSIS
.TP 19
SUBROUTINE ZHGEQZ(
JOB, COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB,
ALPHA, BETA, Q, LDQ, Z, LDZ, WORK, LWORK,
RWORK, INFO )
.TP 19
.ti +4
CHARACTER
COMPQ, COMPZ, JOB
.TP 19
.ti +4
INTEGER
IHI, ILO, INFO, LDA, LDB, LDQ, LDZ, LWORK, N
.TP 19
.ti +4
DOUBLE
PRECISION RWORK( * )
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), ALPHA( * ), B( LDB, * ),
BETA( * ), Q( LDQ, * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
ZHGEQZ implements a single-shift version of the QZ
method for finding the generalized eigenvalues w(i)=ALPHA(i)/BETA(i)
of the equation
A are then ALPHA(1),...,ALPHA(N), and of B are BETA(1),...,BETA(N).

If JOB='S' and COMPQ and COMPZ are 'V' or 'I', then the unitary
transformations used to reduce (A,B) are accumulated into the arrays
Q and Z s.t.:
.br

     Q(in) A(in) Z(in)* = Q(out) A(out) Z(out)*
.br
     Q(in) B(in) Z(in)* = Q(out) B(out) Z(out)*
.br

Ref: C.B. Moler & G.W. Stewart, "An Algorithm for Generalized Matrix
     Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973),
     pp. 241--256.
.br

.SH ARGUMENTS
.TP 8
JOB     (input) CHARACTER*1
= 'E': compute only ALPHA and BETA.  A and B will not
necessarily be put into generalized Schur form.
= 'S': put A and B into generalized Schur form, as well
as computing ALPHA and BETA.
.TP 8
COMPQ   (input) CHARACTER*1
= 'N': do not modify Q.
.br
= 'V': multiply the array Q on the right by the conjugate
transpose of the unitary tranformation that is
applied to the left side of A and B to reduce them
to Schur form.
= 'I': like COMPQ='V', except that Q will be initialized to
the identity first.
.TP 8
COMPZ   (input) CHARACTER*1
= 'N': do not modify Z.
.br
= 'V': multiply the array Z on the right by the unitary
tranformation that is applied to the right side of
A and B to reduce them to Schur form.
= 'I': like COMPZ='V', except that Z will be initialized to
the identity first.
.TP 8
N       (input) INTEGER
The order of the matrices A, B, Q, and Z.  N >= 0.
.TP 8
ILO     (input) INTEGER
IHI     (input) INTEGER
It is assumed that A is already upper triangular in rows and
columns 1:ILO-1 and IHI+1:N.
1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
.TP 8
A       (input/output) COMPLEX*16 array, dimension (LDA, N)
On entry, the N-by-N upper Hessenberg matrix A.  Elements
below the subdiagonal must be zero.
If JOB='S', then on exit A and B will have been
simultaneously reduced to upper triangular form.
If JOB='E', then on exit A will have been destroyed.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max( 1, N ).
.TP 8
B       (input/output) COMPLEX*16 array, dimension (LDB, N)
On entry, the N-by-N upper triangular matrix B.  Elements
below the diagonal must be zero.
If JOB='S', then on exit A and B will have been
simultaneously reduced to upper triangular form.
If JOB='E', then on exit B will have been destroyed.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max( 1, N ).
.TP 8
ALPHA   (output) COMPLEX*16 array, dimension (N)
The diagonal elements of A when the pair (A,B) has been
reduced to Schur form.  ALPHA(i)/BETA(i) i=1,...,N
are the generalized eigenvalues.
.TP 8
BETA    (output) COMPLEX*16 array, dimension (N)
The diagonal elements of B when the pair (A,B) has been
reduced to Schur form.  ALPHA(i)/BETA(i) i=1,...,N
are the generalized eigenvalues.  A and B are normalized
so that BETA(1),...,BETA(N) are non-negative real numbers.
.TP 8
Q       (input/output) COMPLEX*16 array, dimension (LDQ, N)
If COMPQ='N', then Q will not be referenced.
If COMPQ='V' or 'I', then the conjugate transpose of the
unitary transformations which are applied to A and B on
the left will be applied to the array Q on the right.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.  LDQ >= 1.
If COMPQ='V' or 'I', then LDQ >= N.
.TP 8
Z       (input/output) COMPLEX*16 array, dimension (LDZ, N)
If COMPZ='N', then Z will not be referenced.
If COMPZ='V' or 'I', then the unitary transformations which
are applied to A and B on the right will be applied to the
array Z on the right.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1.
If COMPZ='V' or 'I', then LDZ >= N.
.TP 8
WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
On exit, if INFO >= 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,N).
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (N)
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
= 1,...,N: the QZ iteration did not converge.  (A,B) is not
in Schur form, but ALPHA(i) and BETA(i),
i=INFO+1,...,N should be correct.
= N+1,...,2*N: the shift calculation failed.  (A,B) is not
in Schur form, but ALPHA(i) and BETA(i),
i=INFO-N+1,...,N should be correct.
> 2*N:     various "impossible" errors.
.SH FURTHER DETAILS
We assume that complex ABS works as long as its value is less than
overflow.
.br

