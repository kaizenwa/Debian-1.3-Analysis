.TH SHGEQZ l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH SHGEQZ l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SHGEQZ - implement a single-/double-shift version of the QZ method for finding the generalized eigenvalues  w(j)=(ALPHAR(j) + i*ALPHAI(j))/BETAR(j) of the equation   det( A - w(i) B ) = 0  In addition, the pair A,B may be reduced to generalized Schur form
.SH SYNOPSIS
.TP 19
SUBROUTINE SHGEQZ(
JOB, COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB,
ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, WORK,
LWORK, INFO )
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
REAL
A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
B( LDB, * ), BETA( * ), Q( LDQ, * ), WORK( * ),
Z( LDZ, * )
.SH PURPOSE
SHGEQZ implements a single-/double-shift version of the QZ method for
finding the generalized eigenvalues
B is upper triangular, and A is block upper triangular, where the
diagonal blocks are either 1-by-1 or 2-by-2, the 2-by-2 blocks having
complex generalized eigenvalues (see the description of the argument
JOB.)
.br

If JOB='S', then the pair (A,B) is simultaneously reduced to Schur
form by applying one orthogonal tranformation (usually called Q) on
the left and another (usually called Z) on the right.  The 2-by-2
upper-triangular diagonal blocks of B corresponding to 2-by-2 blocks
of A will be reduced to positive diagonal matrices.  (I.e.,
if A(j+1,j) is non-zero, then B(j+1,j)=B(j,j+1)=0 and B(j,j) and
B(j+1,j+1) will be positive.)
.br

If JOB='E', then at each iteration, the same transformations
are computed, but they are only applied to those parts of A and B
which are needed to compute ALPHAR, ALPHAI, and BETAR.
.br

If JOB='S' and COMPQ and COMPZ are 'V' or 'I', then the orthogonal
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
= 'E': compute only ALPHAR, ALPHAI, and BETA.  A and B will
not necessarily be put into generalized Schur form.
= 'S': put A and B into generalized Schur form, as well
as computing ALPHAR, ALPHAI, and BETA.
.TP 8
COMPQ   (input) CHARACTER*1
= 'N': do not modify Q.
.br
= 'V': multiply the array Q on the right by the transpose of
the orthogonal tranformation that is applied to the
left side of A and B to reduce them to Schur form.
= 'I': like COMPQ='V', except that Q will be initialized to
the identity first.
.TP 8
COMPZ   (input) CHARACTER*1
= 'N': do not modify Z.
.br
= 'V': multiply the array Z on the right by the orthogonal
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
A       (input/output) REAL array, dimension (LDA, N)
On entry, the N-by-N upper Hessenberg matrix A.  Elements
below the subdiagonal must be zero.
If JOB='S', then on exit A and B will have been
simultaneously reduced to generalized Schur form.
If JOB='E', then on exit A will have been destroyed.
The diagonal blocks will be correct, but the off-diagonal
portion will be meaningless.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max( 1, N ).
.TP 8
B       (input/output) REAL array, dimension (LDB, N)
On entry, the N-by-N upper triangular matrix B.  Elements
below the diagonal must be zero.  2-by-2 blocks in B
corresponding to 2-by-2 blocks in A will be reduced to
positive diagonal form.  (I.e., if A(j+1,j) is non-zero,
then B(j+1,j)=B(j,j+1)=0 and B(j,j) and B(j+1,j+1) will be
positive.)
If JOB='S', then on exit A and B will have been
simultaneously reduced to Schur form.
If JOB='E', then on exit B will have been destroyed.
Elements corresponding to diagonal blocks of A will be
correct, but the off-diagonal portion will be meaningless.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max( 1, N ).
.TP 8
ALPHAR  (output) REAL array, dimension (N)
ALPHAR(1:N) will be set to real parts of the diagonal
elements of A that would result from reducing A and B to
Schur form and then further reducing them both to triangular
form using unitary transformations s.t. the diagonal of B
was non-negative real.  Thus, if A(j,j) is in a 1-by-1 block
(i.e., A(j+1,j)=A(j,j+1)=0), then ALPHAR(j)=A(j,j).
Note that the (real or complex) values
(ALPHAR(j) + i*ALPHAI(j))/BETA(j), j=1,...,N, are the
generalized eigenvalues of the matrix pencil A - wB.
.TP 8
ALPHAI  (output) REAL array, dimension (N)
ALPHAI(1:N) will be set to imaginary parts of the diagonal
elements of A that would result from reducing A and B to
Schur form and then further reducing them both to triangular
form using unitary transformations s.t. the diagonal of B
was non-negative real.  Thus, if A(j,j) is in a 1-by-1 block
(i.e., A(j+1,j)=A(j,j+1)=0), then ALPHAR(j)=0.
Note that the (real or complex) values
(ALPHAR(j) + i*ALPHAI(j))/BETA(j), j=1,...,N, are the
generalized eigenvalues of the matrix pencil A - wB.
.TP 8
BETA    (output) REAL array, dimension (N)
BETA(1:N) will be set to the (real) diagonal elements of B
that would result from reducing A and B to Schur form and
then further reducing them both to triangular form using
unitary transformations s.t. the diagonal of B was
non-negative real.  Thus, if A(j,j) is in a 1-by-1 block
(i.e., A(j+1,j)=A(j,j+1)=0), then BETA(j)=B(j,j).
Note that the (real or complex) values
(ALPHAR(j) + i*ALPHAI(j))/BETA(j), j=1,...,N, are the
generalized eigenvalues of the matrix pencil A - wB.
(Note that BETA(1:N) will always be non-negative, and no
BETAI is necessary.)
.TP 8
Q       (input/output) REAL array, dimension (LDQ, N)
If COMPQ='N', then Q will not be referenced.
If COMPQ='V' or 'I', then the transpose of the orthogonal
transformations which are applied to A and B on the left
will be applied to the array Q on the right.
.TP 8
LDQ     (input) INTEGER
The leading dimension of the array Q.  LDQ >= 1.
If COMPQ='V' or 'I', then LDQ >= N.
.TP 8
Z       (input/output) REAL array, dimension (LDZ, N)
If COMPZ='N', then Z will not be referenced.
If COMPZ='V' or 'I', then the orthogonal transformations
which are applied to A and B on the right will be applied
to the array Z on the right.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1.
If COMPZ='V' or 'I', then LDZ >= N.
.TP 8
WORK    (workspace/output) REAL array, dimension (LWORK)
On exit, if INFO >= 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,N).
.TP 8
INFO    (output) INTEGER
= 0: successful exit
.br
< 0: if INFO = -i, the i-th argument had an illegal value
.br
= 1,...,N: the QZ iteration did not converge.  (A,B) is not
in Schur form, but ALPHAR(i), ALPHAI(i), and
BETA(i), i=INFO+1,...,N should be correct.
= N+1,...,2*N: the shift calculation failed.  (A,B) is not
in Schur form, but ALPHAR(i), ALPHAI(i), and
BETA(i), i=INFO-N+1,...,N should be correct.
> 2*N:     various "impossible" errors.
.SH FURTHER DETAILS
Iteration counters:
.br

JITER  -- counts iterations.
.br
IITER  -- counts iterations run since ILAST was last
.br
          changed.  This is therefore reset only when a 1-by-1 or
          2-by-2 block deflates off the bottom.
.br

