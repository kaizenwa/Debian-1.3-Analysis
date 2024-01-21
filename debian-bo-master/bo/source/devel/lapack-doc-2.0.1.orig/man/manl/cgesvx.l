.TH CGESVX l "08 October 1994" "LAPACK version 2.0" "LAPACK driver routine (version 2.0)"
.TH CGESVX l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
CGESVX - use the LU factorization to compute the solution to a complex system of linear equations  A * X = B,
.SH SYNOPSIS
.TP 19
SUBROUTINE CGESVX(
FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, IPIV,
EQUED, R, C, B, LDB, X, LDX, RCOND, FERR, BERR,
WORK, RWORK, INFO )
.TP 19
.ti +4
CHARACTER
EQUED, FACT, TRANS
.TP 19
.ti +4
INTEGER
INFO, LDA, LDAF, LDB, LDX, N, NRHS
.TP 19
.ti +4
REAL
RCOND
.TP 19
.ti +4
INTEGER
IPIV( * )
.TP 19
.ti +4
REAL
BERR( * ), C( * ), FERR( * ), R( * ),
RWORK( * )
.TP 19
.ti +4
COMPLEX
A( LDA, * ), AF( LDAF, * ), B( LDB, * ),
WORK( * ), X( LDX, * )
.SH PURPOSE
CGESVX uses the LU factorization to compute the solution to a complex
system of linear equations
   A * X = B,
where A is an N-by-N matrix and X and B are N-by-NRHS matrices.

Error bounds on the solution and a condition estimate are also
provided.
.br

.SH DESCRIPTION
The following steps are performed:
.br

1. If FACT = 'E', real scaling factors are computed to equilibrate
   the system:
.br
      TRANS = 'N':  diag(R)*A*diag(C)     *inv(diag(C))*X = diag(R)*B
      TRANS = 'T': (diag(R)*A*diag(C))**T *inv(diag(R))*X = diag(C)*B
      TRANS = 'C': (diag(R)*A*diag(C))**H *inv(diag(R))*X = diag(C)*B
   Whether or not the system will be equilibrated depends on the
   scaling of the matrix A, but if equilibration is used, A is
   overwritten by diag(R)*A*diag(C) and B by diag(R)*B (if TRANS='N')
   or diag(C)*B (if TRANS = 'T' or 'C').
.br

2. If FACT = 'N' or 'E', the LU decomposition is used to factor the
   matrix A (after equilibration if FACT = 'E') as
.br
      A = P * L * U,
.br
   where P is a permutation matrix, L is a unit lower triangular
   matrix, and U is upper triangular.
.br

3. The factored form of A is used to estimate the condition number
   of the matrix A.  If the reciprocal of the condition number is
   less than machine precision, steps 4-6 are skipped.
.br

4. The system of equations is solved for X using the factored form
   of A.
.br

5. Iterative refinement is applied to improve the computed solution
   matrix and calculate error bounds and backward error estimates
   for it.
.br

6. If equilibration was used, the matrix X is premultiplied by
   diag(C) (if TRANS = 'N') or diag(R) (if TRANS = 'T' or 'C') so
   that it solves the original system before equilibration.

.SH ARGUMENTS
.TP 8
FACT    (input) CHARACTER*1
Specifies whether or not the factored form of the matrix A is
supplied on entry, and if not, whether the matrix A should be
equilibrated before it is factored.
= 'F':  On entry, AF and IPIV contain the factored form of A.
If EQUED is not 'N', the matrix A has been
equilibrated with scaling factors given by R and C.
A, AF, and IPIV are not modified.
= 'N':  The matrix A will be copied to AF and factored.
.br
= 'E':  The matrix A will be equilibrated if necessary, then
copied to AF and factored.
.TP 8
TRANS   (input) CHARACTER*1
Specifies the form of the system of equations:
.br
= 'N':  A * X = B     (No transpose)
.br
= 'T':  A**T * X = B  (Transpose)
.br
= 'C':  A**H * X = B  (Conjugate transpose)
.TP 8
N       (input) INTEGER
The number of linear equations, i.e., the order of the
matrix A.  N >= 0.
.TP 8
NRHS    (input) INTEGER
The number of right hand sides, i.e., the number of columns
of the matrices B and X.  NRHS >= 0.
.TP 8
A       (input/output) COMPLEX array, dimension (LDA,N)
On entry, the N-by-N matrix A.  If FACT = 'F' and EQUED is
not 'N', then A must have been equilibrated by the scaling
factors in R and/or C.  A is not modified if FACT = 'F' or
'N', or if FACT = 'E' and EQUED = 'N' on exit.

On exit, if EQUED .ne. 'N', A is scaled as follows:
EQUED = 'R':  A := diag(R) * A
.br
EQUED = 'C':  A := A * diag(C)
.br
EQUED = 'B':  A := diag(R) * A * diag(C).
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
AF      (input or output) COMPLEX array, dimension (LDAF,N)
If FACT = 'F', then AF is an input argument and on entry
contains the factors L and U from the factorization
A = P*L*U as computed by CGETRF.  If EQUED .ne. 'N', then
AF is the factored form of the equilibrated matrix A.

If FACT = 'N', then AF is an output argument and on exit
returns the factors L and U from the factorization A = P*L*U
of the original matrix A.

If FACT = 'E', then AF is an output argument and on exit
returns the factors L and U from the factorization A = P*L*U
of the equilibrated matrix A (see the description of A for
the form of the equilibrated matrix).
.TP 8
LDAF    (input) INTEGER
The leading dimension of the array AF.  LDAF >= max(1,N).
.TP 8
IPIV    (input or output) INTEGER array, dimension (N)
If FACT = 'F', then IPIV is an input argument and on entry
contains the pivot indices from the factorization A = P*L*U
as computed by CGETRF; row i of the matrix was interchanged
with row IPIV(i).

If FACT = 'N', then IPIV is an output argument and on exit
contains the pivot indices from the factorization A = P*L*U
of the original matrix A.

If FACT = 'E', then IPIV is an output argument and on exit
contains the pivot indices from the factorization A = P*L*U
of the equilibrated matrix A.
.TP 8
EQUED   (input or output) CHARACTER*1
Specifies the form of equilibration that was done.
= 'N':  No equilibration (always true if FACT = 'N').
.br
= 'R':  Row equilibration, i.e., A has been premultiplied by
diag(R).
= 'C':  Column equilibration, i.e., A has been postmultiplied
by diag(C).
= 'B':  Both row and column equilibration, i.e., A has been
replaced by diag(R) * A * diag(C).
EQUED is an input argument if FACT = 'F'; otherwise, it is an
output argument.
.TP 8
R       (input or output) REAL array, dimension (N)
The row scale factors for A.  If EQUED = 'R' or 'B', A is
multiplied on the left by diag(R); if EQUED = 'N' or 'C', R
is not accessed.  R is an input argument if FACT = 'F';
otherwise, R is an output argument.  If FACT = 'F' and
EQUED = 'R' or 'B', each element of R must be positive.
.TP 8
C       (input or output) REAL array, dimension (N)
The column scale factors for A.  If EQUED = 'C' or 'B', A is
multiplied on the right by diag(C); if EQUED = 'N' or 'R', C
is not accessed.  C is an input argument if FACT = 'F';
otherwise, C is an output argument.  If FACT = 'F' and
EQUED = 'C' or 'B', each element of C must be positive.
.TP 8
B       (input/output) COMPLEX array, dimension (LDB,NRHS)
On entry, the N-by-NRHS right hand side matrix B.
On exit,
if EQUED = 'N', B is not modified;
if TRANS = 'N' and EQUED = 'R' or 'B', B is overwritten by
diag(R)*B;
if TRANS = 'T' or 'C' and EQUED = 'C' or 'B', B is
overwritten by diag(C)*B.
.TP 8
LDB     (input) INTEGER
The leading dimension of the array B.  LDB >= max(1,N).
.TP 8
X       (output) COMPLEX array, dimension (LDX,NRHS)
If INFO = 0, the N-by-NRHS solution matrix X to the original
system of equations.  Note that A and B are modified on exit
if EQUED .ne. 'N', and the solution to the equilibrated
system is inv(diag(C))*X if TRANS = 'N' and EQUED = 'C' or
'B', or inv(diag(R))*X if TRANS = 'T' or 'C' and EQUED = 'R'
or 'B'.
.TP 8
LDX     (input) INTEGER
The leading dimension of the array X.  LDX >= max(1,N).
.TP 8
RCOND   (output) REAL
The estimate of the reciprocal condition number of the matrix
A after equilibration (if done).  If RCOND is less than the
machine precision (in particular, if RCOND = 0), the matrix
is singular to working precision.  This condition is
indicated by a return code of INFO > 0, and the solution and
error bounds are not computed.
.TP 8
FERR    (output) REAL array, dimension (NRHS)
The estimated forward error bound for each solution vector
X(j) (the j-th column of the solution matrix X).
If XTRUE is the true solution corresponding to X(j), FERR(j)
is an estimated upper bound for the magnitude of the largest
element in (X(j) - XTRUE) divided by the magnitude of the
largest element in X(j).  The estimate is as reliable as
the estimate for RCOND, and is almost always a slight
overestimate of the true error.
.TP 8
BERR    (output) REAL array, dimension (NRHS)
The componentwise relative backward error of each solution
vector X(j) (i.e., the smallest relative change in
any element of A or B that makes X(j) an exact solution).
.TP 8
WORK    (workspace) COMPLEX array, dimension (2*N)
.TP 8
RWORK   (workspace/output) REAL array, dimension (2*N)
On exit, RWORK(1) contains the reciprocal pivot growth
factor norm(A)/norm(U). The "max absolute element" norm is
used. If RWORK(1) is much less than 1, then the stability
of the LU factorization of the (equilibrated) matrix A
could be poor. This also means that the solution X, condition
estimator RCOND, and forward error bound FERR could be
unreliable. If factorization fails with 0<INFO<=N, then
RWORK(1) contains the reciprocal pivot growth factor for the
leading INFO columns of A.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  if INFO = i, and i is
.br
<= N:  U(i,i) is exactly zero.  The factorization has
been completed, but the factor U is exactly
singular, so the solution and error bounds
could not be computed.
= N+1: RCOND is less than machine precision.  The
factorization has been completed, but the
matrix is singular to working precision, and
the solution and error bounds have not been
computed.
