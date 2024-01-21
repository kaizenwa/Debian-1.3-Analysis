.TH ZHPGV l "08 October 1994" "LAPACK version 2.0" "LAPACK driver routine (version 2.0)"
.TH ZHPGV l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
ZHPGV - compute all the eigenvalues and, optionally, the eigenvectors of a complex generalized Hermitian-definite eigenproblem, of the form A*x=(lambda)*B*x, A*Bx=(lambda)*x, or B*A*x=(lambda)*x
.SH SYNOPSIS
.TP 18
SUBROUTINE ZHPGV(
ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK,
RWORK, INFO )
.TP 18
.ti +4
CHARACTER
JOBZ, UPLO
.TP 18
.ti +4
INTEGER
INFO, ITYPE, LDZ, N
.TP 18
.ti +4
DOUBLE
PRECISION RWORK( * ), W( * )
.TP 18
.ti +4
COMPLEX*16
AP( * ), BP( * ), WORK( * ), Z( LDZ, * )
.SH PURPOSE
ZHPGV computes all the eigenvalues and, optionally, the eigenvectors
of a complex generalized Hermitian-definite eigenproblem, of the form
A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
Here A and B are assumed to be Hermitian, stored in packed format,
and B is also positive definite.
.br

.SH ARGUMENTS
.TP 8
ITYPE   (input) INTEGER
Specifies the problem type to be solved:
.br
= 1:  A*x = (lambda)*B*x
.br
= 2:  A*B*x = (lambda)*x
.br
= 3:  B*A*x = (lambda)*x
.TP 8
JOBZ    (input) CHARACTER*1
.br
= 'N':  Compute eigenvalues only;
.br
= 'V':  Compute eigenvalues and eigenvectors.
.TP 8
UPLO    (input) CHARACTER*1
.br
= 'U':  Upper triangles of A and B are stored;
.br
= 'L':  Lower triangles of A and B are stored.
.TP 8
N       (input) INTEGER
The order of the matrices A and B.  N >= 0.
.TP 8
AP      (input/output) COMPLEX*16 array, dimension (N*(N+1)/2)
On entry, the upper or lower triangle of the Hermitian matrix
A, packed columnwise in a linear array.  The j-th column of A
is stored in the array AP as follows:
if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
if UPLO = 'L', AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n.

On exit, the contents of AP are destroyed.
.TP 8
BP      (input/output) COMPLEX*16 array, dimension (N*(N+1)/2)
On entry, the upper or lower triangle of the Hermitian matrix
B, packed columnwise in a linear array.  The j-th column of B
is stored in the array BP as follows:
if UPLO = 'U', BP(i + (j-1)*j/2) = B(i,j) for 1<=i<=j;
if UPLO = 'L', BP(i + (j-1)*(2*n-j)/2) = B(i,j) for j<=i<=n.

On exit, the triangular factor U or L from the Cholesky
factorization B = U**H*U or B = L*L**H, in the same storage
format as B.
.TP 8
W       (output) DOUBLE PRECISION array, dimension (N)
If INFO = 0, the eigenvalues in ascending order.
.TP 8
Z       (output) COMPLEX*16 array, dimension (LDZ, N)
If JOBZ = 'V', then if INFO = 0, Z contains the matrix Z of
eigenvectors.  The eigenvectors are normalized as follows:
if ITYPE = 1 or 2, Z**H*B*Z = I;
if ITYPE = 3, Z**H*inv(B)*Z = I.
If JOBZ = 'N', then Z is not referenced.
.TP 8
LDZ     (input) INTEGER
The leading dimension of the array Z.  LDZ >= 1, and if
JOBZ = 'V', LDZ >= max(1,N).
.TP 8
WORK    (workspace) COMPLEX*16 array, dimension (max(1, 2*N-1))
.TP 8
RWORK   (workspace) DOUBLE PRECISION array, dimension (max(1, 3*N-2))
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.br
> 0:  ZPPTRF or ZHPEV returned an error code:
.br
<= N:  if INFO = i, ZHPEV failed to converge;
i off-diagonal elements of an intermediate
tridiagonal form did not convergeto zero;
> N:   if INFO = N + i, for 1 <= i <= n, then the leading
minor of order i of B is not positive definite.
The factorization of B could not be completed and
no eigenvalues or eigenvectors were computed.
