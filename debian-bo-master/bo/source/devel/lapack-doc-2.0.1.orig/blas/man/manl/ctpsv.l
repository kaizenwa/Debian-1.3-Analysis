.TH CTPSV l "16 October 1992" "BLAS routine" "BLAS routine"
.SH NAME
CTPSV - solve one of the systems of equations   A*x = b, or A'*x = b, or conjg( A' )*x = b,
.SH SYNOPSIS
.TP 17
SUBROUTINE CTPSV
( UPLO, TRANS, DIAG, N, AP, X, INCX )
.TP 17
.ti +4
INTEGER
INCX, N
.TP 17
.ti +4
CHARACTER*1
DIAG, TRANS, UPLO
.TP 17
.ti +4
COMPLEX
AP( * ), X( * )
.SH PURPOSE
CTPSV  solves one of the systems of equations

where b and x are n element vectors and A is an n by n unit, or
non-unit, upper or lower triangular matrix, supplied in packed form.

No test for singularity or near-singularity is included in this
routine. Such tests must be performed before calling this routine.

.SH PARAMETERS
.TP 7
UPLO   - CHARACTER*1.
On entry, UPLO specifies whether the matrix is an upper or
lower triangular matrix as follows:

UPLO = 'U' or 'u'   A is an upper triangular matrix.

UPLO = 'L' or 'l'   A is a lower triangular matrix.

Unchanged on exit.
.TP 7
TRANS  - CHARACTER*1.
On entry, TRANS specifies the equations to be solved as
follows:

TRANS = 'N' or 'n'   A*x = b.

TRANS = 'T' or 't'   A'*x = b.

TRANS = 'C' or 'c'   conjg( A' )*x = b.

Unchanged on exit.
.TP 7
DIAG   - CHARACTER*1.
On entry, DIAG specifies whether or not A is unit
triangular as follows:

DIAG = 'U' or 'u'   A is assumed to be unit triangular.

DIAG = 'N' or 'n'   A is not assumed to be unit
triangular.

Unchanged on exit.
.TP 7
N      - INTEGER.
On entry, N specifies the order of the matrix A.
N must be at least zero.
Unchanged on exit.
.TP 7
AP     - COMPLEX          array of DIMENSION at least
( ( n*( n + 1 ) )/2 ).
Before entry with  UPLO = 'U' or 'u', the array AP must
contain the upper triangular matrix packed sequentially,
column by column, so that AP( 1 ) contains a( 1, 1 ),
AP( 2 ) and AP( 3 ) contain a( 1, 2 ) and a( 2, 2 )
respectively, and so on.
Before entry with UPLO = 'L' or 'l', the array AP must
contain the lower triangular matrix packed sequentially,
column by column, so that AP( 1 ) contains a( 1, 1 ),
AP( 2 ) and AP( 3 ) contain a( 2, 1 ) and a( 3, 1 )
respectively, and so on.
Note that when  DIAG = 'U' or 'u', the diagonal elements of
A are not referenced, but are assumed to be unity.
Unchanged on exit.
.TP 7
X      - COMPLEX          array of dimension at least
( 1 + ( n - 1 )*abs( INCX ) ).
Before entry, the incremented array X must contain the n
element right-hand side vector b. On exit, X is overwritten
with the solution vector x.
.TP 7
INCX   - INTEGER.
On entry, INCX specifies the increment for the elements of
X. INCX must not be zero.
Unchanged on exit.

Level 2 Blas routine.

-- Written on 22-October-1986.
Jack Dongarra, Argonne National Lab.
Jeremy Du Croz, Nag Central Office.
Sven Hammarling, Nag Central Office.
Richard Hanson, Sandia National Labs.
