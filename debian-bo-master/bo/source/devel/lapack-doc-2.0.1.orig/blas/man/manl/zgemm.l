.TH ZGEMM l "16 October 1992" "BLAS routine" "BLAS routine"
.SH NAME
ZGEMM - perform one of the matrix-matrix operations   C := alpha*op( A )*op( B ) + beta*C,
.SH SYNOPSIS
.TP 17
SUBROUTINE ZGEMM
( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
BETA, C, LDC )
.TP 17
.ti +4
CHARACTER*1
TRANSA, TRANSB
.TP 17
.ti +4
INTEGER
M, N, K, LDA, LDB, LDC
.TP 17
.ti +4
COMPLEX*16
ALPHA, BETA
.TP 17
.ti +4
COMPLEX*16
A( LDA, * ), B( LDB, * ), C( LDC, * )
.SH PURPOSE
ZGEMM  performs one of the matrix-matrix operations

where  op( X ) is one of
.br

   op( X ) = X   or   op( X ) = X'   or   op( X ) = conjg( X' ),

alpha and beta are scalars, and A, B and C are matrices, with op( A )
an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.

.SH PARAMETERS

TRANSA - CHARACTER*1.
On entry, TRANSA specifies the form of op( A ) to be used in
the matrix multiplication as follows:

TRANSA = 'N' or 'n',  op( A ) = A.

TRANSA = 'T' or 't',  op( A ) = A'.

TRANSA = 'C' or 'c',  op( A ) = conjg( A' ).

Unchanged on exit.

TRANSB - CHARACTER*1.
On entry, TRANSB specifies the form of op( B ) to be used in
the matrix multiplication as follows:

TRANSB = 'N' or 'n',  op( B ) = B.

TRANSB = 'T' or 't',  op( B ) = B'.

TRANSB = 'C' or 'c',  op( B ) = conjg( B' ).

Unchanged on exit.
.TP 7
M      - INTEGER.
On entry,  M  specifies  the number  of rows  of the  matrix
op( A )  and of the  matrix  C.  M  must  be at least  zero.
Unchanged on exit.
.TP 7
N      - INTEGER.
On entry,  N  specifies the number  of columns of the matrix
op( B ) and the number of columns of the matrix C. N must be
at least zero.
Unchanged on exit.
.TP 7
K      - INTEGER.
On entry,  K  specifies  the number of columns of the matrix
op( A ) and the number of rows of the matrix op( B ). K must
be at least  zero.
Unchanged on exit.
.TP 7
ALPHA  - COMPLEX*16      .
On entry, ALPHA specifies the scalar alpha.
Unchanged on exit.
.TP 7
A      - COMPLEX*16       array of DIMENSION ( LDA, ka ), where ka is
k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
part of the array  A  must contain the matrix  A,  otherwise
the leading  k by m  part of the array  A  must contain  the
matrix A.
Unchanged on exit.
.TP 7
LDA    - INTEGER.
On entry, LDA specifies the first dimension of A as declared
in the calling (sub) program. When  TRANSA = 'N' or 'n' then
LDA must be at least  max( 1, m ), otherwise  LDA must be at
least  max( 1, k ).
Unchanged on exit.
.TP 7
B      - COMPLEX*16       array of DIMENSION ( LDB, kb ), where kb is
n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
part of the array  B  must contain the matrix  B,  otherwise
the leading  n by k  part of the array  B  must contain  the
matrix B.
Unchanged on exit.
.TP 7
LDB    - INTEGER.
On entry, LDB specifies the first dimension of B as declared
in the calling (sub) program. When  TRANSB = 'N' or 'n' then
LDB must be at least  max( 1, k ), otherwise  LDB must be at
least  max( 1, n ).
Unchanged on exit.
.TP 7
BETA   - COMPLEX*16      .
On entry,  BETA  specifies the scalar  beta.  When  BETA  is
supplied as zero then C need not be set on input.
Unchanged on exit.
.TP 7
C      - COMPLEX*16       array of DIMENSION ( LDC, n ).
Before entry, the leading  m by n  part of the array  C must
contain the matrix  C,  except when  beta  is zero, in which
case C need not be set on entry.
On exit, the array  C  is overwritten by the  m by n  matrix
( alpha*op( A )*op( B ) + beta*C ).
.TP 7
LDC    - INTEGER.
On entry, LDC specifies the first dimension of C as declared
in  the  calling  (sub)  program.   LDC  must  be  at  least
max( 1, m ).
Unchanged on exit.

Level 3 Blas routine.

-- Written on 8-February-1989.
Jack Dongarra, Argonne National Laboratory.
Iain Duff, AERE Harwell.
Jeremy Du Croz, Numerical Algorithms Group Ltd.
Sven Hammarling, Numerical Algorithms Group Ltd.

.. External Functions ..

.. External Subroutines ..

.. Intrinsic Functions ..

.. Local Scalars ..
