.TH ZSYR2K l "16 October 1992" "BLAS routine" "BLAS routine"
.SH NAME
ZSYR2K - perform one of the symmetric rank 2k operations   C := alpha*A*B' + alpha*B*A' + beta*C,
.SH SYNOPSIS
.TP 19
SUBROUTINE ZSYR2K(
UPLO, TRANS, N, K, ALPHA, A, LDA, B, LDB,
BETA, C, LDC )
.TP 19
.ti +4
CHARACTER*1
UPLO, TRANS
.TP 19
.ti +4
INTEGER
N, K, LDA, LDB, LDC
.TP 19
.ti +4
COMPLEX*16
ALPHA, BETA
.TP 19
.ti +4
COMPLEX*16
A( LDA, * ), B( LDB, * ), C( LDC, * )
.SH PURPOSE
ZSYR2K  performs one of the symmetric rank 2k operations

or
.br

   C := alpha*A'*B + alpha*B'*A + beta*C,
.br

where  alpha and beta  are scalars,  C is an  n by n symmetric matrix
and  A and B  are  n by k  matrices  in the  first  case  and  k by n
matrices in the second case.
.br

.SH PARAMETERS
.TP 7
UPLO   - CHARACTER*1.
On  entry,   UPLO  specifies  whether  the  upper  or  lower
triangular  part  of the  array  C  is to be  referenced  as
follows:

UPLO = 'U' or 'u'   Only the  upper triangular part of  C
is to be referenced.

UPLO = 'L' or 'l'   Only the  lower triangular part of  C
is to be referenced.

Unchanged on exit.
.TP 7
TRANS  - CHARACTER*1.
On entry,  TRANS  specifies the operation to be performed as
follows:

TRANS = 'N' or 'n'    C := alpha*A*B' + alpha*B*A' +
beta*C.

TRANS = 'T' or 't'    C := alpha*A'*B + alpha*B'*A +
beta*C.

Unchanged on exit.
.TP 7
N      - INTEGER.
On entry,  N specifies the order of the matrix C.  N must be
at least zero.
Unchanged on exit.
.TP 7
K      - INTEGER.
On entry with  TRANS = 'N' or 'n',  K  specifies  the number
of  columns  of the  matrices  A and B,  and on  entry  with
TRANS = 'T' or 't',  K  specifies  the number of rows of the
matrices  A and B.  K must be at least zero.
Unchanged on exit.
.TP 7
ALPHA  - COMPLEX*16      .
On entry, ALPHA specifies the scalar alpha.
Unchanged on exit.
.TP 7
A      - COMPLEX*16       array of DIMENSION ( LDA, ka ), where ka is
k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
part of the array  A  must contain the matrix  A,  otherwise
the leading  k by n  part of the array  A  must contain  the
matrix A.
Unchanged on exit.
.TP 7
LDA    - INTEGER.
On entry, LDA specifies the first dimension of A as declared
in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
then  LDA must be at least  max( 1, n ), otherwise  LDA must
be at least  max( 1, k ).
Unchanged on exit.
.TP 7
B      - COMPLEX*16       array of DIMENSION ( LDB, kb ), where kb is
k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
part of the array  B  must contain the matrix  B,  otherwise
the leading  k by n  part of the array  B  must contain  the
matrix B.
Unchanged on exit.
.TP 7
LDB    - INTEGER.
On entry, LDB specifies the first dimension of B as declared
in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
then  LDB must be at least  max( 1, n ), otherwise  LDB must
be at least  max( 1, k ).
Unchanged on exit.
.TP 7
BETA   - COMPLEX*16      .
On entry, BETA specifies the scalar beta.
Unchanged on exit.
.TP 7
C      - COMPLEX*16       array of DIMENSION ( LDC, n ).
Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
upper triangular part of the array C must contain the upper
triangular part  of the  symmetric matrix  and the strictly
lower triangular part of C is not referenced.  On exit, the
upper triangular part of the array  C is overwritten by the
upper triangular part of the updated matrix.
Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
lower triangular part of the array C must contain the lower
triangular part  of the  symmetric matrix  and the strictly
upper triangular part of C is not referenced.  On exit, the
lower triangular part of the array  C is overwritten by the
lower triangular part of the updated matrix.
.TP 7
LDC    - INTEGER.
On entry, LDC specifies the first dimension of C as declared
in  the  calling  (sub)  program.   LDC  must  be  at  least
max( 1, n ).
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
