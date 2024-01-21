.TH SSYTRD l "08 October 1994" "LAPACK version 2.0" "LAPACK routine (version 2.0)"
.TH SSYTRD l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SSYTRD - reduce a real symmetric matrix A to real symmetric tridiagonal form T by an orthogonal similarity transformation
.SH SYNOPSIS
.TP 19
SUBROUTINE SSYTRD(
UPLO, N, A, LDA, D, E, TAU, WORK, LWORK, INFO )
.TP 19
.ti +4
CHARACTER
UPLO
.TP 19
.ti +4
INTEGER
INFO, LDA, LWORK, N
.TP 19
.ti +4
REAL
A( LDA, * ), D( * ), E( * ), TAU( * ),
WORK( * )
.SH PURPOSE
SSYTRD reduces a real symmetric matrix A to real symmetric
tridiagonal form T by an orthogonal similarity transformation:
Q**T * A * Q = T.
.br

.SH ARGUMENTS
.TP 8
UPLO    (input) CHARACTER*1
= 'U':  Upper triangle of A is stored;
.br
= 'L':  Lower triangle of A is stored.
.TP 8
N       (input) INTEGER
The order of the matrix A.  N >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the symmetric matrix A.  If UPLO = 'U', the leading
N-by-N upper triangular part of A contains the upper
triangular part of the matrix A, and the strictly lower
triangular part of A is not referenced.  If UPLO = 'L', the
leading N-by-N lower triangular part of A contains the lower
triangular part of the matrix A, and the strictly upper
triangular part of A is not referenced.
On exit, if UPLO = 'U', the diagonal and first superdiagonal
of A are overwritten by the corresponding elements of the
tridiagonal matrix T, and the elements above the first
superdiagonal, with the array TAU, represent the orthogonal
matrix Q as a product of elementary reflectors; if UPLO
= 'L', the diagonal and first subdiagonal of A are over-
written by the corresponding elements of the tridiagonal
matrix T, and the elements below the first subdiagonal, with
the array TAU, represent the orthogonal matrix Q as a product
of elementary reflectors. See Further Details.
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
D       (output) REAL array, dimension (N)
The diagonal elements of the tridiagonal matrix T:
D(i) = A(i,i).
.TP 8
E       (output) REAL array, dimension (N-1)
The off-diagonal elements of the tridiagonal matrix T:
E(i) = A(i,i+1) if UPLO = 'U', E(i) = A(i+1,i) if UPLO = 'L'.
.TP 8
TAU     (output) REAL array, dimension (N-1)
The scalar factors of the elementary reflectors (see Further
Details).
.TP 8
WORK    (workspace/output) REAL array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= 1.
For optimum performance LWORK >= N*NB, where NB is the
optimal blocksize.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value
.SH FURTHER DETAILS
If UPLO = 'U', the matrix Q is represented as a product of elementary
reflectors
.br

   Q = H(n-1) . . . H(2) H(1).
.br

Each H(i) has the form
.br

   H(i) = I - tau * v * v'
.br

where tau is a real scalar, and v is a real vector with
.br
v(i+1:n) = 0 and v(i) = 1; v(1:i-1) is stored on exit in
.br
A(1:i-1,i+1), and tau in TAU(i).
.br

If UPLO = 'L', the matrix Q is represented as a product of elementary
reflectors
.br

   Q = H(1) H(2) . . . H(n-1).
.br

Each H(i) has the form
.br

   H(i) = I - tau * v * v'
.br

where tau is a real scalar, and v is a real vector with
.br
v(1:i) = 0 and v(i+1) = 1; v(i+2:n) is stored on exit in A(i+2:n,i),
and tau in TAU(i).
.br

The contents of A on exit are illustrated by the following examples
with n = 5:
.br

if UPLO = 'U':                       if UPLO = 'L':
.br

  (  d   e   v2  v3  v4 )              (  d                  )
  (      d   e   v3  v4 )              (  e   d              )
  (          d   e   v4 )              (  v1  e   d          )
  (              d   e  )              (  v1  v2  e   d      )
  (                  d  )              (  v1  v2  v3  e   d  )

where d and e denote diagonal and off-diagonal elements of T, and vi
denotes an element of the vector defining H(i).
.br

