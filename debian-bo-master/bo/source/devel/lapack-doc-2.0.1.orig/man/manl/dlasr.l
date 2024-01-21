.TH DLASR l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH DLASR l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DLASR - perform the transformation   A := P*A, when SIDE = 'L' or 'l' ( Left-hand side )   A := A*P', when SIDE = 'R' or 'r' ( Right-hand side )  where A is an m by n real matrix and P is an orthogonal matrix,
.SH SYNOPSIS
.TP 18
SUBROUTINE DLASR(
SIDE, PIVOT, DIRECT, M, N, C, S, A, LDA )
.TP 18
.ti +4
CHARACTER
DIRECT, PIVOT, SIDE
.TP 18
.ti +4
INTEGER
LDA, M, N
.TP 18
.ti +4
DOUBLE
PRECISION A( LDA, * ), C( * ), S( * )
.SH PURPOSE
DLASR   performs the transformation
consisting of a sequence of plane rotations determined by the
parameters PIVOT and DIRECT as follows ( z = m when SIDE = 'L' or 'l'
and z = n when SIDE = 'R' or 'r' ):
.br

When  DIRECT = 'F' or 'f'  ( Forward sequence ) then
.br

   P = P( z - 1 )*...*P( 2 )*P( 1 ),
.br

and when DIRECT = 'B' or 'b'  ( Backward sequence ) then
.br

   P = P( 1 )*P( 2 )*...*P( z - 1 ),
.br

where  P( k ) is a plane rotation matrix for the following planes:

   when  PIVOT = 'V' or 'v'  ( Variable pivot ),
.br
      the plane ( k, k + 1 )
.br

   when  PIVOT = 'T' or 't'  ( Top pivot ),
.br
      the plane ( 1, k + 1 )
.br

   when  PIVOT = 'B' or 'b'  ( Bottom pivot ),
.br
      the plane ( k, z )
.br

c( k ) and s( k )  must contain the  cosine and sine that define the
matrix  P( k ).  The two by two plane rotation part of the matrix
P( k ), R( k ), is assumed to be of the form
.br

   R( k ) = (  c( k )  s( k ) ).
.br
            ( -s( k )  c( k ) )
.br

This version vectorises across rows of the array A when SIDE = 'L'.

.SH ARGUMENTS
.TP 8
SIDE    (input) CHARACTER*1
Specifies whether the plane rotation matrix P is applied to
A on the left or the right.
= 'L':  Left, compute A := P*A
.br
= 'R':  Right, compute A:= A*P'
.TP 8
DIRECT  (input) CHARACTER*1
Specifies whether P is a forward or backward sequence of
plane rotations.
= 'F':  Forward, P = P( z - 1 )*...*P( 2 )*P( 1 )
.br
= 'B':  Backward, P = P( 1 )*P( 2 )*...*P( z - 1 )
.TP 8
PIVOT   (input) CHARACTER*1
Specifies the plane for which P(k) is a plane rotation
matrix.
= 'V':  Variable pivot, the plane (k,k+1)
.br
= 'T':  Top pivot, the plane (1,k+1)
.br
= 'B':  Bottom pivot, the plane (k,z)
.TP 8
M       (input) INTEGER
The number of rows of the matrix A.  If m <= 1, an immediate
return is effected.
.TP 8
N       (input) INTEGER
The number of columns of the matrix A.  If n <= 1, an
immediate return is effected.

C, S    (input) DOUBLE PRECISION arrays, dimension
(M-1) if SIDE = 'L'
(N-1) if SIDE = 'R'
c(k) and s(k) contain the cosine and sine that define the
matrix P(k).  The two by two plane rotation part of the
matrix P(k), R(k), is assumed to be of the form
R( k ) = (  c( k )  s( k ) ).
( -s( k )  c( k ) )
.TP 8
A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
The m by n matrix A.  On exit, A is overwritten by P*A if
SIDE = 'R' or by A*P' if SIDE = 'L'.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,M).
