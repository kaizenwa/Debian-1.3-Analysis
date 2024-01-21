      SUBROUTINE ZPTTRF( N, D, E, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * )
      COMPLEX*16         E( * )
*     ..
*
*  Purpose
*  =======
*
*  ZPTTRF computes the factorization of a complex Hermitian positive
*  definite tridiagonal matrix A.
*
*  If the subdiagonal elements of A are supplied in the array E, the
*  factorization has the form A = L*D*L**H, where D is diagonal and L
*  is unit lower bidiagonal; if the superdiagonal elements of A are
*  supplied, it has the form A = U**H*D*U, where U is unit upper
*  bidiagonal.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the n diagonal elements of the tridiagonal matrix
*          A.  On exit, the n diagonal elements of the diagonal matrix
*          D from the L*D*L**H factorization of A.
*
*  E       (input/output) COMPLEX*16 array, dimension (N-1)
*          On entry, the (n-1) off-diagonal elements of the tridiagonal
*          matrix A.  On exit, the (n-1) off-diagonal elements of the
*          unit bidiagonal factor L or U from the factorization of A.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the leading minor of order i is not
*                positive definite; if i < N, the factorization could
*                not be completed, while if i = N, the factorization was
*                completed, but D(N) = 0.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      DOUBLE PRECISION   DI, EII, EIR, F, G
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCMPLX, DIMAG
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -1
         CALL XERBLA( 'ZPTTRF', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Compute the L*D*L' (or U'*D*U) factorization of A.
*
      DO 10 I = 1, N - 1
*
*        Drop out of the loop if d(i) <= 0: the matrix is not positive
*        definite.
*
         DI = D( I )
         IF( DI.LE.ZERO )
     $      GO TO 20
*
*        Solve for e(i) and d(i+1).
*
         EIR = DBLE( E( I ) )
         EII = DIMAG( E( I ) )
         F = EIR / DI
         G = EII / DI
         E( I ) = DCMPLX( F, G )
         D( I+1 ) = D( I+1 ) - F*EIR - G*EII
   10 CONTINUE
*
*     Check d(n) for positive definiteness.
*
      I = N
      IF( D( I ).GT.ZERO )
     $   GO TO 30
*
   20 CONTINUE
      INFO = I
*
   30 CONTINUE
      RETURN
*
*     End of ZPTTRF
*
      END
