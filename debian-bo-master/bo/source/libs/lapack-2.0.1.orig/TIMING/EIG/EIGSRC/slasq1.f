      SUBROUTINE SLASQ1( N, D, E, WORK, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      REAL               D( * ), E( * ), WORK( * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is initialized to 0, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      REAL               ITCNT, OPS
*     ..
*
*     Purpose
*     =======
*
*     SLASQ1 computes the singular values of a real N-by-N bidiagonal
*     matrix with diagonal D and off-diagonal E. The singular values are
*     computed to high relative accuracy, barring over/underflow or
*     denormalization. The algorithm is described in
*
*     "Accurate singular values and differential qd algorithms," by
*     K. V. Fernando and B. N. Parlett,
*     Numer. Math., Vol-67, No. 2, pp. 191-230,1994.
*
*     See also
*     "Implementation of differential qd algorithms," by
*     K. V. Fernando and B. N. Parlett, Technical Report,
*     Department of Mathematics, University of California at Berkeley,
*     1994 (Under preparation).
*
*     Arguments
*     =========
*
*  N       (input) INTEGER
*          The number of rows and columns in the matrix. N >= 0.
*
*  D       (input/output) REAL array, dimension (N)
*          On entry, D contains the diagonal elements of the
*          bidiagonal matrix whose SVD is desired. On normal exit,
*          D contains the singular values in decreasing order.
*
*  E       (input/output) REAL array, dimension (N)
*          On entry, elements E(1:N-1) contain the off-diagonal elements
*          of the bidiagonal matrix whose SVD is desired.
*          On exit, E is overwritten.
*
*  WORK    (workspace) REAL array, dimension (2*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the algorithm did not converge;  i
*                specifies how many superdiagonals did not converge.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               MEIGTH
      PARAMETER          ( MEIGTH = -0.125E0 )
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E0 )
      REAL               ONE
      PARAMETER          ( ONE = 1.0E0 )
      REAL               TEN
      PARAMETER          ( TEN = 10.0E0 )
      REAL               HUNDRD
      PARAMETER          ( HUNDRD = 100.0E0 )
      REAL               TWO56
      PARAMETER          ( TWO56 = 256.0E0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            RESTRT
      INTEGER            I, IERR, J, KE, KEND, M, NY
      REAL               DM, DX, EPS, SCL, SFMIN, SIG1, SIG2, SIGMN,
     $                   SIGMX, SMALL2, THRESH, TOL, TOL2, TOLMUL
*     ..
*     .. External Functions ..
      REAL               SLAMCH
      EXTERNAL           SLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           SCOPY, SLAS2, SLASCL, SLASQ2, SLASRT, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, REAL, SQRT
*     ..
*     .. Executable Statements ..
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -2
         CALL XERBLA( 'SLASQ1', -INFO )
         RETURN
      ELSE IF( N.EQ.0 ) THEN
         RETURN
      ELSE IF( N.EQ.1 ) THEN
         D( 1 ) = ABS( D( 1 ) )
         RETURN
      ELSE IF( N.EQ.2 ) THEN
         OPS = OPS + 18
         CALL SLAS2( D( 1 ), E( 1 ), D( 2 ), SIGMN, SIGMX )
         D( 1 ) = SIGMX
         D( 2 ) = SIGMN
         RETURN
      END IF
*
*     Estimate the largest singular value
*
      SIGMX = ZERO
      DO 10 I = 1, N - 1
         SIGMX = MAX( SIGMX, ABS( E( I ) ) )
   10 CONTINUE
*
*     Early return if sigmx is zero (matrix is already diagonal)
*
      IF( SIGMX.EQ.ZERO )
     $   GO TO 70
*
      DO 20 I = 1, N
         D( I ) = ABS( D( I ) )
         SIGMX = MAX( SIGMX, D( I ) )
   20 CONTINUE
*
*     Get machine parameters
*
      EPS = SLAMCH( 'EPSILON' )
      SFMIN = SLAMCH( 'SAFE MINIMUM' )
*
*     Compute singular values to relative accuracy TOL
*     It is assumed that tol**2 does not underflow.
*
      OPS = OPS + 4*N + 10
      TOLMUL = MAX( TEN, MIN( HUNDRD, EPS**( -MEIGTH ) ) )
      TOL = TOLMUL*EPS
      TOL2 = TOL**2
*
      THRESH = SIGMX*SQRT( SFMIN )*TOL
*
*     Scale matrix so the square of the largest element is
*     1 / ( 256 * SFMIN )
*
      SCL = SQRT( ONE / ( TWO56*SFMIN ) )
      SMALL2 = ONE / ( TWO56*TOLMUL**2 )
      CALL SCOPY( N, D, 1, WORK( 1 ), 1 )
      CALL SCOPY( N-1, E, 1, WORK( N+1 ), 1 )
      CALL SLASCL( 'G', 0, 0, SIGMX, SCL, N, 1, WORK( 1 ), N, IERR )
      CALL SLASCL( 'G', 0, 0, SIGMX, SCL, N-1, 1, WORK( N+1 ), N-1,
     $             IERR )
*
*     Square D and E (the input for the qd algorithm)
*
      DO 30 J = 1, 2*N - 1
         WORK( J ) = WORK( J )**2
   30 CONTINUE
*
*     Apply qd algorithm
*
      M = 0
      E( N ) = ZERO
      DX = WORK( 1 )
      DM = DX
      KE = 0
      RESTRT = .FALSE.
      DO 60 I = 1, N
         OPS = OPS + 2
         IF( ABS( E( I ) ).LE.THRESH .OR. WORK( N+I ).LE.TOL2*
     $       ( DM / REAL( I-M ) ) ) THEN
            NY = I - M
            IF( NY.EQ.1 ) THEN
               GO TO 50
            ELSE IF( NY.EQ.2 ) THEN
               OPS = OPS + 18
               CALL SLAS2( D( M+1 ), E( M+1 ), D( M+2 ), SIG1, SIG2 )
               D( M+1 ) = SIG1
               D( M+2 ) = SIG2
            ELSE
               KEND = KE + 1 - M
               CALL SLASQ2( NY, D( M+1 ), E( M+1 ), WORK( M+1 ),
     $                      WORK( M+N+1 ), EPS, TOL2, SMALL2, DM, KEND,
     $                      INFO )
*
*                 Return, INFO = number of unconverged superdiagonals
*
               IF( INFO.NE.0 ) THEN
                  INFO = INFO + I
                  RETURN
               END IF
*
*                 Undo scaling
*
               OPS = OPS + 2*NY
               DO 40 J = M + 1, M + NY
                  D( J ) = SQRT( D( J ) )
   40          CONTINUE
               CALL SLASCL( 'G', 0, 0, SCL, SIGMX, NY, 1, D( M+1 ), NY,
     $                      IERR )
            END IF
   50       CONTINUE
            M = I
            IF( I.NE.N ) THEN
               DX = WORK( I+1 )
               DM = DX
               KE = I
               RESTRT = .TRUE.
            END IF
         END IF
         IF( I.NE.N .AND. .NOT.RESTRT ) THEN
            OPS = OPS + 3
            DX = WORK( I+1 )*( DX / ( DX+WORK( N+I ) ) )
            IF( DM.GT.DX ) THEN
               DM = DX
               KE = I
            END IF
         END IF
         RESTRT = .FALSE.
   60 CONTINUE
      KEND = KE + 1
*
*     Sort the singular values into decreasing order
*
   70 CONTINUE
      CALL SLASRT( 'D', N, D, INFO )
      RETURN
*
*     End of SLASQ1
*
      END
