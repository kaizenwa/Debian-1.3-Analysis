      SUBROUTINE DLASQ4( N, Q, E, TAU, SUP )
*
*  -- LAPACK routine (instrumented to count operations, version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            N
      DOUBLE PRECISION   SUP, TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   E( * ), Q( * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is initialized to 0, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*     Purpose
*     =======
*
*     DLASQ4 estimates TAU, the smallest eigenvalue of a matrix. This
*     routine improves the input value of SUP which is an upper bound
*     for the smallest eigenvalue for this matrix .
*
*     Arguments
*     =========
*
*  N       (input) INTEGER
*          On entry, N specifies the number of rows and columns
*          in the matrix. N must be at least 0.
*
*  Q       (input) DOUBLE PRECISION array, dimension (N)
*          Q array
*
*  E       (input) DOUBLE PRECISION array, dimension (N)
*          E array
*
*  TAU     (output) DOUBLE PRECISION
*          Estimate of the shift
*
*  SUP     (input/output) DOUBLE PRECISION
*          Upper bound for the smallest singular value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
      DOUBLE PRECISION   BIS, BIS1
      PARAMETER          ( BIS = 0.9999D+0, BIS1 = 0.7D+0 )
      INTEGER            IFLMAX
      PARAMETER          ( IFLMAX = 5 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IFL
      DOUBLE PRECISION   D, DM, XINF
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
      IFL = 1
      SUP = MIN( SUP, Q( 1 ), Q( 2 ), Q( 3 ), Q( N ), Q( N-1 ),
     $      Q( N-2 ) )
      OPS = OPS + 1
      TAU = SUP*BIS
      XINF = ZERO
   10 CONTINUE
      IF( IFL.EQ.IFLMAX ) THEN
         TAU = XINF
         RETURN
      END IF
      OPS = OPS + 1
      D = Q( 1 ) - TAU
      DM = D
      DO 20 I = 1, N - 2
         OPS = OPS + 4
         D = ( D / ( D+E( I ) ) )*Q( I+1 ) - TAU
         IF( DM.GT.D )
     $      DM = D
         IF( D.LT.ZERO ) THEN
            SUP = TAU
            OPS = OPS + 3
            TAU = MAX( SUP*BIS1**IFL, D+TAU )
            IFL = IFL + 1
            GO TO 10
         END IF
   20 CONTINUE
      OPS = OPS + 4
      D = ( D / ( D+E( N-1 ) ) )*Q( N ) - TAU
      IF( DM.GT.D )
     $   DM = D
      IF( D.LT.ZERO ) THEN
         SUP = TAU
         OPS = OPS + 3
         XINF = MAX( XINF, D+TAU )
         IF( SUP*BIS1**IFL.LE.XINF ) THEN
            TAU = XINF
         ELSE
            OPS = OPS + 2
            TAU = SUP*BIS1**IFL
            IFL = IFL + 1
            GO TO 10
         END IF
      ELSE
         OPS = OPS + 1
         SUP = MIN( SUP, DM+TAU )
      END IF
      RETURN
*
*     End of DLASQ4
*
      END
