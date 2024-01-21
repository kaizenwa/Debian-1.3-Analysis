      SUBROUTINE SLASQ3( N, Q, E, QQ, EE, SUP, SIGMA, KEND, OFF, IPHASE,
     $                   ICONV, EPS, TOL2, SMALL2 )
*
*  -- LAPACK routine (instrumented to count operations, version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            ICONV, IPHASE, KEND, N, OFF
      REAL               EPS, SIGMA, SMALL2, SUP, TOL2
*     ..
*     .. Array Arguments ..
      REAL               E( * ), EE( * ), Q( * ), QQ( * )
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
*     SLASQ3 is the workhorse of the whole bidiagonal SVD algorithm.
*     This can be described as the differential qd with shifts.
*
*     Arguments
*     =========
*
*  N       (input/output) INTEGER
*          On entry, N specifies the number of rows and columns
*          in the matrix. N must be at least 3.
*          On exit N is non-negative and less than the input value.
*
*  Q       (input/output) REAL array, dimension (N)
*          Q array in ping (see IPHASE below)
*
*  E       (input/output) REAL array, dimension (N)
*          E array in ping (see IPHASE below)
*
*  QQ      (input/output) REAL array, dimension (N)
*          Q array in pong (see IPHASE below)
*
*  EE      (input/output) REAL array, dimension (N)
*          E array in pong (see IPHASE below)
*
*  SUP     (input/output) REAL
*          Upper bound for the smallest eigenvalue
*
*  SIGMA   (input/output) REAL
*          Accumulated shift for the present submatrix
*
*  KEND    (input/output) INTEGER
*          Index where minimum D(i) occurs in recurrence for
*          splitting criterion
*
*  OFF     (input/output) INTEGER
*          Offset for arrays
*
*  IPHASE  (input/output) INTEGER
*          If IPHASE = 1 (ping) then data is in Q and E arrays
*          If IPHASE = 2 (pong) then data is in QQ and EE arrays
*
*  ICONV   (input) INTEGER
*          If ICONV = 0 a bottom part of a matrix (with a split)
*          If ICONV =-3 a top part of a matrix (with a split)
*
*  EPS     (input) REAL
*          Machine epsilon
*
*  TOL2    (input) REAL
*          Square of the relative tolerance TOL as defined in SLASQ1
*
*  SMALL2  (input) REAL
*          A threshold value as defined in SLASQ1
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
      INTEGER            NPP
      PARAMETER          ( NPP = 32 )
      INTEGER            IPP
      PARAMETER          ( IPP = 5 )
      REAL               HALF, FOUR
      PARAMETER          ( HALF = 0.5E+0, FOUR = 4.0E+0 )
      INTEGER            IFLMAX
      PARAMETER          ( IFLMAX = 2 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LDEF, LSPLIT
      INTEGER            I, IC, ICNT, IFL, IP, ISP, K1END, K2END, KE,
     $                   KS, MAXIT, N1, N2
      REAL               D, DM, QEMAX, T1, TAU, TOLX, TOLY, TOLZ, XX, YY
*     ..
*     .. External Subroutines ..
      EXTERNAL           SCOPY, SLASQ4
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
      ICNT = 0
      TAU = ZERO
      DM = SUP
      OPS = OPS + 2
      TOLX = SIGMA*TOL2
      TOLZ = MAX( SMALL2, SIGMA )*TOL2
*
*     Set maximum number of iterations
*
      MAXIT = 100*N
*
*     Flipping
*
      IC = 2
      IF( N.GT.3 ) THEN
         IF( IPHASE.EQ.1 ) THEN
            DO 10 I = 1, N - 2
               IF( Q( I ).GT.Q( I+1 ) )
     $            IC = IC + 1
               IF( E( I ).GT.E( I+1 ) )
     $            IC = IC + 1
   10       CONTINUE
            IF( Q( N-1 ).GT.Q( N ) )
     $         IC = IC + 1
            IF( IC.LT.N ) THEN
               CALL SCOPY( N, Q, 1, QQ, -1 )
               CALL SCOPY( N-1, E, 1, EE, -1 )
               IF( KEND.NE.0 )
     $            KEND = N - KEND + 1
               IPHASE = 2
            END IF
         ELSE
            DO 20 I = 1, N - 2
               IF( QQ( I ).GT.QQ( I+1 ) )
     $            IC = IC + 1
               IF( EE( I ).GT.EE( I+1 ) )
     $            IC = IC + 1
   20       CONTINUE
            IF( QQ( N-1 ).GT.QQ( N ) )
     $         IC = IC + 1
            IF( IC.LT.N ) THEN
               CALL SCOPY( N, QQ, 1, Q, -1 )
               CALL SCOPY( N-1, EE, 1, E, -1 )
               IF( KEND.NE.0 )
     $            KEND = N - KEND + 1
               IPHASE = 1
            END IF
         END IF
      END IF
      IF( ICONV.EQ.-3 ) THEN
         IF( IPHASE.EQ.1 ) THEN
            GO TO 220
         ELSE
            GO TO 100
         END IF
      END IF
      IF( IPHASE.EQ.2 )
     $   GO TO 150
*
*     The ping section of the code
*
   30 CONTINUE
      IFL = 0
*
*     Compute the shift
*
      IF( KEND.EQ.0 .OR. SUP.EQ.ZERO ) THEN
         TAU = ZERO
      ELSE IF( ICNT.GT.0 .AND. DM.LE.TOLZ ) THEN
         TAU = ZERO
      ELSE
         IP = MAX( IPP, N / NPP )
         N2 = 2*IP + 1
         IF( N2.GE.N ) THEN
            N1 = 1
            N2 = N
         ELSE IF( KEND+IP.GT.N ) THEN
            N1 = N - 2*IP
         ELSE IF( KEND-IP.LT.1 ) THEN
            N1 = 1
         ELSE
            N1 = KEND - IP
         END IF
         CALL SLASQ4( N2, Q( N1 ), E( N1 ), TAU, SUP )
      END IF
   40 CONTINUE
      ICNT = ICNT + 1
      IF( ICNT.GT.MAXIT ) THEN
         SUP = -ONE
         RETURN
      END IF
      IF( TAU.EQ.ZERO ) THEN
*
*     dqd algorithm
*
         D = Q( 1 )
         DM = D
         KE = 0
         OPS = OPS + 3*N - 3
         DO 50 I = 1, N - 3
            QQ( I ) = D + E( I )
            D = ( D / QQ( I ) )*Q( I+1 )
            IF( DM.GT.D ) THEN
               DM = D
               KE = I
            END IF
   50    CONTINUE
         KE = KE + 1
*
*     Penultimate dqd step (in ping)
*
         K2END = KE
         QQ( N-2 ) = D + E( N-2 )
         D = ( D / QQ( N-2 ) )*Q( N-1 )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
         END IF
*
*     Final dqd step (in ping)
*
         K1END = KE
         QQ( N-1 ) = D + E( N-1 )
         D = ( D / QQ( N-1 ) )*Q( N )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         QQ( N ) = D
      ELSE
*
*     The dqds algorithm (in ping)
*
         D = Q( 1 ) - TAU
         DM = D
         KE = 0
         IF( D.LT.ZERO )
     $      GO TO 140
         OPS = OPS + 4*( N-3 )
         DO 60 I = 1, N - 3
            QQ( I ) = D + E( I )
            D = ( D / QQ( I ) )*Q( I+1 ) - TAU
            IF( DM.GT.D ) THEN
               DM = D
               KE = I
               IF( D.LT.ZERO )
     $            GO TO 70
            END IF
   60    CONTINUE
         KE = KE + 1
         GO TO 80
   70    CONTINUE
         OPS = OPS - 4*( N-3-KE )
         GO TO 140
   80    CONTINUE
*
*     Penultimate dqds step (in ping)
*
         OPS = OPS + 4
         K2END = KE
         QQ( N-2 ) = D + E( N-2 )
         D = ( D / QQ( N-2 ) )*Q( N-1 ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
            IF( D.LT.ZERO )
     $         GO TO 140
         END IF
*
*     Final dqds step (in ping)
*
         OPS = OPS + 4
         K1END = KE
         QQ( N-1 ) = D + E( N-1 )
         D = ( D / QQ( N-1 ) )*Q( N ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         QQ( N ) = D
      END IF
*
*        Convergence when QQ(N) is small (in ping)
*
      OPS = OPS + 1
      IF( ABS( QQ( N ) ).LE.SIGMA*TOL2 ) THEN
         QQ( N ) = ZERO
         DM = ZERO
         KE = N
      END IF
      IF( QQ( N ).LT.ZERO )
     $   GO TO 140
*
*     Non-negative qd array: Update the e's
*
      OPS = OPS + 2*( N-1 )
      DO 90 I = 1, N - 1
         EE( I ) = ( E( I ) / QQ( I ) )*Q( I+1 )
   90 CONTINUE
*
*     Updating sigma and iphase in ping
*
      OPS = OPS + 1
      SIGMA = SIGMA + TAU
      IPHASE = 2
  100 CONTINUE
      TOLX = SIGMA*TOL2
      TOLY = SIGMA*EPS
      TOLZ = MAX( SIGMA, SMALL2 )*TOL2
      OPS = OPS + 3
*
*     Checking for deflation and convergence (in ping)
*
  110 CONTINUE
      IF( N.LE.2 )
     $   RETURN
*
*        Deflation: bottom 1x1 (in ping)
*
      LDEF = .FALSE.
      IF( EE( N-1 ).LE.TOLZ ) THEN
         LDEF = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         OPS = OPS + 2
         IF( EE( N-1 ).LE.EPS*( SIGMA+QQ( N ) ) ) THEN
            OPS = OPS + 5
            IF( EE( N-1 )*( QQ( N ) / ( QQ( N )+SIGMA ) ).LE.TOL2*
     $          ( QQ( N )+SIGMA ) ) THEN
               LDEF = .TRUE.
            END IF
         END IF
      ELSE
         OPS = OPS + 1
         IF( EE( N-1 ).LE.QQ( N )*TOL2 ) THEN
            LDEF = .TRUE.
         END IF
      END IF
      IF( LDEF ) THEN
         OPS = OPS + 1
         Q( N ) = QQ( N ) + SIGMA
         N = N - 1
         ICONV = ICONV + 1
         GO TO 110
      END IF
*
*        Deflation: bottom 2x2 (in ping)
*
      LDEF = .FALSE.
      IF( EE( N-2 ).LE.TOLZ ) THEN
         LDEF = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         OPS = OPS + 7
         T1 = SIGMA + EE( N-1 )*( SIGMA / ( SIGMA+QQ( N ) ) )
         IF( EE( N-2 )*( T1 / ( QQ( N-1 )+T1 ) ).LE.TOLY ) THEN
            OPS = OPS + 3
            IF( EE( N-2 )*( QQ( N-1 ) / ( QQ( N-1 )+T1 ) ).LE.TOLX )
     $           THEN
               LDEF = .TRUE.
            END IF
         END IF
      ELSE
         OPS = OPS + 5
         IF( EE( N-2 ).LE.( QQ( N ) / ( QQ( N )+EE( N-1 )+QQ( N-1 ) ) )*
     $       QQ( N-1 )*TOL2 ) THEN
            LDEF = .TRUE.
         END IF
      END IF
      IF( LDEF ) THEN
         QEMAX = MAX( QQ( N ), QQ( N-1 ), EE( N-1 ) )
         IF( QEMAX.NE.ZERO ) THEN
            OPS = OPS + 15
            IF( QEMAX.EQ.QQ( N-1 ) ) THEN
               XX = HALF*( QQ( N )+QQ( N-1 )+EE( N-1 )+QEMAX*
     $              SQRT( ( ( QQ( N )-QQ( N-1 )+EE( N-1 ) ) /
     $              QEMAX )**2+FOUR*EE( N-1 ) / QEMAX ) )
            ELSE IF( QEMAX.EQ.QQ( N ) ) THEN
               XX = HALF*( QQ( N )+QQ( N-1 )+EE( N-1 )+QEMAX*
     $              SQRT( ( ( QQ( N-1 )-QQ( N )+EE( N-1 ) ) /
     $              QEMAX )**2+FOUR*EE( N-1 ) / QEMAX ) )
            ELSE
               XX = HALF*( QQ( N )+QQ( N-1 )+EE( N-1 )+QEMAX*
     $              SQRT( ( ( QQ( N )-QQ( N-1 )+EE( N-1 ) ) /
     $              QEMAX )**2+FOUR*QQ( N-1 ) / QEMAX ) )
            END IF
            YY = ( MAX( QQ( N ), QQ( N-1 ) ) / XX )*
     $           MIN( QQ( N ), QQ( N-1 ) )
         ELSE
            XX = ZERO
            YY = ZERO
         END IF
         OPS = OPS + 2
         Q( N-1 ) = SIGMA + XX
         Q( N ) = YY + SIGMA
         N = N - 2
         ICONV = ICONV + 2
         GO TO 110
      END IF
*
*     Updating bounds before going to pong
*
      IF( ICONV.EQ.0 ) THEN
         KEND = KE
         SUP = MIN( DM, SUP-TAU )
      ELSE IF( ICONV.GT.0 ) THEN
         SUP = MIN( QQ( N ), QQ( N-1 ), QQ( N-2 ), QQ( 1 ), QQ( 2 ),
     $         QQ( 3 ) )
         IF( ICONV.EQ.1 ) THEN
            KEND = K1END
         ELSE IF( ICONV.EQ.2 ) THEN
            KEND = K2END
         ELSE
            KEND = N
         END IF
         ICNT = 0
         MAXIT = 100*N
      END IF
*
*     Checking for splitting in ping
*
      LSPLIT = .FALSE.
      DO 120 KS = N - 3, 3, -1
         IF( EE( KS ).LE.TOLY ) THEN
            OPS = OPS + 3
            IF( EE( KS )*( MIN( QQ( KS+1 ),
     $          QQ( KS ) ) / ( MIN( QQ( KS+1 ), QQ( KS ) )+SIGMA ) ).LE.
     $          TOLX ) THEN
               LSPLIT = .TRUE.
               GO TO 130
            END IF
         END IF
  120 CONTINUE
*
      KS = 2
      IF( EE( 2 ).LE.TOLZ ) THEN
         LSPLIT = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         OPS = OPS + 7
         T1 = SIGMA + EE( 1 )*( SIGMA / ( SIGMA+QQ( 1 ) ) )
         IF( EE( 2 )*( T1 / ( QQ( 1 )+T1 ) ).LE.TOLY ) THEN
            OPS = OPS + 3
            IF( EE( 2 )*( QQ( 1 ) / ( QQ( 1 )+T1 ) ).LE.TOLX ) THEN
               LSPLIT = .TRUE.
            END IF
         END IF
      ELSE
         OPS = OPS + 5
         IF( EE( 2 ).LE.( QQ( 1 ) / ( QQ( 1 )+EE( 1 )+QQ( 2 ) ) )*
     $       QQ( 2 )*TOL2 ) THEN
            LSPLIT = .TRUE.
         END IF
      END IF
      IF( LSPLIT )
     $   GO TO 130
*
      KS = 1
      IF( EE( 1 ).LE.TOLZ ) THEN
         LSPLIT = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         OPS = OPS + 2
         IF( EE( 1 ).LE.EPS*( SIGMA+QQ( 1 ) ) ) THEN
            OPS = OPS + 5
            IF( EE( 1 )*( QQ( 1 ) / ( QQ( 1 )+SIGMA ) ).LE.TOL2*
     $          ( QQ( 1 )+SIGMA ) ) THEN
               LSPLIT = .TRUE.
            END IF
         END IF
      ELSE
         OPS = OPS + 1
         IF( EE( 1 ).LE.QQ( 1 )*TOL2 ) THEN
            LSPLIT = .TRUE.
         END IF
      END IF
*
  130 CONTINUE
      IF( LSPLIT ) THEN
         SUP = MIN( QQ( N ), QQ( N-1 ), QQ( N-2 ) )
         ISP = -( OFF+1 )
         OFF = OFF + KS
         N = N - KS
         KEND = MAX( 1, KEND-KS )
         E( KS ) = SIGMA
         EE( KS ) = ISP
         ICONV = 0
         RETURN
      END IF
*
*     Coincidence
*
      IF( TAU.EQ.ZERO .AND. DM.LE.TOLZ .AND. KEND.NE.N .AND. ICONV.EQ.
     $    0 .AND. ICNT.GT.0 ) THEN
         CALL SCOPY( N-KE, E( KE ), 1, QQ( KE ), 1 )
         QQ( N ) = ZERO
         CALL SCOPY( N-KE, Q( KE+1 ), 1, EE( KE ), 1 )
         SUP = ZERO
      END IF
      ICONV = 0
      GO TO 150
*
*     A new shift when the previous failed (in ping)
*
  140 CONTINUE
      IFL = IFL + 1
      SUP = TAU
*
*     SUP is small or
*     Too many bad shifts (ping)
*
      IF( SUP.LE.TOLZ .OR. IFL.GE.IFLMAX ) THEN
         TAU = ZERO
         GO TO 40
*
*     The asymptotic shift (in ping)
*
      ELSE
         TAU = MAX( TAU+D, ZERO )
         IF( TAU.LE.TOLZ )
     $      TAU = ZERO
         GO TO 40
      END IF
*
*     the pong section of the code
*
  150 CONTINUE
      IFL = 0
*
*     Compute the shift (in pong)
*
      IF( KEND.EQ.0 .AND. SUP.EQ.ZERO ) THEN
         TAU = ZERO
      ELSE IF( ICNT.GT.0 .AND. DM.LE.TOLZ ) THEN
         TAU = ZERO
      ELSE
         IP = MAX( IPP, N / NPP )
         N2 = 2*IP + 1
         IF( N2.GE.N ) THEN
            N1 = 1
            N2 = N
         ELSE IF( KEND+IP.GT.N ) THEN
            N1 = N - 2*IP
         ELSE IF( KEND-IP.LT.1 ) THEN
            N1 = 1
         ELSE
            N1 = KEND - IP
         END IF
         CALL SLASQ4( N2, QQ( N1 ), EE( N1 ), TAU, SUP )
      END IF
  160 CONTINUE
      ICNT = ICNT + 1
      IF( ICNT.GT.MAXIT ) THEN
         SUP = -SUP
         RETURN
      END IF
      IF( TAU.EQ.ZERO ) THEN
*
*     The dqd algorithm (in pong)
*
         D = QQ( 1 )
         DM = D
         KE = 0
         OPS = OPS + 3*N - 3
         DO 170 I = 1, N - 3
            Q( I ) = D + EE( I )
            D = ( D / Q( I ) )*QQ( I+1 )
            IF( DM.GT.D ) THEN
               DM = D
               KE = I
            END IF
  170    CONTINUE
         KE = KE + 1
*
*     Penultimate dqd step (in pong)
*
         K2END = KE
         Q( N-2 ) = D + EE( N-2 )
         D = ( D / Q( N-2 ) )*QQ( N-1 )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
         END IF
*
*     Final dqd step (in pong)
*
         K1END = KE
         Q( N-1 ) = D + EE( N-1 )
         D = ( D / Q( N-1 ) )*QQ( N )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         Q( N ) = D
      ELSE
*
*     The dqds algorithm (in pong)
*
         OPS = OPS + 1 + 4*( N-3 )
         D = QQ( 1 ) - TAU
         DM = D
         KE = 0
         IF( D.LT.ZERO )
     $      GO TO 260
         DO 180 I = 1, N - 3
            Q( I ) = D + EE( I )
            D = ( D / Q( I ) )*QQ( I+1 ) - TAU
            IF( DM.GT.D ) THEN
               DM = D
               KE = I
               IF( D.LT.ZERO )
     $            GO TO 190
            END IF
  180    CONTINUE
         KE = KE + 1
         GO TO 200
  190    CONTINUE
         OPS = OPS - 4*( N-3-KE )
         GO TO 260
  200    CONTINUE
*
*     Penultimate dqds step (in pong)
*
         K2END = KE
         OPS = OPS + 4
         Q( N-2 ) = D + EE( N-2 )
         D = ( D / Q( N-2 ) )*QQ( N-1 ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
            IF( D.LT.ZERO )
     $         GO TO 260
         END IF
*
*     Final dqds step (in pong)
*
         K1END = KE
         OPS = OPS + 4
         Q( N-1 ) = D + EE( N-1 )
         D = ( D / Q( N-1 ) )*QQ( N ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         Q( N ) = D
      END IF
*
*        Convergence when is small (in pong)
*
      OPS = OPS + 1
      IF( ABS( Q( N ) ).LE.SIGMA*TOL2 ) THEN
         Q( N ) = ZERO
         DM = ZERO
         KE = N
      END IF
      IF( Q( N ).LT.ZERO )
     $   GO TO 260
*
*     Non-negative qd array: Update the e's
*
      OPS = OPS + 2*N - 1
      DO 210 I = 1, N - 1
         E( I ) = ( EE( I ) / Q( I ) )*QQ( I+1 )
  210 CONTINUE
*
*     Updating sigma and iphase in pong
*
      SIGMA = SIGMA + TAU
  220 CONTINUE
      IPHASE = 1
      OPS = OPS + 2
      TOLX = SIGMA*TOL2
      TOLY = SIGMA*EPS
*
*     Checking for deflation and convergence (in pong)
*
  230 CONTINUE
      IF( N.LE.2 )
     $   RETURN
*
*        Deflation: bottom 1x1 (in pong)
*
      LDEF = .FALSE.
      IF( E( N-1 ).LE.TOLZ ) THEN
         LDEF = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         OPS = OPS + 2
         IF( E( N-1 ).LE.EPS*( SIGMA+Q( N ) ) ) THEN
            OPS = OPS + 5
            IF( E( N-1 )*( Q( N ) / ( Q( N )+SIGMA ) ).LE.TOL2*
     $          ( Q( N )+SIGMA ) ) THEN
               LDEF = .TRUE.
            END IF
         END IF
      ELSE
         OPS = OPS + 1
         IF( E( N-1 ).LE.Q( N )*TOL2 ) THEN
            LDEF = .TRUE.
         END IF
      END IF
      IF( LDEF ) THEN
         OPS = OPS + 1
         Q( N ) = Q( N ) + SIGMA
         N = N - 1
         ICONV = ICONV + 1
         GO TO 230
      END IF
*
*        Deflation: bottom 2x2 (in pong)
*
      LDEF = .FALSE.
      IF( E( N-2 ).LE.TOLZ ) THEN
         LDEF = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         OPS = OPS + 7
         T1 = SIGMA + E( N-1 )*( SIGMA / ( SIGMA+Q( N ) ) )
         IF( E( N-2 )*( T1 / ( Q( N-1 )+T1 ) ).LE.TOLY ) THEN
            OPS = OPS + 3
            IF( E( N-2 )*( Q( N-1 ) / ( Q( N-1 )+T1 ) ).LE.TOLX ) THEN
               LDEF = .TRUE.
            END IF
         END IF
      ELSE
         OPS = OPS + 5
         IF( E( N-2 ).LE.( Q( N ) / ( Q( N )+EE( N-1 )+Q( N-1 ) )*Q( N-
     $       1 ) )*TOL2 ) THEN
            LDEF = .TRUE.
         END IF
      END IF
      IF( LDEF ) THEN
         QEMAX = MAX( Q( N ), Q( N-1 ), E( N-1 ) )
         IF( QEMAX.NE.ZERO ) THEN
            OPS = OPS + 15
            IF( QEMAX.EQ.Q( N-1 ) ) THEN
               XX = HALF*( Q( N )+Q( N-1 )+E( N-1 )+QEMAX*
     $              SQRT( ( ( Q( N )-Q( N-1 )+E( N-1 ) ) / QEMAX )**2+
     $              FOUR*E( N-1 ) / QEMAX ) )
            ELSE IF( QEMAX.EQ.Q( N ) ) THEN
               XX = HALF*( Q( N )+Q( N-1 )+E( N-1 )+QEMAX*
     $              SQRT( ( ( Q( N-1 )-Q( N )+E( N-1 ) ) / QEMAX )**2+
     $              FOUR*E( N-1 ) / QEMAX ) )
            ELSE
               XX = HALF*( Q( N )+Q( N-1 )+E( N-1 )+QEMAX*
     $              SQRT( ( ( Q( N )-Q( N-1 )+E( N-1 ) ) / QEMAX )**2+
     $              FOUR*Q( N-1 ) / QEMAX ) )
            END IF
            YY = ( MAX( Q( N ), Q( N-1 ) ) / XX )*
     $           MIN( Q( N ), Q( N-1 ) )
         ELSE
            XX = ZERO
            YY = ZERO
         END IF
         OPS = OPS + 2
         Q( N-1 ) = SIGMA + XX
         Q( N ) = YY + SIGMA
         N = N - 2
         ICONV = ICONV + 2
         GO TO 230
      END IF
*
*     Updating bounds before going to pong
*
      IF( ICONV.EQ.0 ) THEN
         KEND = KE
         SUP = MIN( DM, SUP-TAU )
      ELSE IF( ICONV.GT.0 ) THEN
         SUP = MIN( Q( N ), Q( N-1 ), Q( N-2 ), Q( 1 ), Q( 2 ), Q( 3 ) )
         IF( ICONV.EQ.1 ) THEN
            KEND = K1END
         ELSE IF( ICONV.EQ.2 ) THEN
            KEND = K2END
         ELSE
            KEND = N
         END IF
         ICNT = 0
         MAXIT = 100*N
      END IF
*
*     Checking for splitting in pong
*
      LSPLIT = .FALSE.
      DO 240 KS = N - 3, 3, -1
         OPS = OPS + 3
         IF( E( KS ).LE.TOLY ) THEN
            IF( E( KS )*( MIN( Q( KS+1 ), Q( KS ) ) / ( MIN( Q( KS+1 ),
     $          Q( KS ) )+SIGMA ) ).LE.TOLX ) THEN
               LSPLIT = .TRUE.
               GO TO 250
            END IF
         END IF
  240 CONTINUE
*
      KS = 2
      IF( E( 2 ).LE.TOLZ ) THEN
         LSPLIT = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         OPS = OPS + 7
         T1 = SIGMA + E( 1 )*( SIGMA / ( SIGMA+Q( 1 ) ) )
         IF( E( 2 )*( T1 / ( Q( 1 )+T1 ) ).LE.TOLY ) THEN
            OPS = OPS + 3
            IF( E( 2 )*( Q( 1 ) / ( Q( 1 )+T1 ) ).LE.TOLX ) THEN
               LSPLIT = .TRUE.
            END IF
         END IF
      ELSE
         OPS = OPS + 5
         IF( E( 2 ).LE.( Q( 1 ) / ( Q( 1 )+E( 1 )+Q( 2 ) ) )*Q( 2 )*
     $       TOL2 ) THEN
            LSPLIT = .TRUE.
         END IF
      END IF
      IF( LSPLIT )
     $   GO TO 250
*
      KS = 1
      IF( E( 1 ).LE.TOLZ ) THEN
         LSPLIT = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         OPS = OPS + 2
         IF( E( 1 ).LE.EPS*( SIGMA+Q( 1 ) ) ) THEN
            OPS = OPS + 5
            IF( E( 1 )*( Q( 1 ) / ( Q( 1 )+SIGMA ) ).LE.TOL2*
     $          ( Q( 1 )+SIGMA ) ) THEN
               LSPLIT = .TRUE.
            END IF
         END IF
      ELSE
         OPS = OPS + 1
         IF( E( 1 ).LE.Q( 1 )*TOL2 ) THEN
            LSPLIT = .TRUE.
         END IF
      END IF
*
  250 CONTINUE
      IF( LSPLIT ) THEN
         SUP = MIN( Q( N ), Q( N-1 ), Q( N-2 ) )
         ISP = OFF + 1
         OFF = OFF + KS
         KEND = MAX( 1, KEND-KS )
         N = N - KS
         E( KS ) = SIGMA
         EE( KS ) = ISP
         ICONV = 0
         RETURN
      END IF
*
*     Coincidence
*
      IF( TAU.EQ.ZERO .AND. DM.LE.TOLZ .AND. KEND.NE.N .AND. ICONV.EQ.
     $    0 .AND. ICNT.GT.0 ) THEN
         CALL SCOPY( N-KE, EE( KE ), 1, Q( KE ), 1 )
         Q( N ) = ZERO
         CALL SCOPY( N-KE, QQ( KE+1 ), 1, E( KE ), 1 )
         SUP = ZERO
      END IF
      ICONV = 0
      GO TO 30
*
*     Computation of a new shift when the previous failed (in pong)
*
  260 CONTINUE
      IFL = IFL + 1
      SUP = TAU
*
*     SUP is small or
*     Too many bad shifts (in pong)
*
      IF( SUP.LE.TOLZ .OR. IFL.GE.IFLMAX ) THEN
         TAU = ZERO
         GO TO 160
*
*     The asymptotic shift (in pong)
*
      ELSE
         OPS = OPS + 1
         TAU = MAX( TAU+D, ZERO )
         IF( TAU.LE.TOLZ )
     $      TAU = ZERO
         GO TO 160
      END IF
*
*     End of SLASQ3
*
      END
