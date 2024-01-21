      SUBROUTINE SERRGG( PATH, NUNIT )
*
*  -- LAPACK test routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  Purpose
*  =======
*
*  SERRGG tests the error exits for SGGGLM, SGGHRD, SGGLSE, SGGQRF,
*  SGGRQF, SGGSVD, SGGSVP, SHGEQZ, STGEVC, and STGSJA.
*
*  Arguments
*  =========
*
*  PATH    (input) CHARACTER*3
*          The LAPACK path name for the routines to be tested.
*
*  NUNIT   (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NMAX, LW
      PARAMETER          ( NMAX = 3, LW = 6*NMAX )
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            DUMMYK, DUMMYL, I, INFO, J, M, NCYCLE, NT
      REAL               TOLA, TOLB
*     ..
*     .. Local Arrays ..
      LOGICAL            SEL( NMAX )
      INTEGER            IW( NMAX )
      REAL               A( NMAX, NMAX ), B( NMAX, NMAX ),
     $                   Q( NMAX, NMAX ), R1( NMAX ), R2( NMAX ),
     $                   R3( NMAX ), TAU( NMAX ), U( NMAX, NMAX ),
     $                   V( NMAX, NMAX ), W( LW ), Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, SGGGLM, SGGHRD, SGGLSE, SGGQRF, SGGRQF,
     $                   SGGSVD, SGGSVP, SHGEQZ, STGEVC, STGSJA
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*6        SRNAMT
      INTEGER            INFOT, NOUT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NOUT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
      C2 = PATH( 2: 3 )
*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = ZERO
            B( I, J ) = ZERO
   10    CONTINUE
   20 CONTINUE
      DO 30 I = 1, NMAX
         A( I, I ) = ONE
         B( I, I ) = ONE
   30 CONTINUE
      OK = .TRUE.
      TOLA = 1.0E0
      TOLB = 1.0E0
      NT = 0
*
*     Test error exits for the GG path.
*
      IF( LSAMEN( 2, C2, 'GG' ) ) THEN
*
*        SGGHRD
*
         SRNAMT = 'SGGHRD'
         INFOT = 1
         CALL SGGHRD( '/', 'N', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGGHRD( 'N', '/', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGGHRD( 'N', 'N', -1, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SGGHRD( 'N', 'N', 0, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SGGHRD( 'N', 'N', 0, 1, 1, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL SGGHRD( 'N', 'N', 2, 1, 1, A, 1, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL SGGHRD( 'N', 'N', 2, 1, 1, A, 2, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL SGGHRD( 'V', 'N', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL SGGHRD( 'N', 'V', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        SHGEQZ
*
         SRNAMT = 'SHGEQZ'
         INFOT = 1
         CALL SHGEQZ( '/', 'N', 'N', 0, 1, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SHGEQZ( 'E', '/', 'N', 0, 1, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SHGEQZ( 'E', 'N', '/', 0, 1, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SHGEQZ( 'E', 'N', 'N', -1, 0, 0, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SHGEQZ( 'E', 'N', 'N', 0, 0, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SHGEQZ( 'E', 'N', 'N', 0, 1, 1, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL SHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 1, B, 2, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL SHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 2, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL SHGEQZ( 'E', 'V', 'N', 2, 1, 1, A, 2, B, 2, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL SHGEQZ( 'E', 'N', 'V', 2, 1, 1, A, 2, B, 2, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        STGEVC
*
         SRNAMT = 'STGEVC'
         INFOT = 1
         CALL STGEVC( '/', 'A', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL STGEVC( 'R', '/', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL STGEVC( 'R', 'A', SEL, -1, A, 1, B, 1, Q, 1, Z, 1, 0, M,
     $                W, INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL STGEVC( 'R', 'A', SEL, 2, A, 1, B, 2, Q, 1, Z, 2, 0, M, W,
     $                INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL STGEVC( 'R', 'A', SEL, 2, A, 2, B, 1, Q, 1, Z, 2, 0, M, W,
     $                INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL STGEVC( 'L', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL STGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL STGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 2, 1, M, W,
     $                INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the GSV path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GSV' ) ) THEN
*
*        SGGSVD
*
         SRNAMT = 'SGGSVD'
         INFOT = 1
         CALL SGGSVD( '/', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGGSVD( 'N', '/', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGGSVD( 'N', 'N', '/', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SGGSVD( 'N', 'N', 'N', -1, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SGGSVD( 'N', 'N', 'N', 0, -1, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SGGSVD( 'N', 'N', 'N', 0, 0, -1, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL SGGSVD( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 0, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL SGGSVD( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                0, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL SGGSVD( 'U', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 0, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL SGGSVD( 'N', 'V', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 0, Q, 1, W, IW, INFO )
         CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL SGGSVD( 'N', 'N', 'Q', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 0, W, IW, INFO )
         CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        SGGSVP
*
         SRNAMT = 'SGGSVP'
         INFOT = 1
         CALL SGGSVP( '/', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGGSVP( 'N', '/', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGGSVP( 'N', 'N', '/', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SGGSVP( 'N', 'N', 'N', -1, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SGGSVP( 'N', 'N', 'N', 0, -1, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SGGSVP( 'N', 'N', 'N', 0, 0, -1, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL SGGSVP( 'N', 'N', 'N', 0, 0, 0, A, 0, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL SGGSVP( 'N', 'N', 'N', 0, 0, 0, A, 1, B, 0, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL SGGSVP( 'U', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 0, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL SGGSVP( 'N', 'V', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 0, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL SGGSVP( 'N', 'N', 'Q', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 0, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        STGSJA
*
         SRNAMT = 'STGSJA'
         INFOT = 1
         CALL STGSJA( '/', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL STGSJA( 'N', '/', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL STGSJA( 'N', 'N', '/', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL STGSJA( 'N', 'N', 'N', -1, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL STGSJA( 'N', 'N', 'N', 0, -1, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL STGSJA( 'N', 'N', 'N', 0, 0, -1, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL STGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 0, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL STGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                0, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL STGSJA( 'U', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 0, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL STGSJA( 'N', 'V', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 0, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL STGSJA( 'N', 'N', 'Q', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 0, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*     Test error exits for the GLM path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GLM' ) ) THEN
*
*        SGGGLM
*
         SRNAMT = 'SGGGLM'
         INFOT = 1
         CALL SGGGLM( -1, 0, 0, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'SGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGGGLM( 0, -1, 0, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'SGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGGGLM( 0, 1, 0, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'SGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGGGLM( 0, 0, -1, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'SGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGGGLM( 1, 0, 0, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'SGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SGGGLM( 0, 0, 0, A, 0, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'SGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL SGGGLM( 0, 0, 0, A, 1, B, 0, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'SGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL SGGGLM( 1, 1, 1, A, 1, B, 1, R1, R2, R3, W, 1, INFO )
         CALL CHKXER( 'SGGGLM', INFOT, NOUT, LERR, OK ) 
         NT = NT + 8
*
*     Test error exits for the LSE path.
*
      ELSE IF( LSAMEN( 3, PATH, 'LSE' ) ) THEN
*
*        SGGLSE
*
         SRNAMT = 'SGGLSE'
         INFOT = 1
         CALL SGGLSE( -1, 0, 0, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'SGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGGLSE( 0, -1, 0, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'SGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGGLSE( 0, 0, -1, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'SGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGGLSE( 0, 0, 1, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'SGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGGLSE( 0, 1, 0, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'SGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SGGLSE( 0, 0, 0, A, 0, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'SGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL SGGLSE( 0, 0, 0, A, 1, B, 0, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'SGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL SGGLSE( 1, 1, 1, A, 1, B, 1, R1, R2, R3, W, 1, INFO )
         CALL CHKXER( 'SGGLSE', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the GQR path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GQR' ) ) THEN
*
*        SGGQRF
*
         SRNAMT = 'SGGQRF'
         INFOT = 1
         CALL SGGQRF( -1, 0, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGGQRF( 0, -1, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGGQRF( 0, 0, -1, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SGGQRF( 0, 0, 0, A, 0, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL SGGQRF( 0, 0, 0, A, 1, R1, B, 0, R2, W, LW, INFO )
         CALL CHKXER( 'SGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL SGGQRF( 1, 1, 2, A, 1, R1, B, 1, R2, W, 1, INFO )
         CALL CHKXER( 'SGGQRF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        SGGRQF
*
         SRNAMT = 'SGGRQF'
         INFOT = 1
         CALL SGGRQF( -1, 0, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGGRQF( 0, -1, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGGRQF( 0, 0, -1, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SGGRQF( 0, 0, 0, A, 0, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL SGGRQF( 0, 0, 0, A, 1, R1, B, 0, R2, W, LW, INFO )
         CALL CHKXER( 'SGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL SGGRQF( 1, 1, 2, A, 1, R1, B, 1, R2, W, 1, INFO )
         CALL CHKXER( 'SGGRQF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
      END IF
*
*     Print a summary line.
*
      IF( OK ) THEN
         WRITE( NOUT, FMT = 9999 )PATH, NT
      ELSE
         WRITE( NOUT, FMT = 9998 )PATH
      END IF
*
 9999 FORMAT( 1X, A3, ' routines passed the tests of the error exits (',
     $      I3, ' tests done)' )
 9998 FORMAT( ' *** ', A3, ' routines failed the tests of the error ',
     $      'exits ***' )
*
      RETURN
*
*     End of SERRGG
*
      END
