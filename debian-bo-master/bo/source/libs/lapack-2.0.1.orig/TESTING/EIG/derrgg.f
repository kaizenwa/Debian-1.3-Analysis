      SUBROUTINE DERRGG( PATH, NUNIT )
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
*  DERRGG tests the error exits for DGGGLM, SGGHRD, SGGLSE, SGGQRF,
*  DGGRQF, SGGSVD, SGGSVP, DHGEQZ, DTGEVC, and STGSJA.
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
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            DUMMYK, DUMMYL, I, INFO, J, M, NCYCLE, NT
      DOUBLE PRECISION   TOLA, TOLB
*     ..
*     .. Local Arrays ..
      LOGICAL            SEL( NMAX )
      INTEGER            IW( NMAX )
      DOUBLE PRECISION   A( NMAX, NMAX ), B( NMAX, NMAX ),
     $                   Q( NMAX, NMAX ), R1( NMAX ), R2( NMAX ),
     $                   R3( NMAX ), TAU( NMAX ), U( NMAX, NMAX ),
     $                   V( NMAX, NMAX ), W( LW ), Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, DGGGLM, DGGHRD, DGGLSE, DGGQRF, DGGRQF,
     $                   DGGSVD, DGGSVP, DHGEQZ, DTGEVC, DTGSJA
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
      TOLA = 1.0D0
      TOLB = 1.0D0
      NT = 0
*
*     Test error exits for the GG path.
*
      IF( LSAMEN( 2, C2, 'GG' ) ) THEN
*
*        DGGHRD
*
         SRNAMT = 'DGGHRD'
         INFOT = 1
         CALL DGGHRD( '/', 'N', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGGHRD( 'N', '/', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGGHRD( 'N', 'N', -1, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DGGHRD( 'N', 'N', 0, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DGGHRD( 'N', 'N', 0, 1, 1, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DGGHRD( 'N', 'N', 2, 1, 1, A, 1, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DGGHRD( 'N', 'N', 2, 1, 1, A, 2, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DGGHRD( 'V', 'N', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL DGGHRD( 'N', 'V', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        DHGEQZ
*
         SRNAMT = 'DHGEQZ'
         INFOT = 1
         CALL DHGEQZ( '/', 'N', 'N', 0, 1, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DHGEQZ( 'E', '/', 'N', 0, 1, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DHGEQZ( 'E', 'N', '/', 0, 1, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DHGEQZ( 'E', 'N', 'N', -1, 0, 0, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DHGEQZ( 'E', 'N', 'N', 0, 0, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DHGEQZ( 'E', 'N', 'N', 0, 1, 1, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 1, B, 2, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 2, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL DHGEQZ( 'E', 'V', 'N', 2, 1, 1, A, 2, B, 2, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL DHGEQZ( 'E', 'N', 'V', 2, 1, 1, A, 2, B, 2, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        DTGEVC
*
         SRNAMT = 'DTGEVC'
         INFOT = 1
         CALL DTGEVC( '/', 'A', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DTGEVC( 'R', '/', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DTGEVC( 'R', 'A', SEL, -1, A, 1, B, 1, Q, 1, Z, 1, 0, M,
     $                W, INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DTGEVC( 'R', 'A', SEL, 2, A, 1, B, 2, Q, 1, Z, 2, 0, M, W,
     $                INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DTGEVC( 'R', 'A', SEL, 2, A, 2, B, 1, Q, 1, Z, 2, 0, M, W,
     $                INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DTGEVC( 'L', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL DTGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL DTGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 2, 1, M, W,
     $                INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the GSV path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GSV' ) ) THEN
*
*        DGGSVD
*
         SRNAMT = 'DGGSVD'
         INFOT = 1
         CALL DGGSVD( '/', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGGSVD( 'N', '/', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGGSVD( 'N', 'N', '/', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DGGSVD( 'N', 'N', 'N', -1, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DGGSVD( 'N', 'N', 'N', 0, -1, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DGGSVD( 'N', 'N', 'N', 0, 0, -1, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DGGSVD( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 0, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL DGGSVD( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                0, R1, R2, U, 1, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL DGGSVD( 'U', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 0, V, 1, Q, 1, W, IW, INFO )
         CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL DGGSVD( 'N', 'V', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 0, Q, 1, W, IW, INFO )
         CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL DGGSVD( 'N', 'N', 'Q', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 0, W, IW, INFO )
         CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        DGGSVP
*
         SRNAMT = 'DGGSVP'
         INFOT = 1
         CALL DGGSVP( '/', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGGSVP( 'N', '/', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGGSVP( 'N', 'N', '/', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DGGSVP( 'N', 'N', 'N', -1, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DGGSVP( 'N', 'N', 'N', 0, -1, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DGGSVP( 'N', 'N', 'N', 0, 0, -1, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DGGSVP( 'N', 'N', 'N', 0, 0, 0, A, 0, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DGGSVP( 'N', 'N', 'N', 0, 0, 0, A, 1, B, 0, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL DGGSVP( 'U', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 0, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL DGGSVP( 'N', 'V', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 0, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL DGGSVP( 'N', 'N', 'Q', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 0, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        DTGSJA
*
         SRNAMT = 'DTGSJA'
         INFOT = 1
         CALL DTGSJA( '/', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DTGSJA( 'N', '/', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DTGSJA( 'N', 'N', '/', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DTGSJA( 'N', 'N', 'N', -1, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DTGSJA( 'N', 'N', 'N', 0, -1, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DTGSJA( 'N', 'N', 'N', 0, 0, -1, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DTGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 0, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL DTGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                0, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL DTGSJA( 'U', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 0, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL DTGSJA( 'N', 'V', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 0, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL DTGSJA( 'N', 'N', 'Q', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 0, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*     Test error exits for the GLM path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GLM' ) ) THEN
*
*        DGGGLM
*
         SRNAMT = 'DGGGLM'
         INFOT = 1
         CALL DGGGLM( -1, 0, 0, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'DGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGGGLM( 0, -1, 0, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'DGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGGGLM( 0, 1, 0, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'DGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGGGLM( 0, 0, -1, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'DGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGGGLM( 1, 0, 0, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'DGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DGGGLM( 0, 0, 0, A, 0, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'DGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DGGGLM( 0, 0, 0, A, 1, B, 0, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'DGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL DGGGLM( 1, 1, 1, A, 1, B, 1, R1, R2, R3, W, 1, INFO )
         CALL CHKXER( 'DGGGLM', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the LSE path.
*
      ELSE IF( LSAMEN( 3, PATH, 'LSE' ) ) THEN
*
*        DGGLSE
*
         SRNAMT = 'DGGLSE'
         INFOT = 1
         CALL DGGLSE( -1, 0, 0, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'DGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGGLSE( 0, -1, 0, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'DGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGGLSE( 0, 0, -1, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'DGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGGLSE( 0, 0, 1, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'DGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGGLSE( 0, 1, 0, A, 1, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'DGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DGGLSE( 0, 0, 0, A, 0, B, 1, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'DGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DGGLSE( 0, 0, 0, A, 1, B, 0, R1, R2, R3, W, LW, INFO )
         CALL CHKXER( 'DGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL DGGLSE( 1, 1, 1, A, 1, B, 1, R1, R2, R3, W, 1, INFO )
         CALL CHKXER( 'DGGLSE', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the GQR path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GQR' ) ) THEN
*
*        DGGQRF
*
         SRNAMT = 'DGGQRF'
         INFOT = 1
         CALL DGGQRF( -1, 0, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGGQRF( 0, -1, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGGQRF( 0, 0, -1, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DGGQRF( 0, 0, 0, A, 0, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DGGQRF( 0, 0, 0, A, 1, R1, B, 0, R2, W, LW, INFO )
         CALL CHKXER( 'DGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DGGQRF( 1, 1, 2, A, 1, R1, B, 1, R2, W, 1, INFO )
         CALL CHKXER( 'DGGQRF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        DGGRQF
*
         SRNAMT = 'DGGRQF'
         INFOT = 1
         CALL DGGRQF( -1, 0, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGGRQF( 0, -1, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGGRQF( 0, 0, -1, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DGGRQF( 0, 0, 0, A, 0, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DGGRQF( 0, 0, 0, A, 1, R1, B, 0, R2, W, LW, INFO )
         CALL CHKXER( 'DGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DGGRQF( 1, 1, 2, A, 1, R1, B, 1, R2, W, 1, INFO )
         CALL CHKXER( 'DGGRQF', INFOT, NOUT, LERR, OK )
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
*     End of DERRGG
*
      END
