      SUBROUTINE CERRGG( PATH, NUNIT )
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
*  CERRGG tests the error exits for CGGGLM, CGGHRD, CGGLSE, CGGQRF,
*  CGGRQF, CGGSVD, CGGSVP, CHGEQZ, CTGEVC, and CTGSJA.
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
      INTEGER            IW( LW )
      REAL               R1( NMAX ), R2( NMAX ), RW( LW )
      COMPLEX            A( NMAX, NMAX ), ALPHA( NMAX ),
     $                   B( NMAX, NMAX ), BETA( NMAX ), Q( NMAX, NMAX ),
     $                   TAU( NMAX ), U( NMAX, NMAX ), V( NMAX, NMAX ),
     $                   W( LW ), Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CGGGLM, CGGHRD, CGGLSE, CGGQRF, CGGRQF, CGGSVD,
     $                   CGGSVP, CHGEQZ, CHKXER, CTGEVC, CTGSJA
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
*        CGGHRD
*
         SRNAMT = 'CGGHRD'
         INFOT = 1
         CALL CGGHRD( '/', 'N', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGHRD( 'N', '/', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGHRD( 'N', 'N', -1, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGGHRD( 'N', 'N', 0, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGHRD( 'N', 'N', 0, 1, 1, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CGGHRD( 'N', 'N', 2, 1, 1, A, 1, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CGGHRD( 'N', 'N', 2, 1, 1, A, 2, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CGGHRD( 'V', 'N', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CGGHRD( 'N', 'V', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        CHGEQZ
*
         SRNAMT = 'CHGEQZ'
         INFOT = 1
         CALL CHGEQZ( '/', 'N', 'N', 0, 1, 0, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHGEQZ( 'E', '/', 'N', 0, 1, 0, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHGEQZ( 'E', 'N', '/', 0, 1, 0, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CHGEQZ( 'E', 'N', 'N', -1, 0, 0, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CHGEQZ( 'E', 'N', 'N', 0, 0, 0, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CHGEQZ( 'E', 'N', 'N', 0, 1, 1, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 1, B, 2, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 2, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL CHGEQZ( 'E', 'V', 'N', 2, 1, 1, A, 2, B, 2, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL CHGEQZ( 'E', 'N', 'V', 2, 1, 1, A, 2, B, 2, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        CTGEVC
*
         SRNAMT = 'CTGEVC'
         INFOT = 1
         CALL CTGEVC( '/', 'A', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CTGEVC( 'R', '/', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CTGEVC( 'R', 'A', SEL, -1, A, 1, B, 1, Q, 1, Z, 1, 0, M,
     $                W, RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CTGEVC( 'R', 'A', SEL, 2, A, 1, B, 2, Q, 1, Z, 2, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CTGEVC( 'R', 'A', SEL, 2, A, 2, B, 1, Q, 1, Z, 2, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CTGEVC( 'L', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CTGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CTGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 2, 1, M, W,
     $                RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the GSV path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GSV' ) ) THEN
*
*        CGGSVD
*
         SRNAMT = 'CGGSVD'
         INFOT = 1
         CALL CGGSVD( '/', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGSVD( 'N', '/', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGSVD( 'N', 'N', '/', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGGSVD( 'N', 'N', 'N', -1, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGSVD( 'N', 'N', 'N', 0, -1, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CGGSVD( 'N', 'N', 'N', 0, 0, -1, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CGGSVD( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 0, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CGGSVD( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                0, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL CGGSVD( 'U', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 0, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL CGGSVD( 'N', 'V', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 0, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL CGGSVD( 'N', 'N', 'Q', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 0, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        CGGSVP
*
         SRNAMT = 'CGGSVP'
         INFOT = 1
         CALL CGGSVP( '/', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGSVP( 'N', '/', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGSVP( 'N', 'N', '/', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGGSVP( 'N', 'N', 'N', -1, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGSVP( 'N', 'N', 'N', 0, -1, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CGGSVP( 'N', 'N', 'N', 0, 0, -1, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CGGSVP( 'N', 'N', 'N', 0, 0, 0, A, 0, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CGGSVP( 'N', 'N', 'N', 0, 0, 0, A, 1, B, 0, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL CGGSVP( 'U', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 0, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL CGGSVP( 'N', 'V', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 0, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL CGGSVP( 'N', 'N', 'Q', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 0, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        CTGSJA
*
         SRNAMT = 'CTGSJA'
         INFOT = 1
         CALL CTGSJA( '/', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CTGSJA( 'N', '/', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CTGSJA( 'N', 'N', '/', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CTGSJA( 'N', 'N', 'N', -1, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CTGSJA( 'N', 'N', 'N', 0, -1, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CTGSJA( 'N', 'N', 'N', 0, 0, -1, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CTGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 0, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CTGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                0, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL CTGSJA( 'U', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 0, V, 1, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL CTGSJA( 'N', 'V', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 0, Q, 1, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL CTGSJA( 'N', 'N', 'Q', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 0, W,
     $                NCYCLE, INFO ) 
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*     Test error exits for the GLM path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GLM' ) ) THEN
*
*        CGGGLM
*
         SRNAMT = 'CGGGLM'
         INFOT = 1
         CALL CGGGLM( -1, 0, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGGLM( 0, -1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGGLM( 0, 1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGGLM( 0, 0, -1, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGGLM( 1, 0, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGGLM( 0, 0, 0, A, 0, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CGGGLM( 0, 0, 0, A, 1, B, 0, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CGGGLM( 1, 1, 1, A, 1, B, 1, TAU, ALPHA, BETA, W, 1,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the LSE path.
*
      ELSE IF( LSAMEN( 3, PATH, 'LSE' ) ) THEN
*
*        CGGLSE
*
         SRNAMT = 'CGGLSE'
         INFOT = 1
         CALL CGGLSE( -1, 0, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGLSE( 0, -1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGLSE( 0, 0, -1, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGLSE( 0, 0, 1, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGLSE( 0, 1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGLSE( 0, 0, 0, A, 0, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CGGLSE( 0, 0, 0, A, 1, B, 0, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CGGLSE( 1, 1, 1, A, 1, B, 1, TAU, ALPHA, BETA, W, 1,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the GQR path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GQR' ) ) THEN
*
*        CGGQRF
*
         SRNAMT = 'CGGQRF'
         INFOT = 1
         CALL CGGQRF( -1, 0, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGQRF( 0, -1, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGQRF( 0, 0, -1, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGQRF( 0, 0, 0, A, 0, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CGGQRF( 0, 0, 0, A, 1, ALPHA, B, 0, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CGGQRF( 1, 1, 2, A, 1, ALPHA, B, 1, BETA, W, 1, INFO )
         CALL CHKXER( 'CGGQRF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        CGGRQF
*
         SRNAMT = 'CGGRQF'
         INFOT = 1
         CALL CGGRQF( -1, 0, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGRQF( 0, -1, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGRQF( 0, 0, -1, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGRQF( 0, 0, 0, A, 0, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CGGRQF( 0, 0, 0, A, 1, ALPHA, B, 0, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CGGRQF( 1, 1, 2, A, 1, ALPHA, B, 1, BETA, W, 1, INFO )
         CALL CHKXER( 'CGGRQF', INFOT, NOUT, LERR, OK )
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
*     End of CERRGG
*
      END
