      SUBROUTINE ZERRGT( PATH, NUNIT )
*
*  -- LAPACK test routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  Purpose
*  =======
*
*  ZERRGT tests the error exits for the COMPLEX*16 tridiagonal
*  routines.
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
      INTEGER            NMAX
      PARAMETER          ( NMAX = 2 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            INFO
      DOUBLE PRECISION   ANORM, RCOND
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   D( NMAX ), DF( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   RW( NMAX )
      COMPLEX*16         B( NMAX ), E( NMAX ), EF( NMAX ), W( NMAX ),
     $                   X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, ZPTCON, ZPTRFS, ZPTTRF, ZPTTRS
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
      D( 1 ) = 1.D0
      D( 2 ) = 2.D0
      E( 1 ) = 3.D0
      E( 2 ) = 4.D0
      ANORM = 1.0D0
      OK = .TRUE.
*
      IF( LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        Test error exits for the general tridiagonal routines.
*
      ELSE IF( LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        Test error exits for the positive definite tridiagonal
*        routines.
*
*        ZPTTRF
*
         SRNAMT = 'ZPTTRF'
         INFOT = 1
         CALL ZPTTRF( -1, D, E, INFO )
         CALL CHKXER( 'ZPTTRF', INFOT, NOUT, LERR, OK )
*
*        ZPTTRS
*
         SRNAMT = 'ZPTTRS'
         INFOT = 1
         CALL ZPTTRS( '/', 1, 0, D, E, X, 1, INFO )
         CALL CHKXER( 'ZPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZPTTRS( 'U', -1, 0, D, E, X, 1, INFO )
         CALL CHKXER( 'ZPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZPTTRS( 'U', 0, -1, D, E, X, 1, INFO )
         CALL CHKXER( 'ZPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZPTTRS( 'U', 2, 1, D, E, X, 1, INFO )
         CALL CHKXER( 'ZPTTRS', INFOT, NOUT, LERR, OK )
*
*        ZPTRFS
*
         SRNAMT = 'ZPTRFS'
         INFOT = 1
         CALL ZPTRFS( '/', 1, 0, D, E, DF, EF, B, 1, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL CHKXER( 'ZPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZPTRFS( 'U', -1, 0, D, E, DF, EF, B, 1, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL CHKXER( 'ZPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZPTRFS( 'U', 0, -1, D, E, DF, EF, B, 1, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL CHKXER( 'ZPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZPTRFS( 'U', 2, 1, D, E, DF, EF, B, 1, X, 2, R1, R2, W,
     $                RW, INFO )
         CALL CHKXER( 'ZPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZPTRFS( 'U', 2, 1, D, E, DF, EF, B, 2, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL CHKXER( 'ZPTRFS', INFOT, NOUT, LERR, OK )
*
*        ZPTCON
*
         SRNAMT = 'ZPTCON'
         INFOT = 1
         CALL ZPTCON( -1, D, E, ANORM, RCOND, RW, INFO )
         CALL CHKXER( 'ZPTCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZPTCON( 0, D, E, -ANORM, RCOND, RW, INFO )
         CALL CHKXER( 'ZPTCON', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of ZERRGT
*
      END
