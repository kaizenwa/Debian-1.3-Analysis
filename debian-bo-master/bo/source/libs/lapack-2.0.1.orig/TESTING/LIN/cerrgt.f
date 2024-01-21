      SUBROUTINE CERRGT( PATH, NUNIT )
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
*  CERRGT tests the error exits for the COMPLEX tridiagonal
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
      REAL               ANORM, RCOND
*     ..
*     .. Local Arrays ..
      REAL               D( NMAX ), DF( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   RW( NMAX )
      COMPLEX            B( NMAX ), E( NMAX ), EF( NMAX ), W( NMAX ),
     $                   X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, CPTCON, CPTRFS, CPTTRF, CPTTRS
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
      D( 1 ) = 1.
      D( 2 ) = 2.
      E( 1 ) = 3.
      E( 2 ) = 4.
      ANORM = 1.0
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
*        CPTTRF
*
         SRNAMT = 'CPTTRF'
         INFOT = 1
         CALL CPTTRF( -1, D, E, INFO )
         CALL CHKXER( 'CPTTRF', INFOT, NOUT, LERR, OK )
*
*        CPTTRS
*
         SRNAMT = 'CPTTRS'
         INFOT = 1
         CALL CPTTRS( '/', 1, 0, D, E, X, 1, INFO )
         CALL CHKXER( 'CPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CPTTRS( 'U', -1, 0, D, E, X, 1, INFO )
         CALL CHKXER( 'CPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CPTTRS( 'U', 0, -1, D, E, X, 1, INFO )
         CALL CHKXER( 'CPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CPTTRS( 'U', 2, 1, D, E, X, 1, INFO )
         CALL CHKXER( 'CPTTRS', INFOT, NOUT, LERR, OK )
*
*        CPTRFS
*
         SRNAMT = 'CPTRFS'
         INFOT = 1
         CALL CPTRFS( '/', 1, 0, D, E, DF, EF, B, 1, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL CHKXER( 'CPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CPTRFS( 'U', -1, 0, D, E, DF, EF, B, 1, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL CHKXER( 'CPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CPTRFS( 'U', 0, -1, D, E, DF, EF, B, 1, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL CHKXER( 'CPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CPTRFS( 'U', 2, 1, D, E, DF, EF, B, 1, X, 2, R1, R2, W,
     $                RW, INFO )
         CALL CHKXER( 'CPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CPTRFS( 'U', 2, 1, D, E, DF, EF, B, 2, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL CHKXER( 'CPTRFS', INFOT, NOUT, LERR, OK )
*
*        CPTCON
*
         SRNAMT = 'CPTCON'
         INFOT = 1
         CALL CPTCON( -1, D, E, ANORM, RCOND, RW, INFO )
         CALL CHKXER( 'CPTCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CPTCON( 0, D, E, -ANORM, RCOND, RW, INFO )
         CALL CHKXER( 'CPTCON', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of CERRGT
*
      END
