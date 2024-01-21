      SUBROUTINE ZERRLS( PATH, NUNIT )
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
*  ZERRLS tests the error exits for the COMPLEX*16 least squares
*  driver routines.
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
      INTEGER            INFO, IRNK
      DOUBLE PRECISION   RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      DOUBLE PRECISION   RW( NMAX ), S( NMAX )
      COMPLEX*16         A( NMAX, NMAX ), B( NMAX, NMAX ), W( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, ZGELS, ZGELSS, ZGELSX
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
      C2 = PATH( 2: 3 )
      A( 1, 1 ) = 1.D0
      A( 1, 2 ) = 2.D0
      A( 2, 2 ) = 3.D0
      A( 2, 1 ) = 4.D0
      OK = .TRUE.
      WRITE( NOUT, FMT = * )
*
*     Test error exits for the least squares driver routines.
*
      IF( LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        ZGELS
*
         SRNAMT = 'ZGELS '
         INFOT = 1
         CALL ZGELS( '/', 0, 0, 0, A, 1, B, 1, W, 1, INFO )
         CALL CHKXER( 'ZGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZGELS( 'N', -1, 0, 0, A, 1, B, 1, W, 1, INFO )
         CALL CHKXER( 'ZGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZGELS( 'N', 0, -1, 0, A, 1, B, 1, W, 1, INFO )
         CALL CHKXER( 'ZGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZGELS( 'N', 0, 0, -1, A, 1, B, 1, W, 1, INFO )
         CALL CHKXER( 'ZGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZGELS( 'N', 2, 0, 0, A, 1, B, 2, W, 2, INFO )
         CALL CHKXER( 'ZGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZGELS( 'N', 2, 0, 0, A, 2, B, 1, W, 2, INFO )
         CALL CHKXER( 'ZGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZGELS( 'N', 1, 1, 0, A, 1, B, 1, W, 1, INFO )
         CALL CHKXER( 'ZGELS ', INFOT, NOUT, LERR, OK )
*
*        ZGELSS
*
         SRNAMT = 'ZGELSS'
         INFOT = 1
         CALL ZGELSS( -1, 0, 0, A, 1, B, 1, S, RCOND, IRNK, W, 1, RW,
     $                INFO )
         CALL CHKXER( 'ZGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZGELSS( 0, -1, 0, A, 1, B, 1, S, RCOND, IRNK, W, 1, RW,
     $                INFO )
         CALL CHKXER( 'ZGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZGELSS( 0, 0, -1, A, 1, B, 1, S, RCOND, IRNK, W, 1, RW,
     $                INFO )
         CALL CHKXER( 'ZGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZGELSS( 2, 0, 0, A, 1, B, 2, S, RCOND, IRNK, W, 2, RW,
     $                INFO )
         CALL CHKXER( 'ZGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZGELSS( 2, 0, 0, A, 2, B, 1, S, RCOND, IRNK, W, 2, RW,
     $                INFO )
         CALL CHKXER( 'ZGELSS', INFOT, NOUT, LERR, OK )
*
*        ZGELSX
*
         SRNAMT = 'ZGELSX'
         INFOT = 1
         CALL ZGELSX( -1, 0, 0, A, 1, B, 1, IP, RCOND, IRNK, W, RW,
     $                INFO )
         CALL CHKXER( 'ZGELSX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZGELSX( 0, -1, 0, A, 1, B, 1, IP, RCOND, IRNK, W, RW,
     $                INFO )
         CALL CHKXER( 'ZGELSX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZGELSX( 0, 0, -1, A, 1, B, 1, IP, RCOND, IRNK, W, RW,
     $                INFO )
         CALL CHKXER( 'ZGELSX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZGELSX( 2, 0, 0, A, 1, B, 2, IP, RCOND, IRNK, W, RW,
     $                INFO )
         CALL CHKXER( 'ZGELSX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZGELSX( 2, 0, 0, A, 2, B, 1, IP, RCOND, IRNK, W, RW,
     $                INFO )
         CALL CHKXER( 'ZGELSX', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of ZERRLS
*
      END
