      SUBROUTINE CERRLS( PATH, NUNIT )
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
*  CERRLS tests the error exits for the COMPLEX least squares
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
      REAL               RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      REAL               RW( NMAX ), S( NMAX )
      COMPLEX            A( NMAX, NMAX ), B( NMAX, NMAX ), W( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CGELS, CGELSS, CGELSX, CHKXER
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
      A( 1, 1 ) = 1.
      A( 1, 2 ) = 2.
      A( 2, 2 ) = 3.
      A( 2, 1 ) = 4.
      OK = .TRUE.
      WRITE( NOUT, FMT = * )
*
*     Test error exits for the least squares driver routines.
*
      IF( LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        CGELS
*
         SRNAMT = 'CGELS '
         INFOT = 1
         CALL CGELS( '/', 0, 0, 0, A, 1, B, 1, W, 1, INFO )
         CALL CHKXER( 'CGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGELS( 'N', -1, 0, 0, A, 1, B, 1, W, 1, INFO )
         CALL CHKXER( 'CGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGELS( 'N', 0, -1, 0, A, 1, B, 1, W, 1, INFO )
         CALL CHKXER( 'CGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGELS( 'N', 0, 0, -1, A, 1, B, 1, W, 1, INFO )
         CALL CHKXER( 'CGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CGELS( 'N', 2, 0, 0, A, 1, B, 2, W, 2, INFO )
         CALL CHKXER( 'CGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CGELS( 'N', 2, 0, 0, A, 2, B, 1, W, 2, INFO )
         CALL CHKXER( 'CGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CGELS( 'N', 1, 1, 0, A, 1, B, 1, W, 1, INFO )
         CALL CHKXER( 'CGELS ', INFOT, NOUT, LERR, OK )
*
*        CGELSS
*
         SRNAMT = 'CGELSS'
         INFOT = 1
         CALL CGELSS( -1, 0, 0, A, 1, B, 1, S, RCOND, IRNK, W, 1, RW,
     $                INFO )
         CALL CHKXER( 'CGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGELSS( 0, -1, 0, A, 1, B, 1, S, RCOND, IRNK, W, 1, RW,
     $                INFO )
         CALL CHKXER( 'CGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGELSS( 0, 0, -1, A, 1, B, 1, S, RCOND, IRNK, W, 1, RW,
     $                INFO )
         CALL CHKXER( 'CGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGELSS( 2, 0, 0, A, 1, B, 2, S, RCOND, IRNK, W, 2, RW,
     $                INFO )
         CALL CHKXER( 'CGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CGELSS( 2, 0, 0, A, 2, B, 1, S, RCOND, IRNK, W, 2, RW,
     $                INFO )
         CALL CHKXER( 'CGELSS', INFOT, NOUT, LERR, OK )
*
*        CGELSX
*
         SRNAMT = 'CGELSX'
         INFOT = 1
         CALL CGELSX( -1, 0, 0, A, 1, B, 1, IP, RCOND, IRNK, W, RW,
     $                INFO )
         CALL CHKXER( 'CGELSX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGELSX( 0, -1, 0, A, 1, B, 1, IP, RCOND, IRNK, W, RW,
     $                INFO )
         CALL CHKXER( 'CGELSX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGELSX( 0, 0, -1, A, 1, B, 1, IP, RCOND, IRNK, W, RW,
     $                INFO )
         CALL CHKXER( 'CGELSX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGELSX( 2, 0, 0, A, 1, B, 2, IP, RCOND, IRNK, W, RW,
     $                INFO )
         CALL CHKXER( 'CGELSX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CGELSX( 2, 0, 0, A, 2, B, 1, IP, RCOND, IRNK, W, RW,
     $                INFO )
         CALL CHKXER( 'CGELSX', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of CERRLS
*
      END
