      SUBROUTINE DERRLS( PATH, NUNIT )
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
*  DERRLS tests the error exits for the DOUBLE PRECISION least squares
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
      DOUBLE PRECISION   A( NMAX, NMAX ), B( NMAX, NMAX ), S( NMAX ),
     $                   W( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, DGELS, DGELSS, DGELSX
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
      A( 1, 1 ) = 1.D0
      A( 1, 2 ) = 2.D0
      A( 2, 2 ) = 3.D0
      A( 2, 1 ) = 4.D0
      OK = .TRUE.
*
      IF( LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        Test error exits for the least squares driver routines.
*
*        DGELS
*
         SRNAMT = 'DGELS '
         INFOT = 1
         CALL DGELS( '/', 0, 0, 0, A, 1, B, 1, W, 1, INFO )
         CALL CHKXER( 'DGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGELS( 'N', -1, 0, 0, A, 1, B, 1, W, 1, INFO )
         CALL CHKXER( 'DGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGELS( 'N', 0, -1, 0, A, 1, B, 1, W, 1, INFO )
         CALL CHKXER( 'DGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DGELS( 'N', 0, 0, -1, A, 1, B, 1, W, 1, INFO )
         CALL CHKXER( 'DGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DGELS( 'N', 2, 0, 0, A, 1, B, 2, W, 2, INFO )
         CALL CHKXER( 'DGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DGELS( 'N', 2, 0, 0, A, 2, B, 1, W, 2, INFO )
         CALL CHKXER( 'DGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DGELS( 'N', 1, 1, 0, A, 1, B, 1, W, 1, INFO )
         CALL CHKXER( 'DGELS ', INFOT, NOUT, LERR, OK )
*
*        DGELSS
*
         SRNAMT = 'DGELSS'
         INFOT = 1
         CALL DGELSS( -1, 0, 0, A, 1, B, 1, S, RCOND, IRNK, W, 1, INFO )
         CALL CHKXER( 'DGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGELSS( 0, -1, 0, A, 1, B, 1, S, RCOND, IRNK, W, 1, INFO )
         CALL CHKXER( 'DGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGELSS( 0, 0, -1, A, 1, B, 1, S, RCOND, IRNK, W, 1, INFO )
         CALL CHKXER( 'DGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DGELSS( 2, 0, 0, A, 1, B, 2, S, RCOND, IRNK, W, 2, INFO )
         CALL CHKXER( 'DGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DGELSS( 2, 0, 0, A, 2, B, 1, S, RCOND, IRNK, W, 2, INFO )
         CALL CHKXER( 'DGELSS', INFOT, NOUT, LERR, OK )
*
*        DGELSX
*
         SRNAMT = 'DGELSX'
         INFOT = 1
         CALL DGELSX( -1, 0, 0, A, 1, B, 1, IP, RCOND, IRNK, W, INFO )
         CALL CHKXER( 'DGELSX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGELSX( 0, -1, 0, A, 1, B, 1, IP, RCOND, IRNK, W, INFO )
         CALL CHKXER( 'DGELSX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGELSX( 0, 0, -1, A, 1, B, 1, IP, RCOND, IRNK, W, INFO )
         CALL CHKXER( 'DGELSX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DGELSX( 2, 0, 0, A, 1, B, 2, IP, RCOND, IRNK, W, INFO )
         CALL CHKXER( 'DGELSX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DGELSX( 2, 0, 0, A, 2, B, 1, IP, RCOND, IRNK, W, INFO )
         CALL CHKXER( 'DGELSX', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of DERRLS
*
      END
