      SUBROUTINE DERRST( PATH, NUNIT )
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
*  DERRST tests the error exits for DSYTRD, DORGTR, DORMTR, DSPTRD,
*  DOPGTR, DOPMTR, DSTEQR, SSTERF, SSTEBZ, SSTEIN, DPTEQR, DSBTRD,
*  DSYEV, SSYEVX, SSYEVD, DSBEV, SSBEVX, SSBEVD,
*  DSPEV, SSPEVX, SSPEVD, DSTEV, SSTEVX, SSTEVD, and SSTEDC.
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
      INTEGER            NMAX, LIW, LW
      PARAMETER          ( NMAX = 3, LIW = 3*NMAX, LW = 4*NMAX )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, IL, INFO, IU, J, KD, M, NSPLIT, NT
      DOUBLE PRECISION   TOL, VL, VU
*     ..
*     .. Local Arrays ..
      INTEGER            I1( NMAX ), I2( NMAX ), I3( NMAX ), IW( LIW )
      DOUBLE PRECISION   A( NMAX, NMAX ), C( NMAX, NMAX ), D( NMAX ),
     $                   E( NMAX ), Q( NMAX, NMAX ), TAU( NMAX ),
     $                   W( LW ), X( NMAX ), Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, DOPGTR, DOPMTR, DORGTR, DORMTR, DPTEQR,
     $                   DSBEV, DSBEVD, DSBEVX, DSBTRD, DSPEV, DSPEVD,
     $                   DSPEVX, DSPTRD, DSTEBZ, DSTEDC, DSTEIN, DSTEQR,
     $                   DSTERF, DSTEV, DSTEVD, DSTEVX, DSYEV, DSYEVD,
     $                   DSYEVX, DSYTRD
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
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
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
            A( I, J ) = 1.D0 / DBLE( I+J )
   10    CONTINUE
   20 CONTINUE
      OK = .TRUE.
      NT = 0
*
*     Test error exits for the ST path.
*
      IF( LSAMEN( 2, C2, 'ST' ) ) THEN
*
*        DSYTRD
*
         SRNAMT = 'DSYTRD'
         INFOT = 1
         CALL DSYTRD( '/', 0, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'DSYTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSYTRD( 'U', -1, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'DSYTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DSYTRD( 'U', 2, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'DSYTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        DORGTR
*
         SRNAMT = 'DORGTR'
         INFOT = 1
         CALL DORGTR( '/', 0, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'DORGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DORGTR( 'U', -1, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'DORGTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DORGTR( 'U', 2, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'DORGTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DORGTR( 'U', 3, A, 3, TAU, W, 1, INFO )
         CALL CHKXER( 'DORGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        DORMTR
*
         SRNAMT = 'DORMTR'
         INFOT = 1
         CALL DORMTR( '/', 'U', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DORMTR( 'L', '/', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DORMTR( 'L', 'U', '/', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DORMTR( 'L', 'U', 'N', -1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DORMTR( 'L', 'U', 'N', 0, -1, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DORMTR( 'L', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DORMTR( 'R', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DORMTR( 'L', 'U', 'N', 2, 0, A, 2, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL DORMTR( 'L', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL DORMTR( 'R', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        DSPTRD
*
         SRNAMT = 'DSPTRD'
         INFOT = 1
         CALL DSPTRD( '/', 0, A, D, E, TAU, INFO )
         CALL CHKXER( 'DSPTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSPTRD( 'U', -1, A, D, E, TAU, INFO )
         CALL CHKXER( 'DSPTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 2
*
*        DOPGTR
*
         SRNAMT = 'DOPGTR'
         INFOT = 1
         CALL DOPGTR( '/', 0, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'DOPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DOPGTR( 'U', -1, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'DOPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DOPGTR( 'U', 2, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'DOPGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        DOPMTR
*
         SRNAMT = 'DOPMTR'
         INFOT = 1
         CALL DOPMTR( '/', 'U', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'DOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DOPMTR( 'L', '/', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'DOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DOPMTR( 'L', 'U', '/', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'DOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DOPMTR( 'L', 'U', 'N', -1, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'DOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DOPMTR( 'L', 'U', 'N', 0, -1, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'DOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DOPMTR( 'L', 'U', 'N', 2, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'DOPMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        DPTEQR
*
         SRNAMT = 'DPTEQR'
         INFOT = 1
         CALL DPTEQR( '/', 0, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DPTEQR( 'N', -1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DPTEQR( 'V', 2, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DPTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        DSTEBZ
*
         SRNAMT = 'DSTEBZ'
         INFOT = 1
         CALL DSTEBZ( '/', 'E', 0, VL, VU, IL, IU, TOL, D, E, M, NSPLIT,
     $                X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSTEBZ( 'A', '/', 0, VL, VU, IL, IU, TOL, D, E, M, NSPLIT,
     $                X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DSTEBZ( 'A', 'E', -1, VL, VU, IL, IU, TOL, D, E, M,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DSTEBZ( 'V', 'E', 0, 0.0D0, 0.0D0, IL, IU, TOL, D, E, M,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DSTEBZ( 'I', 'E', 0, VL, VU, 0, 0, TOL, D, E, M, NSPLIT,
     $                X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DSTEBZ( 'I', 'E', 1, VL, VU, 1, 0, TOL, D, E, M, NSPLIT,
     $                X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DSTEBZ( 'I', 'E', 0, VL, VU, 1, 1, TOL, D, E, M, NSPLIT,
     $                X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'DSTEBZ', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
*        DSTEIN
*
         SRNAMT = 'DSTEIN'
         INFOT = 1
         CALL DSTEIN( -1, D, E, 0, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL CHKXER( 'DSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DSTEIN( 0, D, E, -1, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL CHKXER( 'DSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DSTEIN( 0, D, E, 1, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL CHKXER( 'DSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DSTEIN( 2, D, E, 0, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL CHKXER( 'DSTEIN', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        DSTEQR
*
         SRNAMT = 'DSTEQR'
         INFOT = 1
         CALL DSTEQR( '/', 0, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSTEQR( 'N', -1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DSTEQR( 'V', 2, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        DSTERF
*
         SRNAMT = 'DSTERF'
         INFOT = 1
         CALL DSTERF( -1, D, E, INFO )
         CALL CHKXER( 'DSTERF', INFOT, NOUT, LERR, OK )
         NT = NT + 1
*
*        DSTEDC
*
         SRNAMT = 'DSTEDC'
         INFOT = 1
         CALL DSTEDC( '/', 0, D, E, Z, 1, W, -1, IW, -1, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSTEDC( 'N', -1, D, E, Z, 1, W, -1, IW, -1, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DSTEDC( 'V', 2, D, E, Z, 1, W, -1, IW, -1, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DSTEDC( 'V', 2, D, E, Z, 2, W, -1, IW, 1, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DSTEDC( 'I', 2, D, E, Z, 2, W, 1, IW, 1, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DSTEDC( 'V', 2, D, E, Z, 2, W, 1, IW, 1, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DSTEDC( 'V', 2, D, E, Z, 2, W, 1000, IW, -1, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DSTEDC( 'I', 2, D, E, Z, 2, W, 1000, IW, 1, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DSTEDC( 'V', 2, D, E, Z, 2, W, 1000, IW, 1, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        DSTEVD
*
         SRNAMT = 'DSTEVD'
         INFOT = 1
         CALL DSTEVD( '/', 0, D, E, Z, 1, W, -1, IW, -1, INFO )
         CALL CHKXER( 'DSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSTEVD( 'N', -1, D, E, Z, 1, W, -1, IW, -1, INFO )
         CALL CHKXER( 'DSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DSTEVD( 'V', 2, D, E, Z, 1, W, -1, IW, -1, INFO )
         CALL CHKXER( 'DSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DSTEVD( 'V', 2, D, E, Z, 2, W, -1, IW, 1, INFO )
         CALL CHKXER( 'DSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DSTEVD( 'V', 2, D, E, Z, 2, W, 1, IW, 1, INFO )
         CALL CHKXER( 'DSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DSTEVD( 'V', 2, D, E, Z, 2, W, 1000, IW, -1, INFO )
         CALL CHKXER( 'DSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DSTEVD( 'V', 2, D, E, Z, 2, W, 1000, IW, 1, INFO )
         CALL CHKXER( 'DSTEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
*        DSTEV
*
         SRNAMT = 'DSTEV'
         INFOT = 1
         CALL DSTEV( '/', 0, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSTEV', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSTEV( 'N', -1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSTEV', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DSTEV( 'V', 2, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSTEV', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        DSTEVX
*
         SRNAMT = 'DSTEVX'
         INFOT = 1
         CALL DSTEVX( '/', 'A', 0, D, E, VL, VU, IL, IU, TOL, M, W, Z,
     $                1, X, IW, I1, INFO )
         CALL CHKXER( 'DSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSTEVX( 'V', '/', 0, D, E, VL, VU, IL, IU, TOL, M, W, Z,
     $                1, X, IW, I1, INFO )
         CALL CHKXER( 'DSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DSTEVX( 'V', 'A', -1, D, E, VL, VU, IL, IU, TOL, M, W, Z,
     $                1, X, IW, I1, INFO )
         CALL CHKXER( 'DSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DSTEVX( 'V', 'V', 1, D, E, 0.0D0, 0.0D0, IL, IU, TOL, M,
     $                W, Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DSTEVX( 'V', 'I', 1, D, E, 0.0D0, 0.0D0, 0, IU, TOL, M, W,
     $                Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DSTEVX( 'V', 'I', 2, D, E, 0.0D0, 0.0D0, 2, 1, TOL, M, W,
     $                Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL DSTEVX( 'V', 'I', 1, D, E, 0.0D0, 0.0D0, 2, 1, TOL, M, W,
     $                Z, 0, X, IW, I1, INFO )
         CALL CHKXER( 'DSTEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
*        DSYEVD
*
         SRNAMT = 'DSYEVD'
         INFOT = 1
         CALL DSYEVD( '/', 'U', 0, A, 1, W, X, LW, IW, -1, INFO )
         CALL CHKXER( 'DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSYEVD( 'N', '/', 0, A, 1, W, X, LW, IW, -1, INFO )
         CALL CHKXER( 'DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DSYEVD( 'N', 'U', -1, A, 1, W, X, LW, IW, -1, INFO )
         CALL CHKXER( 'DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DSYEVD( 'N', 'U', 2, A, 1, W, X, LW, IW, -1, INFO )
         CALL CHKXER( 'DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DSYEVD( 'N', 'U', 1, A, 2, W, X, -1, IW, -1, INFO )
         CALL CHKXER( 'DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DSYEVD( 'N', 'U', 2, A, 2, W, X, 1, IW, -1, INFO )
         CALL CHKXER( 'DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DSYEVD( 'V', 'U', 2, A, 2, W, X, 1, IW, -1, INFO )
         CALL CHKXER( 'DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DSYEVD( 'N', 'U', 1, A, 2, W, X, 1000, IW, -1, INFO )
         CALL CHKXER( 'DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DSYEVD( 'N', 'U', 2, A, 2, W, X, 1000, IW, 0, INFO )
         CALL CHKXER( 'DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DSYEVD( 'V', 'U', 2, A, 2, W, X, 1000, IW, 1, INFO )
         CALL CHKXER( 'DSYEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        DSYEV
*
         SRNAMT = 'DSYEV'
         INFOT = 1
         CALL DSYEV( '/', 'U', 0, A, 1, W, X, LW, INFO )
         CALL CHKXER( 'DSYEV', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSYEV( 'N', '/', 0, A, 1, W, X, LW, INFO )
         CALL CHKXER( 'DSYEV', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DSYEV( 'N', 'U', -1, A, 1, W, X, LW, INFO )
         CALL CHKXER( 'DSYEV', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DSYEV( 'N', 'U', 2, A, 1, W, X, LW, INFO )
         CALL CHKXER( 'DSYEV', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DSYEV( 'N', 'U', 2, A, 2, W, X, 0, INFO )
         CALL CHKXER( 'DSYEV', INFOT, NOUT, LERR, OK )
         NT = NT + 5
*
*        DSYEVX
*
         SRNAMT = 'DSYEVX'
         INFOT = 1
         CALL DSYEVX( '/', 'A', 'U', 0, A, 1, VL, VU, IL, IU, TOL, M, W,
     $                Z, 1, X, LW, IW, I1, INFO )
         CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSYEVX( 'V', '/', 'U', 0, A, 1, VL, VU, IL, IU, TOL, M, W,
     $                Z, 1, X, LW, IW, I1, INFO )
         CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DSYEVX( 'V', 'A', '/', -1, A, 1, VL, VU, IL, IU, TOL, M,
     $                W, Z, 1, X, LW, IW, I1, INFO )
         INFOT = 4
         CALL DSYEVX( 'V', 'A', 'U', -1, A, 1, VL, VU, IL, IU, TOL, M,
     $                W, Z, 1, X, LW, IW, I1, INFO )
         CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DSYEVX( 'V', 'A', 'U', 2, A, 1, VL, VU, IL, IU, TOL, M, W,
     $                Z, 1, X, LW, IW, I1, INFO )
         CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DSYEVX( 'V', 'V', 'U', 1, A, 1, 0.0D0, 0.0D0, IL, IU, TOL,
     $                M, W, Z, 1, X, LW, IW, I1, INFO )
         CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DSYEVX( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 0, IU, TOL,
     $                M, W, Z, 1, X, LW, IW, I1, INFO )
         CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DSYEVX( 'V', 'I', 'U', 2, A, 2, 0.0D0, 0.0D0, 2, 1, TOL,
     $                M, W, Z, 1, X, LW, IW, I1, INFO )
         CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL DSYEVX( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 2, 1, TOL,
     $                M, W, Z, 0, X, LW, IW, I1, INFO )
         CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL DSYEVX( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 2, 1, TOL,
     $                M, W, Z, 1, X, 0, IW, I1, INFO )
         CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        DSPEVD
*
         SRNAMT = 'DSPEVD'
         INFOT = 1
         CALL DSPEVD( '/', 'U', 0, A, W, Z, 1, X, -1, IW, -1, INFO )
         CALL CHKXER( 'DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSPEVD( 'N', '/', 0, A, W, Z, 1, X, -1, IW, -1, INFO )
         CALL CHKXER( 'DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DSPEVD( 'N', 'U', -1, A, W, Z, 1, X, -1, IW, -1, INFO )
         CALL CHKXER( 'DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DSPEVD( 'V', 'U', 2, A, W, Z, 1, X, -1, IW, -1, INFO )
         CALL CHKXER( 'DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DSPEVD( 'N', 'U', 2, A, W, Z, 2, X, -1, IW, -1, INFO )
         CALL CHKXER( 'DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DSPEVD( 'N', 'U', 2, A, W, Z, 2, X, 1, IW, -1, INFO )
         CALL CHKXER( 'DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DSPEVD( 'V', 'U', 2, A, W, Z, 2, X, 1, IW, -1, INFO )
         CALL CHKXER( 'DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DSPEVD( 'N', 'U', 1, A, W, Z, 2, X, 1000, IW, -1, INFO )
         CALL CHKXER( 'DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DSPEVD( 'N', 'U', 2, A, W, Z, 2, X, 1000, IW, 0, INFO )
         CALL CHKXER( 'DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DSPEVD( 'V', 'U', 2, A, W, Z, 2, X, 1000, IW, 1, INFO )
         CALL CHKXER( 'DSPEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        DSPEV
*
         SRNAMT = 'DSPEV'
         INFOT = 1
         CALL DSPEV( '/', 'U', 0, A, W, Z, 1, X, INFO )
         CALL CHKXER( 'DSPEV', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSPEV( 'N', '/', 0, A, W, Z, 1, X, INFO )
         CALL CHKXER( 'DSPEV', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DSPEV( 'N', 'U', -1, A, W, Z, 1, X, INFO )
         CALL CHKXER( 'DSPEV', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DSPEV( 'V', 'U', 2, A, W, Z, 1, X, INFO )
         CALL CHKXER( 'DSPEV', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        DSPEVX
*
         SRNAMT = 'DSPEVX'
         INFOT = 1
         CALL DSPEVX( '/', 'A', 'U', 0, A, VL, VU, IL, IU, TOL, M, W, Z,
     $                1, X, IW, I1, INFO )
         CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSPEVX( 'V', '/', 'U', 0, A, VL, VU, IL, IU, TOL, M, W, Z,
     $                1, X, IW, I1, INFO )
         CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DSPEVX( 'V', 'A', '/', -1, A, VL, VU, IL, IU, TOL, M, W,
     $                Z, 1, X, IW, I1, INFO )
         INFOT = 4
         CALL DSPEVX( 'V', 'A', 'U', -1, A, VL, VU, IL, IU, TOL, M, W,
     $                Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DSPEVX( 'V', 'V', 'U', 1, A, 0.0D0, 0.0D0, IL, IU, TOL, M,
     $                W, Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DSPEVX( 'V', 'I', 'U', 1, A, 0.0D0, 0.0D0, 0, IU, TOL, M,
     $                W, Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DSPEVX( 'V', 'I', 'U', 2, A, 0.0D0, 0.0D0, 2, 1, TOL, M,
     $                W, Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL DSPEVX( 'V', 'I', 'U', 1, A, 0.0D0, 0.0D0, 2, 1, TOL, M,
     $                W, Z, 0, X, IW, I1, INFO )
         CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the SB path.
*
      ELSE IF( LSAMEN( 2, C2, 'SB' ) ) THEN
*
*        DSBTRD
*
         SRNAMT = 'DSBTRD'
         INFOT = 1
         CALL DSBTRD( '/', 'U', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSBTRD( 'N', '/', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DSBTRD( 'N', 'U', -1, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DSBTRD( 'N', 'U', 0, -1, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DSBTRD( 'N', 'U', 1, 1, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DSBTRD( 'V', 'U', 2, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSBTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        DSBEVD
*
         SRNAMT = 'DSBEVD'
         INFOT = 1
         CALL DSBEVD( '/', 'U', 0, KD, A, 1, W, Z, 1, X, -1, IW, -1,
     $                INFO )
         CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSBEVD( 'N', '/', 0, KD, A, 1, W, Z, 1, X, -1, IW, -1,
     $                INFO )
         CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DSBEVD( 'N', 'U', -1, KD, A, 1, W, Z, 1, X, -1, IW, -1,
     $                INFO )
         CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DSBEVD( 'N', 'U', 1, -1, A, 1, W, Z, 1, X, -1, IW, -1,
     $                INFO )
         CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DSBEVD( 'N', 'U', 2, 1, A, 1, W, Z, 1, X, -1, IW, -1,
     $                INFO )
         CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DSBEVD( 'N', 'U', 2, 1, A, 5, W, Z, 0, X, -1, IW, -1,
     $                INFO )
         CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DSBEVD( 'N', 'U', 1, 1, A, 5, W, Z, 2, X, -1, IW, -1,
     $                INFO )
         CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DSBEVD( 'N', 'U', 2, 1, A, 5, W, Z, 2, X, 0, IW, -1,
     $                INFO )
         CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DSBEVD( 'V', 'U', 2, 1, A, 5, W, Z, 2, X, 1, IW, -1,
     $                INFO )
         CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL DSBEVD( 'N', 'U', 1, 1, A, 5, W, Z, 2, X, 1000, IW, -1,
     $                INFO )
         CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL DSBEVD( 'N', 'U', 2, 1, A, 5, W, Z, 2, X, 1000, IW, 0,
     $                INFO )
         CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL DSBEVD( 'V', 'U', 2, 1, A, 5, W, Z, 2, X, 1000, IW, 1,
     $                INFO )
         CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 12
*
*        DSBEV
*
         SRNAMT = 'DSBEV'
         INFOT = 1
         CALL DSBEV( '/', 'U', 0, KD, A, 1, W, Z, 1, X, INFO )
         CALL CHKXER( 'DSBEV', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSBEV( 'N', '/', 0, KD, A, 1, W, Z, 1, X, INFO )
         CALL CHKXER( 'DSBEV', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DSBEV( 'N', 'U', -1, KD, A, 1, W, Z, 1, X, INFO )
         CALL CHKXER( 'DSBEV', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DSBEV( 'N', 'U', 2, -1, A, 1, W, Z, 1, X, INFO )
         CALL CHKXER( 'DSBEV', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DSBEV( 'N', 'U', 2, 1, A, 1, W, Z, 1, X, INFO )
         CALL CHKXER( 'DSBEV', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DSBEV( 'N', 'U', 2, 1, A, 5, W, Z, 0, X, INFO )
         CALL CHKXER( 'DSBEV', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        DSBEVX
*
         SRNAMT = 'DSBEVX'
         INFOT = 1
         CALL DSBEVX( '/', 'A', 'U', 0, KD, A, 1, Q, 1, VL, VU, IL, IU,
     $                TOL, M, W, Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSBEVX( 'V', '/', 'U', 0, KD, A, 1, Q, 1, VL, VU, IL, IU,
     $                TOL, M, W, Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DSBEVX( 'V', 'A', '/', -1, KD, A, 1, Q, 1, VL, VU, IL, IU,
     $                TOL, M, W, Z, 1, X, IW, I1, INFO )
         INFOT = 4
         CALL DSBEVX( 'V', 'A', 'U', -1, KD, A, 1, Q, 1, VL, VU, IL, IU,
     $                TOL, M, W, Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DSBEVX( 'V', 'A', 'U', 2, -1, A, 1, Q, 1, VL, VU, IL, IU,
     $                TOL, M, W, Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DSBEVX( 'V', 'A', 'U', 2, 1, A, 1, Q, 1, VL, VU, IL, IU,
     $                TOL, M, W, Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DSBEVX( 'V', 'A', 'U', 2, 1, A, 5, Q, 1, VL, VU, IL, IU,
     $                TOL, M, W, Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DSBEVX( 'V', 'V', 'U', 1, 1, A, 5, Q, 1, 0.0D0, 0.0D0, IL,
     $                IU, TOL, M, W, Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL DSBEVX( 'V', 'I', 'U', 1, 1, A, 5, Q, 1, 0.0D0, 0.0D0, 0,
     $                IU, TOL, M, W, Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL DSBEVX( 'V', 'I', 'U', 1, 1, A, 5, Q, 1, 0.0D0, 0.0D0, 1,
     $                2, TOL, M, W, Z, 1, X, IW, I1, INFO )
         CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL DSBEVX( 'V', 'I', 'U', 1, 1, A, 5, Q, 1, 0.0D0, 0.0D0, 1,
     $                1, TOL, M, W, Z, 0, X, IW, I1, INFO )
         CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 11
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
 9999 FORMAT( 1X, A3, ' routines passed the tests of the error exits',
     $      ' (', I3, ' tests done)' )
 9998 FORMAT( ' *** ', A3, ' routines failed the tests of the error ',
     $      'exits ***' )
*
      RETURN
*
*     End of DERRST
*
      END
