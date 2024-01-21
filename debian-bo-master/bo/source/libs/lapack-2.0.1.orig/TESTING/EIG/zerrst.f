      SUBROUTINE ZERRST( PATH, NUNIT )
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
*  ZERRST tests the error exits for ZHETRD, ZUNGTR, CUNMTR, ZHPTRD,
*  ZUNGTR, ZUPMTR, ZSTEQR, CSTEIN, ZPTEQR, ZHBTRD,
*  ZHEEV, CHEEVX, CHEEVD, ZHBEV, CHBEVX, CHBEVD,
*  ZHPEV, CHPEVX, CHPEVD, and ZSTEDC.
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
      PARAMETER          ( NMAX = 3, LIW = 5*NMAX, LW = 5*NMAX )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, IL, INFO, IU, J, KD, M, NT
      DOUBLE PRECISION   TOL, VL, VU
*     ..
*     .. Local Arrays ..
      INTEGER            I1( NMAX ), I2( NMAX ), I3( NMAX ), IW( LIW )
      DOUBLE PRECISION   D( NMAX ), E( NMAX ), RW( LW ), X( NMAX )
      COMPLEX*16         A( NMAX, NMAX ), C( NMAX, NMAX ),
     $                   Q( NMAX, NMAX ), TAU( NMAX ), W( LW ),
     $                   Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, ZHBEV, ZHBEVD, ZHBEVX, ZHBTRD, ZHEEV,
     $                   ZHEEVD, ZHEEVX, ZHETRD, ZHPEV, ZHPEVD, ZHPEVX,
     $                   ZHPTRD, ZPTEQR, ZSTEDC, ZSTEIN, ZSTEQR, ZUNGTR,
     $                   ZUNMTR, ZUPGTR, ZUPMTR
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
*        ZHETRD
*
         SRNAMT = 'ZHETRD'
         INFOT = 1
         CALL ZHETRD( '/', 0, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'ZHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHETRD( 'U', -1, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'ZHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZHETRD( 'U', 2, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'ZHETRD', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        ZUNGTR
*
         SRNAMT = 'ZUNGTR'
         INFOT = 1
         CALL ZUNGTR( '/', 0, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'ZUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZUNGTR( 'U', -1, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'ZUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZUNGTR( 'U', 2, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'ZUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZUNGTR( 'U', 3, A, 3, TAU, W, 1, INFO )
         CALL CHKXER( 'ZUNGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        ZUNMTR
*
         SRNAMT = 'ZUNMTR'
         INFOT = 1
         CALL ZUNMTR( '/', 'U', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZUNMTR( 'L', '/', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZUNMTR( 'L', 'U', '/', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZUNMTR( 'L', 'U', 'N', -1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZUNMTR( 'L', 'U', 'N', 0, -1, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZUNMTR( 'L', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZUNMTR( 'R', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZUNMTR( 'L', 'U', 'N', 2, 0, A, 2, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZUNMTR( 'L', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZUNMTR( 'R', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        ZHPTRD
*
         SRNAMT = 'ZHPTRD'
         INFOT = 1
         CALL ZHPTRD( '/', 0, A, D, E, TAU, INFO )
         CALL CHKXER( 'ZHPTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHPTRD( 'U', -1, A, D, E, TAU, INFO )
         CALL CHKXER( 'ZHPTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 2
*
*        ZUPGTR
*
         SRNAMT = 'ZUPGTR'
         INFOT = 1
         CALL ZUPGTR( '/', 0, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'ZUPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZUPGTR( 'U', -1, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'ZUPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZUPGTR( 'U', 2, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'ZUPGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        ZUPMTR
*
         SRNAMT = 'ZUPMTR'
         INFOT = 1
         CALL ZUPMTR( '/', 'U', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'ZUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZUPMTR( 'L', '/', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'ZUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZUPMTR( 'L', 'U', '/', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'ZUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZUPMTR( 'L', 'U', 'N', -1, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'ZUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZUPMTR( 'L', 'U', 'N', 0, -1, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'ZUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZUPMTR( 'L', 'U', 'N', 2, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'ZUPMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        ZPTEQR
*
         SRNAMT = 'ZPTEQR'
         INFOT = 1
         CALL ZPTEQR( '/', 0, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'ZPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZPTEQR( 'N', -1, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'ZPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZPTEQR( 'V', 2, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'ZPTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        ZSTEIN
*
         SRNAMT = 'ZSTEIN'
         INFOT = 1
         CALL ZSTEIN( -1, D, E, 0, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'ZSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZSTEIN( 0, D, E, -1, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'ZSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZSTEIN( 0, D, E, 1, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'ZSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZSTEIN( 2, D, E, 0, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'ZSTEIN', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        ZSTEQR
*
         SRNAMT = 'ZSTEQR'
         INFOT = 1
         CALL ZSTEQR( '/', 0, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'ZSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZSTEQR( 'N', -1, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'ZSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZSTEQR( 'V', 2, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'ZSTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        ZSTEDC
*
         SRNAMT = 'ZSTEDC'
         INFOT = 1
         CALL ZSTEDC( '/', 0, D, E, Z, 1, W, 0, RW, 0, IW, 0, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZSTEDC( 'N', -1, D, E, Z, 1, W, 0, RW, 0, IW, 0, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZSTEDC( 'V', 2, D, E, Z, 1, W, 0, RW, 0, IW, 0, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZSTEDC( 'N', 2, D, E, Z, 2, W, 0, RW, 0, IW, 0, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZSTEDC( 'V', 2, D, E, Z, 2, W, 1, RW, 0, IW, 0, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZSTEDC( 'N', 2, D, E, Z, 2, W, 99, RW, 0, IW, 0, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZSTEDC( 'V', 2, D, E, Z, 2, W, 99, RW, 1, IW, 0, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZSTEDC( 'N', 2, D, E, Z, 2, W, 99, RW, 99, IW, 0, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZSTEDC( 'V', 2, D, E, Z, 2, W, 99, RW, 99, IW, 1, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        ZHEEVD
*
         SRNAMT = 'ZHEEVD'
         INFOT = 1
         CALL ZHEEVD( '/', 'U', 0, A, 1, X, W, LW, RW, 0, IW, 0, INFO )
         CALL CHKXER( 'ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHEEVD( 'N', '/', 0, A, 1, X, W, LW, RW, 0, IW, 0, INFO )
         CALL CHKXER( 'ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHEEVD( 'N', 'U', -1, A, 1, X, W, LW, RW, 0, IW, 0, INFO )
         CALL CHKXER( 'ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZHEEVD( 'N', 'U', 2, A, 1, X, W, LW, RW, 0, IW, 0, INFO )
         CALL CHKXER( 'ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZHEEVD( 'N', 'U', 2, A, 2, X, W, 0, RW, 0, IW, 0, INFO )
         CALL CHKXER( 'ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZHEEVD( 'V', 'U', 2, A, 2, X, W, 1, RW, 0, IW, 0, INFO )
         CALL CHKXER( 'ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZHEEVD( 'N', 'U', 2, A, 2, X, W, 99, RW, 0, IW, 0, INFO )
         CALL CHKXER( 'ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZHEEVD( 'V', 'U', 2, A, 2, X, W, 99, RW, 1, IW, 0, INFO )
         CALL CHKXER( 'ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZHEEVD( 'N', 'U', 2, A, 2, X, W, 99, RW, 99, IW, 0, INFO )
         CALL CHKXER( 'ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZHEEVD( 'V', 'U', 2, A, 2, X, W, 99, RW, 99, IW, 1, INFO )
         CALL CHKXER( 'ZHEEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        ZHEEV
*
         SRNAMT = 'ZHEEV'
         INFOT = 1
         CALL ZHEEV( '/', 'U', 0, A, 1, X, W, LW, RW, INFO )
         CALL CHKXER( 'ZHEEV', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHEEV( 'N', '/', 0, A, 1, X, W, LW, RW, INFO )
         CALL CHKXER( 'ZHEEV', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHEEV( 'N', 'U', -1, A, 1, X, W, LW, RW, INFO )
         CALL CHKXER( 'ZHEEV', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZHEEV( 'N', 'U', 2, A, 1, X, W, LW, RW, INFO )
         CALL CHKXER( 'ZHEEV', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZHEEV( 'N', 'U', 2, A, 2, X, W, 0, RW, INFO )
         CALL CHKXER( 'ZHEEV', INFOT, NOUT, LERR, OK )
         NT = NT + 5
*
*        ZHEEVX
*
         SRNAMT = 'ZHEEVX'
         INFOT = 1
         CALL ZHEEVX( '/', 'A', 'U', 0, A, 1, VL, VU, IL, IU, TOL, M, X,
     $                Z, 1, W, LW, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHEEVX( 'V', '/', 'U', 0, A, 1, VL, VU, IL, IU, TOL, M, X,
     $                Z, 1, W, LW, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHEEVX( 'V', 'A', '/', -1, A, 1, VL, VU, IL, IU, TOL, M,
     $                X, Z, 1, W, LW, RW, IW, I1, INFO )
         INFOT = 4
         CALL ZHEEVX( 'V', 'A', 'U', -1, A, 1, VL, VU, IL, IU, TOL, M,
     $                X, Z, 1, W, LW, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZHEEVX( 'V', 'A', 'U', 2, A, 1, VL, VU, IL, IU, TOL, M, X,
     $                Z, 1, W, LW, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZHEEVX( 'V', 'V', 'U', 1, A, 1, 0.0D0, 0.0D0, IL, IU, TOL,
     $                M, X, Z, 1, W, LW, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZHEEVX( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 0, IU, TOL,
     $                M, X, Z, 1, W, LW, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZHEEVX( 'V', 'I', 'U', 2, A, 2, 0.0D0, 0.0D0, 2, 1, TOL,
     $                M, X, Z, 1, W, LW, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL ZHEEVX( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 2, 1, TOL,
     $                M, X, Z, 0, W, LW, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL ZHEEVX( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 2, 1, TOL,
     $                M, X, Z, 1, W, 0, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        ZHPEVD
*
         SRNAMT = 'ZHPEVD'
         INFOT = 1
         CALL ZHPEVD( '/', 'U', 0, A, RW, Z, 1, Q, 0, X, 0, IW, 0,
     $                INFO )
         CALL CHKXER( 'ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHPEVD( 'N', '/', 0, A, RW, Z, 1, Q, 0, X, 0, IW, 0,
     $                INFO )
         CALL CHKXER( 'ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHPEVD( 'N', 'U', -1, A, RW, Z, 1, Q, 0, X, 0, IW, 0,
     $                INFO )
         CALL CHKXER( 'ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZHPEVD( 'V', 'U', 2, A, RW, Z, 1, Q, 0, X, 0, IW, 0,
     $                INFO )
         CALL CHKXER( 'ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZHPEVD( 'N', 'U', 2, A, RW, Z, 2, Q, 0, X, 0, IW, 0,
     $                INFO )
         CALL CHKXER( 'ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZHPEVD( 'V', 'U', 2, A, RW, Z, 2, Q, 1, X, 0, IW, 0,
     $                INFO )
         CALL CHKXER( 'ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZHPEVD( 'N', 'U', 2, A, RW, Z, 2, Q, 99, X, 0, IW, 0,
     $                INFO )
         CALL CHKXER( 'ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZHPEVD( 'V', 'U', 2, A, RW, Z, 2, Q, 99, X, 1, IW, 0,
     $                INFO )
         CALL CHKXER( 'ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL ZHPEVD( 'N', 'U', 2, A, RW, Z, 2, Q, 99, X, 99, IW, 0,
     $                INFO )
         CALL CHKXER( 'ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL ZHPEVD( 'V', 'U', 2, A, RW, Z, 2, Q, 99, X, 99, IW, 1,
     $                INFO )
         CALL CHKXER( 'ZHPEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        ZHPEV
*
         SRNAMT = 'ZHPEV'
         INFOT = 1
         CALL ZHPEV( '/', 'U', 0, A, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'ZHPEV', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHPEV( 'N', '/', 0, A, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'ZHPEV', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHPEV( 'N', 'U', -1, A, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'ZHPEV', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZHPEV( 'V', 'U', 2, A, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'ZHPEV', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        ZHPEVX
*
         SRNAMT = 'ZHPEVX'
         INFOT = 1
         CALL ZHPEVX( '/', 'A', 'U', 0, A, VL, VU, IL, IU, TOL, M, X, Z,
     $                1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHPEVX( 'V', '/', 'U', 0, A, VL, VU, IL, IU, TOL, M, X, Z,
     $                1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHPEVX( 'V', 'A', '/', -1, A, VL, VU, IL, IU, TOL, M, X,
     $                Z, 1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZHPEVX( 'V', 'A', 'U', -1, A, VL, VU, IL, IU, TOL, M, X,
     $                Z, 1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZHPEVX( 'V', 'V', 'U', 1, A, 0.0D0, 0.0D0, IL, IU, TOL, M,
     $                X, Z, 1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZHPEVX( 'V', 'I', 'U', 1, A, 0.0D0, 0.0D0, 0, IU, TOL, M,
     $                X, Z, 1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZHPEVX( 'V', 'I', 'U', 2, A, 0.0D0, 0.0D0, 2, 1, TOL, M,
     $                X, Z, 1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL ZHPEVX( 'V', 'I', 'U', 1, A, 0.0D0, 0.0D0, 1, 1, TOL, M,
     $                X, Z, 0, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the HB path.
*
      ELSE IF( LSAMEN( 2, C2, 'HB' ) ) THEN
*
*        ZHBTRD
*
         SRNAMT = 'ZHBTRD'
         INFOT = 1
         CALL ZHBTRD( '/', 'U', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'ZHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHBTRD( 'N', '/', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'ZHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHBTRD( 'N', 'U', -1, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'ZHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZHBTRD( 'N', 'U', 0, -1, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'ZHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZHBTRD( 'N', 'U', 1, 1, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'ZHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZHBTRD( 'V', 'U', 2, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'ZHBTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        ZHBEVD
*
         SRNAMT = 'ZHBEVD'
         INFOT = 1
         CALL ZHBEVD( '/', 'U', 0, KD, A, 1, X, Z, 1, W, 0, RW, 0, IW,
     $                0, INFO )
         CALL CHKXER( 'ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHBEVD( 'N', '/', 0, KD, A, 1, X, Z, 1, W, 0, RW, 0, IW,
     $                0, INFO )
         CALL CHKXER( 'ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHBEVD( 'N', 'U', -1, KD, A, 1, X, Z, 1, W, 0, RW, 0, IW,
     $                0, INFO )
         CALL CHKXER( 'ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZHBEVD( 'N', 'U', 1, -1, A, 1, X, Z, 1, W, 0, RW, 0, IW,
     $                0, INFO )
         CALL CHKXER( 'ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZHBEVD( 'N', 'U', 2, 1, A, 1, X, Z, 1, W, 0, RW, 0, IW, 0,
     $                INFO )
         CALL CHKXER( 'ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZHBEVD( 'N', 'U', 2, 1, A, 5, X, Z, 0, W, 0, RW, 0, IW, 0,
     $                INFO )
         CALL CHKXER( 'ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZHBEVD( 'N', 'U', 2, 1, A, 5, X, Z, 2, W, 0, RW, 0, IW, 0,
     $                INFO )
         CALL CHKXER( 'ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZHBEVD( 'V', 'U', 2, 1, A, 5, X, Z, 2, W, 1, RW, 0, IW, 0,
     $                INFO )
         CALL CHKXER( 'ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL ZHBEVD( 'N', 'U', 2, 1, A, 5, X, Z, 2, W, 99, RW, 0, IW,
     $                0, INFO )
         CALL CHKXER( 'ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL ZHBEVD( 'V', 'U', 2, 1, A, 5, X, Z, 2, W, 99, RW, 1, IW,
     $                0, INFO )
         CALL CHKXER( 'ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL ZHBEVD( 'N', 'U', 2, 1, A, 5, X, Z, 2, W, 99, RW, 99, IW,
     $                0, INFO )
         CALL CHKXER( 'ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL ZHBEVD( 'V', 'U', 2, 1, A, 5, X, Z, 2, W, 99, RW, 99, IW,
     $                1, INFO )
         CALL CHKXER( 'ZHBEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 12
*
*        ZHBEV
*
         SRNAMT = 'ZHBEV'
         INFOT = 1
         CALL ZHBEV( '/', 'U', 0, KD, A, 1, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'ZHBEV', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHBEV( 'N', '/', 0, KD, A, 1, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'ZHBEV', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHBEV( 'N', 'U', -1, KD, A, 1, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'ZHBEV', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZHBEV( 'N', 'U', 2, -1, A, 1, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'ZHBEV', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZHBEV( 'N', 'U', 2, 1, A, 1, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'ZHBEV', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZHBEV( 'N', 'U', 2, 1, A, 5, X, Z, 0, W, RW, INFO )
         CALL CHKXER( 'ZHBEV', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        ZHBEVX
*
         SRNAMT = 'ZHBEVX'
         INFOT = 1
         CALL ZHBEVX( '/', 'A', 'U', 0, KD, A, 1, Q, 1, VL, VU, IL, IU,
     $                TOL, M, X, Z, 1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHBEVX( 'V', '/', 'U', 0, KD, A, 1, Q, 1, VL, VU, IL, IU,
     $                TOL, M, X, Z, 1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHBEVX( 'V', 'A', '/', -1, KD, A, 1, Q, 1, VL, VU, IL, IU,
     $                TOL, M, X, Z, 1, W, RW, IW, I1, INFO )
         INFOT = 4
         CALL ZHBEVX( 'V', 'A', 'U', -1, KD, A, 1, Q, 1, VL, VU, IL, IU,
     $                TOL, M, X, Z, 1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZHBEVX( 'V', 'A', 'U', 2, -1, A, 1, Q, 1, VL, VU, IL, IU,
     $                TOL, M, X, Z, 1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZHBEVX( 'V', 'A', 'U', 2, 1, A, 1, Q, 1, VL, VU, IL, IU,
     $                TOL, M, X, Z, 1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZHBEVX( 'V', 'A', 'U', 2, 1, A, 5, Q, 1, VL, VU, IL, IU,
     $                TOL, M, X, Z, 1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZHBEVX( 'V', 'V', 'U', 1, 1, A, 5, Q, 1, 0.0D0, 0.0D0, IL,
     $                IU, TOL, M, X, Z, 1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZHBEVX( 'V', 'I', 'U', 1, 1, A, 5, Q, 1, 0.0D0, 0.0D0, 0,
     $                IU, TOL, M, X, Z, 1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL ZHBEVX( 'V', 'I', 'U', 1, 1, A, 5, Q, 1, 0.0D0, 0.0D0, 1,
     $                2, TOL, M, X, Z, 1, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL ZHBEVX( 'V', 'I', 'U', 1, 1, A, 5, Q, 1, 0.0D0, 0.0D0, 1,
     $                1, TOL, M, X, Z, 0, W, RW, IW, I1, INFO )
         CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )
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
*     End of ZERRST
*
      END
