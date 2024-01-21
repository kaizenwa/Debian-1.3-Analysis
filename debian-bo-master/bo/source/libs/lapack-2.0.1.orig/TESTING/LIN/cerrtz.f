      SUBROUTINE CERRTZ( PATH, NUNIT )
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
*  CERRTZ tests the error exits for CTZRQF.
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
*     ..
*     .. Local Arrays ..
      COMPLEX            A( NMAX, NMAX ), TAU( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, CTZRQF
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
      INTRINSIC          CMPLX
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      C2 = PATH( 2: 3 )
      A( 1, 1 ) = CMPLX( 1., -1. )
      A( 1, 2 ) = CMPLX( 2., -2. )
      A( 2, 2 ) = CMPLX( 3., -3. )
      A( 2, 1 ) = CMPLX( 4., -4. )
      OK = .TRUE.
*
*     Test error exits for the trapezoidal routines.
*
      WRITE( NOUT, FMT = * )
      IF( LSAMEN( 2, C2, 'TZ' ) ) THEN
*
*        CTZRQF
*
         SRNAMT = 'CTZRQF'
         INFOT = 1
         CALL CTZRQF( -1, 0, A, 1, TAU, INFO )
         CALL CHKXER( 'CTZRQF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CTZRQF( 1, 0, A, 1, TAU, INFO )
         CALL CHKXER( 'CTZRQF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CTZRQF( 2, 2, A, 1, TAU, INFO )
         CALL CHKXER( 'CTZRQF', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of CERRTZ
*
      END
