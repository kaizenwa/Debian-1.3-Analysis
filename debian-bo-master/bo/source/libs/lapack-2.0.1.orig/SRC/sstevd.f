      SUBROUTINE SSTEVD( JOBZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,
     $                   LIWORK, INFO )
*
*  -- LAPACK driver routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ
      INTEGER            INFO, LDZ, LIWORK, LWORK, N
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      REAL               D( * ), E( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  SSTEVD computes all eigenvalues and, optionally, eigenvectors of a
*  real symmetric tridiagonal matrix. If eigenvectors are desired, it
*  uses a divide and conquer algorithm.
*
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
*
*  Arguments
*  =========
*
*  JOBZ    (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only;
*          = 'V':  Compute eigenvalues and eigenvectors.
*
*  N       (input) INTEGER
*          The order of the matrix.  N >= 0.
*
*  D       (input/output) REAL array, dimension (N)
*          On entry, the n diagonal elements of the tridiagonal matrix
*          A.
*          On exit, if INFO = 0, the eigenvalues in ascending order.
*
*  E       (input/output) REAL array, dimension (N)
*          On entry, the (n-1) subdiagonal elements of the tridiagonal
*          matrix A, stored in elements 1 to N-1 of E; E(N) need not
*          be set, but is used by the routine.
*          On exit, the contents of E are destroyed.
*
*  Z       (output) REAL array, dimension (LDZ, N)
*          If JOBZ = 'V', then if INFO = 0, Z contains the orthonormal
*          eigenvectors of the matrix A, with the i-th column of Z
*          holding the eigenvector associated with D(i).
*          If JOBZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace/output) REAL array,
*                                         dimension (LWORK)
*          On exit, if LWORK > 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If JOBZ  = 'N' or N <= 1 then LWORK must be at least 1.
*          If JOBZ  = 'V' and N > 1 then LWORK must be at least
*                         ( 1 + 3*N + 2*N*lg N + 2*N**2 ),
*                         where lg( N ) = smallest integer k such
*                                         that 2**k >= N.
*
*  IWORK   (workspace/output) INTEGER array, dimension (LIWORK)
*          On exit, if LIWORK > 0, IWORK(1) returns the optimal LIWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array IWORK.
*          If JOBZ  = 'N' or N <= 1 then LIWORK must be at least 1.
*          If JOBZ  = 'V' and N > 1 then LIWORK must be at least 2+5*N.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the algorithm failed to converge; i
*                off-diagonal elements of E did not converge to zero.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0E0, ONE = 1.0E0, TWO = 2.0E0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            WANTZ
      INTEGER            ISCALE, LGN, LIWMIN, LWMIN
      REAL               BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA, SMLNUM,
     $                   TNRM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      REAL               SLAMCH, SLANST
      EXTERNAL           LSAME, SLAMCH, SLANST
*     ..
*     .. External Subroutines ..
      EXTERNAL           SSCAL, SSTEDC, SSTERF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          INT, LOG, REAL, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
*
      INFO = 0
      LIWMIN = 1
      LWMIN = 1
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
         INFO = -6
      ELSE IF( N.GT.1 .AND. WANTZ ) THEN
         LGN = INT( LOG( REAL( N ) ) / LOG( TWO ) )
         IF( 2**LGN.LT.N )
     $      LGN = LGN + 1
         IF( 2**LGN.LT.N )
     $      LGN = LGN + 1
         LWMIN = 1 + 3*N + 2*N*LGN + 2*N**2
         LIWMIN = 2 + 5*N
         IF( LWORK.LT.LWMIN ) THEN
            INFO = -8
         ELSE IF( LIWORK.LT.LIWMIN ) THEN
            INFO = -10
         END IF
      ELSE IF( LWORK.LT.1 ) THEN
         INFO = -8
      ELSE IF( LIWORK.LT.1 ) THEN
         INFO = -10
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SSTEVD', -INFO )
         GO TO 10
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   GO TO 10
*
      IF( N.EQ.1 ) THEN
         IF( WANTZ )
     $      Z( 1, 1 ) = ONE
         GO TO 10
      END IF
*
*     Get machine constants.
*
      SAFMIN = SLAMCH( 'Safe minimum' )
      EPS = SLAMCH( 'Precision' )
      SMLNUM = SAFMIN / EPS
      BIGNUM = ONE / SMLNUM
      RMIN = SQRT( SMLNUM )
      RMAX = SQRT( BIGNUM )
*
*     Scale matrix to allowable range, if necessary.
*
      ISCALE = 0
      TNRM = SLANST( 'M', N, D, E )
      IF( TNRM.GT.ZERO .AND. TNRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / TNRM
      ELSE IF( TNRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / TNRM
      END IF
      IF( ISCALE.EQ.1 ) THEN
         CALL SSCAL( N, SIGMA, D, 1 )
         CALL SSCAL( N-1, SIGMA, E( 1 ), 1 )
      END IF
*
*     For eigenvalues only, call SSTERF.  For eigenvalues and
*     eigenvectors, call SSTEDC.
*
      IF( .NOT.WANTZ ) THEN
         CALL SSTERF( N, D, E, INFO )
      ELSE
         CALL SSTEDC( 'I', N, D, E, Z, LDZ, WORK, LWORK, IWORK, LIWORK,
     $                INFO )
      END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
      IF( ISCALE.EQ.1 )
     $   CALL SSCAL( N, ONE / SIGMA, D, 1 )
*
   10 CONTINUE
      IF( LWORK.GT.0 )
     $   WORK( 1 ) = LWMIN
      IF( LIWORK.GT.0 )
     $   IWORK( 1 ) = LIWMIN
      RETURN
*
*     End of SSTEVD
*
      END
