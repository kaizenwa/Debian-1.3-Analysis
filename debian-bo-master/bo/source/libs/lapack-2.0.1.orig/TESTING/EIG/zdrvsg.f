      SUBROUTINE ZDRVSG( NSIZES, NN, NTYPES, DOTYPE, ISEED, THRESH,
     $                   NOUNIT, A, LDA, B, LDB, D, U, LDU, BB, V, Z,
     $                   UZ, WORK, NWORK, RWORK, IWORK, RESULT, INFO )
*
*  -- LAPACK test routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LDU, NOUNIT, NSIZES, NTYPES,
     $                   NWORK
      DOUBLE PRECISION   THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            ISEED( 4 ), IWORK( * ), NN( * )
      DOUBLE PRECISION   D( * ), RESULT( 14 ), RWORK( * )
      COMPLEX*16         A( LDA, * ), B( LDB, * ), BB( LDB, * ), U( * ),
     $                   UZ( * ), V( LDU, * ), WORK( * ), Z( LDU, * )
*     ..
*
*  Purpose
*  =======
*
*       ZDRVSG checks the complex Hermitian generalized eigenproblem
*       drivers.
*
*               ZHEGV computes all eigenvalues and, optionally,
*               eigenvectors of a complex Hermitian-definite generalized
*               eigenproblem.
*
*               ZHPGV computes all eigenvalues and, optionally,
*               eigenvectors of a complex Hermitian-definite generalized
*               eigenproblem in packed storage.
*
*               ZHBGV computes all eigenvalues and, optionally,
*               eigenvectors of a complex Hermitian-definite banded
*               generalized eigenproblem.
*
*       When ZDRVSG is called, a number of matrix "sizes" ("n's") and a
*       number of matrix "types" are specified.  For each size ("n")
*       and each type of matrix, one matrix A of the given type will be
*       generated; a random well-conditioned matrix B is also generated
*       and the pair (A,B) is used to test the drivers.
*
*       For each pair (A,B), the following tests are performed:
*
*       (1) ZHEGV with ITYPE = 1 and UPLO ='U':
*
*               | A Z - B Z D | / ( |A| |Z| n ulp )
*
*       (2) as (1) but calling ZHPGV
*       (3) as (1) but calling ZHBGV
*       (4) as (1) but with UPLO = 'L'
*       (5) as (4) but calling ZHPGV
*       (6) as (4) but calling ZHBGV
*
*       (7) ZHEGV with ITYPE = 2 and UPLO ='U':
*
*               | A B Z - Z D | / ( |A| |Z| n ulp )
*
*       (8) as (7) but calling ZHPGV
*       (9) as (7) but with UPLO = 'L'
*       (10) as (9) but calling ZHPGV
*
*       (11) ZHEGV with ITYPE = 3 and UPLO ='U':
*
*               | B A Z - Z D | / ( |A| |Z| n ulp )
*
*       (12) as (11) but calling ZHPGV
*       (13) as (11) but with UPLO = 'L'
*       (14) as (13) but calling ZHPGV
*
*       The "sizes" are specified by an array NN(1:NSIZES); the value of
*       each element NN(j) specifies one size.
*       The "types" are specified by a logical array DOTYPE( 1:NTYPES );
*       if DOTYPE(j) is .TRUE., then matrix type "j" will be generated.
*       This type is used for the matrix A which has half-bandwidth KA.
*       B is generated as a well-conditioned positive definite matrix
*       with half-bandwidth KB (<= KA).
*       Currently, the list of possible types for A is:
*
*       (1)  The zero matrix.
*       (2)  The identity matrix.
*
*       (3)  A diagonal matrix with evenly spaced entries
*            1, ..., ULP  and random signs.
*            (ULP = (first number larger than 1) - 1 )
*       (4)  A diagonal matrix with geometrically spaced entries
*            1, ..., ULP  and random signs.
*       (5)  A diagonal matrix with "clustered" entries 1, ULP, ..., ULP
*            and random signs.
*
*       (6)  Same as (4), but multiplied by SQRT( overflow threshold )
*       (7)  Same as (4), but multiplied by SQRT( underflow threshold )
*
*       (8)  A matrix of the form  U* D U, where U is unitary and
*            D has evenly spaced entries 1, ..., ULP with random signs
*            on the diagonal.
*
*       (9)  A matrix of the form  U* D U, where U is unitary and
*            D has geometrically spaced entries 1, ..., ULP with random
*            signs on the diagonal.
*
*       (10) A matrix of the form  U* D U, where U is unitary and
*            D has "clustered" entries 1, ULP,..., ULP with random
*            signs on the diagonal.
*
*       (11) Same as (8), but multiplied by SQRT( overflow threshold )
*       (12) Same as (8), but multiplied by SQRT( underflow threshold )
*
*       (13) Hermitian matrix with random entries chosen from (-1,1).
*       (14) Same as (13), but multiplied by SQRT( overflow threshold )
*       (15) Same as (13), but multiplied by SQRT( underflow threshold )
*
*       (16) Same as (8), but with KA = 1 and KB = 1
*       (17) Same as (8), but with KA = 2 and KB = 1
*       (18) Same as (8), but with KA = 2 and KB = 2
*       (19) Same as (8), but with KA = 3 and KB = 1
*       (20) Same as (8), but with KA = 3 and KB = 2
*       (21) Same as (8), but with KA = 3 and KB = 3
*
*  Arguments
*  =========
*
*  NSIZES  INTEGER
*          The number of sizes of matrices to use.  If it is zero,
*          ZDRVSG does nothing.  It must be at least zero.
*          Not modified.
*
*  NN      INTEGER array, dimension (NSIZES)
*          An array containing the sizes to be used for the matrices.
*          Zero values will be skipped.  The values must be at least
*          zero.
*          Not modified.
*
*  NTYPES  INTEGER
*          The number of elements in DOTYPE.   If it is zero, ZDRVSG
*          does nothing.  It must be at least zero.  If it is MAXTYP+1
*          and NSIZES is 1, then an additional type, MAXTYP+1 is
*          defined, which is to use whatever matrix is in A.  This
*          is only useful if DOTYPE(1:MAXTYP) is .FALSE. and
*          DOTYPE(MAXTYP+1) is .TRUE. .
*          Not modified.
*
*  DOTYPE  LOGICAL array, dimension (NTYPES)
*          If DOTYPE(j) is .TRUE., then for each size in NN a
*          matrix of that size and of type j will be generated.
*          If NTYPES is smaller than the maximum number of types
*          defined (PARAMETER MAXTYP), then types NTYPES+1 through
*          MAXTYP will not be generated.  If NTYPES is larger
*          than MAXTYP, DOTYPE(MAXTYP+1) through DOTYPE(NTYPES)
*          will be ignored.
*          Not modified.
*
*  ISEED   INTEGER array, dimension (4)
*          On entry ISEED specifies the seed of the random number
*          generator. The array elements should be between 0 and 4095;
*          if not they will be reduced mod 4096.  Also, ISEED(4) must
*          be odd.  The random number generator uses a linear
*          congruential sequence limited to small integers, and so
*          should produce machine independent random numbers. The
*          values of ISEED are changed on exit, and can be used in the
*          next call to ZDRVSG to continue the same random number
*          sequence.
*          Modified.
*
*  THRESH  DOUBLE PRECISION
*          A test will count as "failed" if the "error", computed as
*          described above, exceeds THRESH.  Note that the error
*          is scaled to be O(1), so THRESH should be a reasonably
*          small multiple of 1, e.g., 10 or 100.  In particular,
*          it should not depend on the precision (single vs. double)
*          or the size of the matrix.  It must be at least zero.
*          Not modified.
*
*  NOUNIT  INTEGER
*          The FORTRAN unit number for printing out error messages
*          (e.g., if a routine returns IINFO not equal to 0.)
*          Not modified.
*
*  A       COMPLEX*16 array, dimension (LDA , max(NN))
*          Used to hold the matrix whose eigenvalues are to be
*          computed.  On exit, A contains the last matrix actually
*          used.
*          Modified.
*
*  LDA     INTEGER
*          The leading dimension of A.  It must be at
*          least 1 and at least max( NN ).
*          Not modified.
*
*  B       COMPLEX*16 array, dimension (LDB , max(NN))
*          Used to hold the Hermitian positive definite matrix for
*          the generailzed problem.
*          On exit, B contains the last matrix actually
*          used.
*          Modified.
*
*  LDB     INTEGER
*          The leading dimension of B.  It must be at
*          least 1 and at least max( NN ).
*          Not modified.
*
*  D       DOUBLE PRECISION array, dimension (max(NN))
*          The eigenvalues of A. On exit, the eigenvalues in D
*          correspond with the matrix in A.
*          Modified.
*
*  U       COMPLEX*16 array, dimension (LDU, max(NN))
*          Workspace.
*          Modified.
*
*  LDU     INTEGER
*          The leading dimension of U, BB, V, Z, and UZ.  It must be at
*          least 1 and at least max( NN ).
*          Not modified.
*
*  BB      COMPLEX*16 array, dimension (LDU, max(NN))
*          Workspace.
*          Modified.
*
*  V       COMPLEX*16 array, dimension (LDU, max(NN))
*          Workspace.
*          Modified.
*
*  Z       COMPLEX*16 array, dimension (LDU, max(NN))
*          The matrix of eigenvectors.
*          Modified.
*
*  UZ      COMPLEX*16 array, dimension (LDU, max(NN))
*          Workspace.
*          Modified.
*
*  WORK    COMPLEX*16 array, dimension (NWORK)
*          Workspace.
*          Modified.
*
*  NWORK   INTEGER
*          The number of entries in WORK.  This must be at least
*          2*max( NN(j), 2 )**2.
*          Not modified.
*
*  RWORK   DOUBLE PRECISION array, dimension (3*max(NN))
*          Workspace.
*          Modified.
*
*  IWORK   INTEGER array, dimension (max(NN))
*          Workspace.
*          Modified.
*
*  RESULT  DOUBLE PRECISION array, dimension (14)
*          The values computed by the 14 tests described above.
*          The values are currently limited to 1/ulp, to avoid
*          overflow.
*          Modified.
*
*  INFO    INTEGER
*          If 0, then everything ran OK.
*           -1: NSIZES < 0
*           -2: Some NN(j) < 0
*           -3: NTYPES < 0
*           -5: THRESH < 0
*           -9: LDA < 1 or LDA < NMAX, where NMAX is max( NN(j) ).
*          -16: LDU < 1 or LDU < NMAX.
*          -21: NWORK too small.
*          If  ZLATMR, CLATMS, ZHEGV, ZHPGV or ZHBGV
*              returns an error code, the
*              absolute value of it is returned.
*          Modified.
*
*-----------------------------------------------------------------------
*
*       Some Local Variables and Parameters:
*       ---- ----- --------- --- ----------
*       ZERO, ONE       Real 0 and 1.
*       MAXTYP          The number of types defined.
*       NTEST           The number of tests that have been run
*                       on this matrix.
*       NTESTT          The total number of tests for this call.
*       NMAX            Largest value in NN.
*       NMATS           The number of matrices generated so far.
*       NERRS           The number of tests which have exceeded THRESH
*                       so far (computed by DLAFTS).
*       COND, IMODE     Values to be passed to the matrix generators.
*       ANORM           Norm of A; passed to matrix generators.
*
*       OVFL, UNFL      Overflow and underflow thresholds.
*       ULP, ULPINV     Finest relative precision and its inverse.
*       RTOVFL, RTUNFL  Square roots of the previous 2 values.
*               The following four arrays decode JTYPE:
*       KTYPE(j)        The general type (1-10) for type "j".
*       KMODE(j)        The MODE value to be passed to the matrix
*                       generator for type "j".
*       KMAGN(j)        The order of magnitude ( O(1),
*                       O(overflow^(1/2) ), O(underflow^(1/2) )
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TEN
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TEN = 10.0D+0 )
      COMPLEX*16         CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ),
     $                   CONE = ( 1.0D+0, 0.0D+0 ) )
      INTEGER            MAXTYP
      PARAMETER          ( MAXTYP = 21 )
*     ..
*     .. Local Scalars ..
      LOGICAL            BADNN
      CHARACTER          UPLO
      INTEGER            I, IBTYPE, IBUPLO, IINFO, IJ, IMODE, ITYPE, J,
     $                   JCOL, JSIZE, JTYPE, KA, KA9, KB, KB9, MTYPES,
     $                   N, NERRS, NMATS, NMAX, NTEST, NTESTT
      DOUBLE PRECISION   ANINV, ANORM, COND, OVFL, RTOVFL, RTUNFL, ULP,
     $                   ULPINV, UNFL
*     ..
*     .. Local Arrays ..
      INTEGER            IDUMMA( 1 ), IOLDSD( 4 ), KMAGN( MAXTYP ),
     $                   KMODE( MAXTYP ), KTYPE( MAXTYP )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLABAD, DLAFTS, DLASUM, XERBLA, ZHBGV, ZHEGV,
     $                   ZHPGV, ZLACPY, ZLASET, ZLATMR, ZLATMS, ZSGT01
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, SQRT
*     ..
*     .. Data statements ..
      DATA               KTYPE / 1, 2, 5*4, 5*5, 3*8, 6*9 /
      DATA               KMAGN / 2*1, 1, 1, 1, 2, 3, 1, 1, 1, 2, 3, 1,
     $                   2, 3, 6*1 /
      DATA               KMODE / 2*0, 4, 3, 1, 4, 4, 4, 3, 1, 4, 4, 0,
     $                   0, 0, 6*4 /
*     ..
*     .. Executable Statements ..
*
*     1)      Check for errors
*
      NTESTT = 0
      INFO = 0
*
      BADNN = .FALSE.
      NMAX = 0
      DO 10 J = 1, NSIZES
         NMAX = MAX( NMAX, NN( J ) )
         IF( NN( J ).LT.0 )
     $      BADNN = .TRUE.
   10 CONTINUE
*
*     Check for errors
*
      IF( NSIZES.LT.0 ) THEN
         INFO = -1
      ELSE IF( BADNN ) THEN
         INFO = -2
      ELSE IF( NTYPES.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LE.1 .OR. LDA.LT.NMAX ) THEN
         INFO = -9
      ELSE IF( LDU.LE.1 .OR. LDU.LT.NMAX ) THEN
         INFO = -16
      ELSE IF( 2*MAX( NMAX, 2 )**2.GT.NWORK ) THEN
         INFO = -21
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZDRVSG', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( NSIZES.EQ.0 .OR. NTYPES.EQ.0 )
     $   RETURN
*
*     More Important constants
*
      UNFL = DLAMCH( 'Safe minimum' )
      OVFL = DLAMCH( 'Overflow' )
      CALL DLABAD( UNFL, OVFL )
      ULP = DLAMCH( 'Epsilon' )*DLAMCH( 'Base' )
      ULPINV = ONE / ULP
      RTUNFL = SQRT( UNFL )
      RTOVFL = SQRT( OVFL )
*
*     Loop over sizes, types
*
      NERRS = 0
      NMATS = 0
*
      DO 240 JSIZE = 1, NSIZES
         N = NN( JSIZE )
         ANINV = ONE / DBLE( MAX( 1, N ) )
*
         IF( NSIZES.NE.1 ) THEN
            MTYPES = MIN( MAXTYP, NTYPES )
         ELSE
            MTYPES = MIN( MAXTYP+1, NTYPES )
         END IF
*
         KA9 = 0
         KB9 = 0
         DO 230 JTYPE = 1, MTYPES
            IF( .NOT.DOTYPE( JTYPE ) )
     $         GO TO 230
            NMATS = NMATS + 1
            NTEST = 0
*
            DO 20 J = 1, 4
               IOLDSD( J ) = ISEED( J )
   20       CONTINUE
*
*           2)      Compute "A"
*
*                   Control parameters:
*
*               KMAGN  KMODE        KTYPE
*           =1  O(1)   clustered 1  zero
*           =2  large  clustered 2  identity
*           =3  small  exponential  (none)
*           =4         arithmetic   diagonal, w/ eigenvalues
*           =5         random log   hermitian, w/ eigenvalues
*           =6         random       (none)
*           =7                      random diagonal
*           =8                      random hermitian
*           =9                      banded, w/ eigenvalues
*
            IF( MTYPES.GT.MAXTYP )
     $         GO TO 80
*
            ITYPE = KTYPE( JTYPE )
            IMODE = KMODE( JTYPE )
*
*           Compute norm
*
            GO TO ( 30, 40, 50 )KMAGN( JTYPE )
*
   30       CONTINUE
            ANORM = ONE
            GO TO 60
*
   40       CONTINUE
            ANORM = ( RTOVFL*ULP )*ANINV
            GO TO 60
*
   50       CONTINUE
            ANORM = RTUNFL*N*ULPINV
            GO TO 60
*
   60       CONTINUE
*
            IINFO = 0
            COND = ULPINV
*
*           Special Matrices -- Identity & Jordan block
*
            IF( ITYPE.EQ.1 ) THEN
*
*              Zero
*
               KA = 0
               KB = 0
               CALL ZLASET( 'Full', LDA, N, CZERO, CZERO, A, LDA )
*
            ELSE IF( ITYPE.EQ.2 ) THEN
*
*              Identity
*
               KA = 0
               KB = 0
               CALL ZLASET( 'Full', LDA, N, CZERO, CZERO, A, LDA )
               DO 70 JCOL = 1, N
                  A( JCOL, JCOL ) = ANORM
   70          CONTINUE
*
            ELSE IF( ITYPE.EQ.4 ) THEN
*
*              Diagonal Matrix, [Eigen]values Specified
*
               KA = 0
               KB = 0
               CALL ZLATMS( N, N, 'S', ISEED, 'H', RWORK, IMODE, COND,
     $                      ANORM, 0, 0, 'N', A, LDA, WORK, IINFO )
*
            ELSE IF( ITYPE.EQ.5 ) THEN
*
*              Hermitian, eigenvalues specified
*
               KA = MAX( 0, N-1 )
               KB = KA
               CALL ZLATMS( N, N, 'S', ISEED, 'H', RWORK, IMODE, COND,
     $                      ANORM, N, N, 'N', A, LDA, WORK, IINFO )
*
            ELSE IF( ITYPE.EQ.7 ) THEN
*
*              Diagonal, random eigenvalues
*
               KA = 0
               KB = 0
               CALL ZLATMR( N, N, 'S', ISEED, 'H', WORK, 6, ONE, CONE,
     $                      'T', 'N', WORK( N+1 ), 1, ONE,
     $                      WORK( 2*N+1 ), 1, ONE, 'N', IDUMMA, 0, 0,
     $                      ZERO, ANORM, 'NO', A, LDA, IWORK, IINFO )
*
            ELSE IF( ITYPE.EQ.8 ) THEN
*
*              Hermitian, random eigenvalues
*
               KA = MAX( 0, N-1 )
               KB = KA
               CALL ZLATMR( N, N, 'S', ISEED, 'H', WORK, 6, ONE, CONE,
     $                      'T', 'N', WORK( N+1 ), 1, ONE,
     $                      WORK( 2*N+1 ), 1, ONE, 'N', IDUMMA, N, N,
     $                      ZERO, ANORM, 'NO', A, LDA, IWORK, IINFO )
*
            ELSE IF( ITYPE.EQ.9 ) THEN
*
*              Hermitian banded, eigenvalues specified
*
*              The following values are used for the half-bandwidths:
*
*                ka = 1   kb = 1
*                ka = 2   kb = 1
*                ka = 2   kb = 2
*                ka = 3   kb = 1
*                ka = 3   kb = 2
*                ka = 3   kb = 3
*
               KB9 = KB9 + 1
               IF( KB9.GT.KA9 ) THEN
                  KA9 = KA9 + 1
                  KB9 = 1
               END IF
               KA = MAX( 0, MIN( N-1, KA9 ) )
               KB = MAX( 0, MIN( N-1, KB9 ) )
               CALL ZLATMS( N, N, 'S', ISEED, 'H', RWORK, IMODE, COND,
     $                      ANORM, KA, KA, 'N', A, LDA, WORK, IINFO )
*
            ELSE
*
               IINFO = 1
            END IF
*
            IF( IINFO.NE.0 ) THEN
               WRITE( NOUNIT, FMT = 9999 )'Generator', IINFO, N, JTYPE,
     $            IOLDSD
               INFO = ABS( IINFO )
               RETURN
            END IF
*
   80       CONTINUE
*
*           3) Call ZHEGV, ZHPGV and ZHBGV to compute S and U, do tests.
*
*               loop over the three generalized problems
*                 IBTYPE = 1: A*x = (lambda)*B*x
*                 IBTYPE = 2: A*B*x = (lambda)*x
*                 IBTYPE = 3: B*A*x = (lambda)*x
*
            DO 220 IBTYPE = 1, 3
*
*              loop over the setting UPLO
*
               DO 210 IBUPLO = 1, 2
                  IF( IBUPLO.EQ.1 )
     $               UPLO = 'U'
                  IF( IBUPLO.EQ.2 )
     $               UPLO = 'L'
*
*                 Generate random well-conditioned positive definite
*                 matrix B, of bandwidth not greater than that of A.
*
                  CALL ZLATMS( N, N, 'U', ISEED, 'P', RWORK, 5, TEN,
     $                         ONE, KB, KB, UPLO, B, LDB, WORK( N+1 ),
     $                         IINFO )
*
                  NTEST = NTEST + 1
*
                  CALL ZLACPY( ' ', N, N, A, LDA, Z, LDU )
                  CALL ZLACPY( UPLO, N, N, B, LDB, BB, LDB )
*
                  CALL ZHEGV( IBTYPE, 'V', UPLO, N, Z, LDU, BB, LDB, D,
     $                        WORK, NWORK, RWORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUNIT, FMT = 9999 )'ZHEGV(V,' // UPLO //
     $                  ')', IINFO, N, JTYPE, IOLDSD
                     INFO = ABS( IINFO )
                     IF( IINFO.LT.0 ) THEN
                        RETURN
                     ELSE
                        RESULT( NTEST ) = ULPINV
                        GO TO 90
                     END IF
                  END IF
*
*                 Do Test
*
                  CALL ZSGT01( IBTYPE, UPLO, N, A, LDA, B, LDB, Z, LDU,
     $                         D, UZ, RWORK, RESULT( NTEST ) )
*
   90             CONTINUE
                  NTEST = NTEST + 1
*
*                 Copy the matrices into packed storage.
*
                  IF( LSAME( UPLO, 'U' ) ) THEN
                     IJ = 1
                     DO 110 J = 1, N
                        DO 100 I = 1, J
                           UZ( IJ ) = A( I, J )
                           U( IJ ) = B( I, J )
                           IJ = IJ + 1
  100                   CONTINUE
  110                CONTINUE
                  ELSE
                     IJ = 1
                     DO 130 J = 1, N
                        DO 120 I = J, N
                           UZ( IJ ) = A( I, J )
                           U( IJ ) = B( I, J )
                           IJ = IJ + 1
  120                   CONTINUE
  130                CONTINUE
                  END IF
*
                  CALL ZHPGV( IBTYPE, 'V', UPLO, N, UZ, U, D, Z, LDU,
     $                        WORK, RWORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUNIT, FMT = 9999 )'ZHPGV(V,' // UPLO //
     $                  ')', IINFO, N, JTYPE, IOLDSD
                     INFO = ABS( IINFO )
                     IF( IINFO.LT.0 ) THEN
                        RETURN
                     ELSE
                        RESULT( NTEST ) = ULPINV
                        GO TO 140
                     END IF
                  END IF
*
*                 Do Test
*
                  CALL ZSGT01( IBTYPE, UPLO, N, A, LDA, B, LDB, Z, LDU,
     $                         D, UZ, RWORK, RESULT( NTEST ) )
*
  140             CONTINUE
                  IF( IBTYPE.EQ.1 ) THEN
                     NTEST = NTEST + 1
*
*                    Copy the matrices into band storage.
*
                     IF( LSAME( UPLO, 'U' ) ) THEN
                        DO 170 J = 1, N
                           DO 150 I = MAX( 1, J-KA ), J
                              V( KA+1+I-J, J ) = A( I, J )
  150                      CONTINUE
                           DO 160 I = MAX( 1, J-KB ), J
                              BB( KB+1+I-J, J ) = B( I, J )
  160                      CONTINUE
  170                   CONTINUE
                     ELSE
                        DO 200 J = 1, N
                           DO 180 I = J, MIN( N, J+KA )
                              V( 1+I-J, J ) = A( I, J )
  180                      CONTINUE
                           DO 190 I = J, MIN( N, J+KB )
                              BB( 1+I-J, J ) = B( I, J )
  190                      CONTINUE
  200                   CONTINUE
                     END IF
*
                     CALL ZHBGV( 'V', UPLO, N, KA, KB, V, LDU, BB, LDB,
     $                           D, Z, LDU, WORK, RWORK, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUNIT, FMT = 9999 )'ZHBGV(V,' //
     $                     UPLO // ')', IINFO, N, JTYPE, IOLDSD
                        INFO = ABS( IINFO )
                        IF( IINFO.LT.0 ) THEN
                           RETURN
                        ELSE
                           RESULT( NTEST ) = ULPINV
                           GO TO 210
                        END IF
                     END IF
*
*                    Do Test
*
                     CALL ZSGT01( IBTYPE, UPLO, N, A, LDA, B, LDB, Z,
     $                            LDU, D, UZ, RWORK, RESULT( NTEST ) )
                  END IF
*
  210          CONTINUE
  220       CONTINUE
*
*           End of Loop -- Check for RESULT(j) > THRESH
*
            NTESTT = NTESTT + NTEST
            CALL DLAFTS( 'ZSG', N, N, JTYPE, NTEST, RESULT, IOLDSD,
     $                   THRESH, NOUNIT, NERRS )
  230    CONTINUE
  240 CONTINUE
*
*     Summary
*
      CALL DLASUM( 'ZSG', NOUNIT, NERRS, NTESTT )
*
      RETURN
*
 9999 FORMAT( ' ZDRVSG: ', A, ' returned INFO=', I6, '.', / 9X, 'N=',
     $      I6, ', JTYPE=', I6, ', ISEED=(', 3( I5, ',' ), I5, ')' )
*
*     End of ZDRVSG
*
      END
