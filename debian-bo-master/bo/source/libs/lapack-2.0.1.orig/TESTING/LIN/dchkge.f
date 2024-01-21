      SUBROUTINE DCHKGE( DOTYPE, NM, MVAL, NN, NVAL, NNB, NBVAL, NRHS,
     $                   THRESH, TSTERR, NMAX, A, AFAC, AINV, B, X,
     $                   XACT, WORK, RWORK, IWORK, NOUT )
*
*  -- LAPACK test routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NM, NMAX, NN, NNB, NOUT, NRHS
      DOUBLE PRECISION   THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), MVAL( * ), NBVAL( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), AFAC( * ), AINV( * ), B( * ),
     $                   RWORK( * ), WORK( * ), X( * ), XACT( * )
*     ..
*
*  Purpose
*  =======
*
*  DCHKGE tests DGETRF, -TRI, -TRS, -RFS, and -CON.
*
*  Arguments
*  =========
*
*  DOTYPE  (input) LOGICAL array, dimension (NTYPES)
*          The matrix types to be used for testing.  Matrices of type j
*          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
*          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
*
*  NM      (input) INTEGER
*          The number of values of M contained in the vector MVAL.
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row dimension M.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix column dimension N.
*
*  NNB     (input) INTEGER
*          The number of values of NB contained in the vector NBVAL.
*
*  NBVAL   (input) INTEGER array, dimension (NBVAL)
*          The values of the blocksize NB.
*
*  NRHS    (input) INTEGER
*          The number of right hand side vectors to be generated for
*          each linear system.
*
*  THRESH  (input) DOUBLE PRECISION
*          The threshold value for the test ratios.  A result is
*          included in the output file if RESULT >= THRESH.  To have
*          every test ratio printed, use THRESH = 0.
*
*  TSTERR  (input) LOGICAL
*          Flag that indicates whether error exits are to be tested.
*
*  NMAX    (input) INTEGER
*          The maximum value permitted for M or N, used in dimensioning
*          the work arrays.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
*
*  AFAC    (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
*
*  AINV    (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
*
*  B       (workspace) DOUBLE PRECISION array, dimension (NMAX*NRHS)
*
*  X       (workspace) DOUBLE PRECISION array, dimension (NMAX*NRHS)
*
*  XACT    (workspace) DOUBLE PRECISION array, dimension (NMAX*NRHS)
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension
*                      (NMAX*max(3,NRHS))
*
*  RWORK   (workspace) DOUBLE PRECISION array, dimension
*                      (max(2*NMAX,2*NRHS+NWORK))
*
*  IWORK   (workspace) INTEGER array, dimension (2*NMAX)
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER            NTYPES
      PARAMETER          ( NTYPES = 11 )
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 8 )
      INTEGER            NTRAN
      PARAMETER          ( NTRAN = 3 )
*     ..
*     .. Local Scalars ..
      LOGICAL            TRFCON, ZEROT
      CHARACTER          DIST, NORM, TRANS, TYPE, XTYPE
      CHARACTER*3        PATH
      INTEGER            I, IM, IMAT, IN, INB, INFO, IOFF, ITRAN, IZERO,
     $                   K, K1, KL, KU, LDA, LWORK, M, MODE, N, NB,
     $                   NERRS, NFAIL, NIMAT, NRUN, NT
      DOUBLE PRECISION   AINVNM, ANORM, ANORMI, ANORMO, CNDNUM, DUMMY,
     $                   RCOND, RCONDC, RCONDI, RCONDO
*     ..
*     .. Local Arrays ..
      CHARACTER          TRANSS( NTRAN )
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      DOUBLE PRECISION   RESULT( NTESTS )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DGET06, DLANGE
      EXTERNAL           DGET06, DLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAERH, ALAHD, ALASUM, DERRGE, DGECON, DGERFS,
     $                   DGET01, DGET02, DGET03, DGET04, DGET07, DGETRF,
     $                   DGETRI, DGETRS, DLACPY, DLARHS, DLASET, DLATB4,
     $                   DLATMS, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*6        SRNAMT
      INTEGER            INFOT, NUNIT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NUNIT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Data statements ..
      DATA               ISEEDY / 1988, 1989, 1990, 1991 / ,
     $                   TRANSS / 'N', 'T', 'C' /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      PATH( 1: 1 ) = 'D'
      PATH( 2: 3 ) = 'GE'
      NRUN = 0
      NFAIL = 0
      NERRS = 0
      DO 10 I = 1, 4
         ISEED( I ) = ISEEDY( I )
   10 CONTINUE
*
*     Test the error exits
*
      IF( TSTERR )
     $   CALL DERRGE( PATH, NOUT )
      INFOT = 0
      CALL XLAENV( 2, 2 )
*
*     Do for each value of M in MVAL
*
      DO 90 IM = 1, NM
         M = MVAL( IM )
         LDA = MAX( 1, M )
*
*        Do for each value of N in NVAL
*
         DO 80 IN = 1, NN
            N = NVAL( IN )
            XTYPE = 'N'
            NIMAT = NTYPES
            IF( M.LE.0 .OR. N.LE.0 )
     $         NIMAT = 1
*
            DO 70 IMAT = 1, NIMAT
*
*              Do the tests only if DOTYPE( IMAT ) is true.
*
               IF( .NOT.DOTYPE( IMAT ) )
     $            GO TO 70
*
*              Skip types 5, 6, or 7 if the matrix size is too small.
*
               ZEROT = IMAT.GE.5 .AND. IMAT.LE.7
               IF( ZEROT .AND. N.LT.IMAT-4 )
     $            GO TO 70
*
*              Set up parameters with DLATB4 and generate a test matrix
*              with DLATMS.
*
               CALL DLATB4( PATH, IMAT, M, N, TYPE, KL, KU, ANORM, MODE,
     $                      CNDNUM, DIST )
*
               SRNAMT = 'DLATMS'
               CALL DLATMS( M, N, DIST, ISEED, TYPE, RWORK, MODE,
     $                      CNDNUM, ANORM, KL, KU, 'No packing', A, LDA,
     $                      WORK, INFO )
*
*              Check error code from DLATMS.
*
               IF( INFO.NE.0 ) THEN
                  CALL ALAERH( PATH, 'DLATMS', INFO, 0, ' ', M, N, -1,
     $                         -1, -1, IMAT, NFAIL, NERRS, NOUT )
                  GO TO 70
               END IF
*
*              For types 5-7, zero one or more columns of the matrix to
*              test that INFO is returned correctly.
*
               IF( ZEROT ) THEN
                  IF( IMAT.EQ.5 ) THEN
                     IZERO = 1
                  ELSE IF( IMAT.EQ.6 ) THEN
                     IZERO = MIN( M, N )
                  ELSE
                     IZERO = MIN( M, N ) / 2 + 1
                  END IF
                  IOFF = ( IZERO-1 )*LDA
                  IF( IMAT.LT.7 ) THEN
                     DO 20 I = 1, M
                        A( IOFF+I ) = ZERO
   20                CONTINUE
                  ELSE
                     CALL DLASET( 'Full', M, N-IZERO+1, ZERO, ZERO,
     $                            A( IOFF+1 ), LDA )
                  END IF
               ELSE
                  IZERO = 0
               END IF
*
*              These lines, if used in place of the calls in the DO 60
*              loop, cause the code to bomb on a Sun SPARCstation.
*
*               ANORMO = DLANGE( 'O', M, N, A, LDA, RWORK )
*               ANORMI = DLANGE( 'I', M, N, A, LDA, RWORK )
*
*              Do for each blocksize in NBVAL
*
               DO 60 INB = 1, NNB
                  NB = NBVAL( INB )
                  CALL XLAENV( 1, NB )
*
*                 Compute the LU factorization of the matrix.
*
                  CALL DLACPY( 'Full', M, N, A, LDA, AFAC, LDA )
                  SRNAMT = 'DGETRF'
                  CALL DGETRF( M, N, AFAC, LDA, IWORK, INFO )
*
*                 Check error code from DGETRF.
*
                  IF( INFO.NE.IZERO )
     $               CALL ALAERH( PATH, 'DGETRF', INFO, IZERO, ' ', M,
     $                            N, -1, -1, NB, IMAT, NFAIL, NERRS,
     $                            NOUT )
                  TRFCON = .FALSE.
*
*+    TEST 1
*                 Reconstruct matrix from factors and compute residual.
*
                  CALL DLACPY( 'Full', M, N, AFAC, LDA, AINV, LDA )
                  CALL DGET01( M, N, A, LDA, AINV, LDA, IWORK, RWORK,
     $                         RESULT( 1 ) )
                  NT = 1
*
*+    TEST 2
*                 Form the inverse if the factorization was successful
*                 and compute the residual.
*
                  IF( M.EQ.N .AND. INFO.EQ.0 ) THEN
                     CALL DLACPY( 'Full', N, N, AFAC, LDA, AINV, LDA )
                     SRNAMT = 'DGETRI'
                     LWORK = NMAX*MAX( 3, NRHS )
                     CALL DGETRI( N, AINV, LDA, IWORK, WORK, LWORK,
     $                            INFO )
*
*                    Check error code from DGETRI.
*
                     IF( INFO.NE.0 )
     $                  CALL ALAERH( PATH, 'DGETRI', INFO, 0, ' ', N, N,
     $                               -1, -1, NB, IMAT, NFAIL, NERRS,
     $                               NOUT )
*
*                    Compute the residual for the matrix times its
*                    inverse.  Also compute the 1-norm condition number
*                    of A.
*
                     CALL DGET03( N, A, LDA, AINV, LDA, WORK, LDA,
     $                            RWORK, RCONDO, RESULT( 2 ) )
                     ANORMO = DLANGE( 'O', M, N, A, LDA, RWORK )
*
*                    Compute the infinity-norm condition number of A.
*
                     ANORMI = DLANGE( 'I', M, N, A, LDA, RWORK )
                     AINVNM = DLANGE( 'I', N, N, AINV, LDA, RWORK )
                     IF( ANORMI.LE.ZERO .OR. AINVNM.LE.ZERO ) THEN
                        RCONDI = ONE
                     ELSE
                        RCONDI = ( ONE / ANORMI ) / AINVNM
                     END IF
                     NT = 2
                  ELSE
*
*                    Do only the condition estimate if INFO > 0.
*
                     TRFCON = .TRUE.
                     ANORMO = DLANGE( 'O', M, N, A, LDA, RWORK )
                     ANORMI = DLANGE( 'I', M, N, A, LDA, RWORK )
                     RCONDO = ZERO
                     RCONDI = ZERO
                  END IF
*
*                 Print information about the tests so far that did not
*                 pass the threshold.
*
                  DO 30 K = 1, NT
                     IF( RESULT( K ).GE.THRESH ) THEN
                        IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                     CALL ALAHD( NOUT, PATH )
                        WRITE( NOUT, FMT = 9999 )M, N, NB, IMAT, K,
     $                     RESULT( K )
                        NFAIL = NFAIL + 1
                     END IF
   30             CONTINUE
                  NRUN = NRUN + NT
*
*                 Skip the remaining tests if this is not the first
*                 block size or if M .ne. N.
*
                  IF( INB.GT.1 .OR. M.NE.N )
     $               GO TO 60
*
                  DO 50 ITRAN = 1, NTRAN
                     TRANS = TRANSS( ITRAN )
                     IF( ITRAN.EQ.1 ) THEN
                        ANORM = ANORMO
                        RCONDC = RCONDO
                        NORM = 'O'
                     ELSE
                        ANORM = ANORMI
                        RCONDC = RCONDI
                        NORM = 'I'
                     END IF
                     IF( TRFCON ) THEN
                        K1 = 8
                     ELSE
                        K1 = 3
*
*+    TEST 3
*                       Solve and compute residual for A * X = B.
*
                        SRNAMT = 'DLARHS'
                        CALL DLARHS( PATH, XTYPE, ' ', TRANS, N, N, KL,
     $                               KU, NRHS, A, LDA, XACT, LDA, B,
     $                               LDA, ISEED, INFO )
                        XTYPE = 'C'
*
                        CALL DLACPY( 'Full', N, NRHS, B, LDA, X, LDA )
                        SRNAMT = 'DGETRS'
                        CALL DGETRS( TRANS, N, NRHS, AFAC, LDA, IWORK,
     $                               X, LDA, INFO )
*
*                       Check error code from DGETRS.
*
                        IF( INFO.NE.0 )
     $                     CALL ALAERH( PATH, 'DGETRS', INFO, 0, TRANS,
     $                                  N, N, -1, -1, NRHS, IMAT, NFAIL,
     $                                  NERRS, NOUT )
*
                        CALL DLACPY( 'Full', N, NRHS, B, LDA, WORK,
     $                               LDA )
                        CALL DGET02( TRANS, N, N, NRHS, A, LDA, X, LDA,
     $                               WORK, LDA, RWORK, RESULT( 3 ) )
*
*+    TEST 4
*                       Check solution from generated exact solution.
*
                        CALL DGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                               RESULT( 4 ) )
*
*+    TESTS 5, 6, and 7
*                       Use iterative refinement to improve the
*                       solution.
*
                        SRNAMT = 'DGERFS'
                        CALL DGERFS( TRANS, N, NRHS, A, LDA, AFAC, LDA,
     $                               IWORK, B, LDA, X, LDA, RWORK,
     $                               RWORK( NRHS+1 ), WORK,
     $                               IWORK( N+1 ), INFO )
*
*                       Check error code from DGERFS.
*
                        IF( INFO.NE.0 )
     $                     CALL ALAERH( PATH, 'DGERFS', INFO, 0, TRANS,
     $                                  N, N, -1, -1, NRHS, IMAT, NFAIL,
     $                                  NERRS, NOUT )
*
                        CALL DGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                               RESULT( 5 ) )
                        CALL DGET07( TRANS, N, NRHS, A, LDA, B, LDA, X,
     $                               LDA, XACT, LDA, RWORK,
     $                               RWORK( NRHS+1 ), RESULT( 6 ) )
                        NT = 7
                     END IF
*
*+    TEST 8
*                    Get an estimate of RCOND = 1/CNDNUM.
*
                     IF( ITRAN.LE.2 ) THEN
                        SRNAMT = 'DGECON'
                        CALL DGECON( NORM, N, AFAC, LDA, ANORM, RCOND,
     $                               WORK, IWORK( N+1 ), INFO )
*
*                       Check error code from DGECON.
*
                        IF( INFO.NE.0 )
     $                     CALL ALAERH( PATH, 'DGECON', INFO, 0, NORM,
     $                                  N, N, -1, -1, -1, IMAT, NFAIL,
     $                                  NERRS, NOUT )
*
*                       This line is needed on a Sun SPARCstation.
*
                        DUMMY = RCOND
*
                        RESULT( 8 ) = DGET06( RCOND, RCONDC )
                        NT = 8
                     END IF
*
*                    Print information about the tests that did not pass
*                    the threshold.
*
                     DO 40 K = K1, NT
                        IF( RESULT( K ).GE.THRESH ) THEN
                           IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                        CALL ALAHD( NOUT, PATH )
                           IF( K.LT.8 ) THEN
                              WRITE( NOUT, FMT = 9998 )TRANS, N, NB,
     $                           IMAT, K, RESULT( K )
                           ELSE
                              WRITE( NOUT, FMT = 9997 )NORM, N, NB,
     $                           IMAT, K, RESULT( K )
                           END IF
                           NFAIL = NFAIL + 1
                        END IF
   40                CONTINUE
                     NRUN = NRUN + NT - K1 + 1
   50             CONTINUE
   60          CONTINUE
   70       CONTINUE
*
   80    CONTINUE
   90 CONTINUE
*
*     Print a summary of the results.
*
      CALL ALASUM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' M = ', I5, ', N =', I5, ', NB =', I4, ', type ', I2,
     $      ', test ', I2, ', ratio =', G12.5 )
 9998 FORMAT( ' TRANS=''', A1, ''', N =', I5, ', NB =', I4, ', type ',
     $      I2, ', test ', I2, ', ratio =', G12.5 )
 9997 FORMAT( ' NORM =''', A1, ''', N =', I5, ', NB =', I4, ', type ',
     $      I2, ', test ', I2, ', ratio =', G12.5 )
      RETURN
*
*     End of DCHKGE
*
      END
