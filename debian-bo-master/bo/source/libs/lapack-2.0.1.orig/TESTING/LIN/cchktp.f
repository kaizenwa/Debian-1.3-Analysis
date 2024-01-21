      SUBROUTINE CCHKTP( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, NMAX,
     $                   AP, AINVP, B, X, XACT, WORK, RWORK, NOUT )
*
*  -- LAPACK test routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NMAX, NN, NOUT, NRHS
      REAL               THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            NVAL( * )
      REAL               RWORK( * )
      COMPLEX            AINVP( * ), AP( * ), B( * ), WORK( * ), X( * ),
     $                   XACT( * )
*     ..
*
*  Purpose
*  =======
*
*  CCHKTP tests CTPTRI, -TRS, -RFS, and -CON, and CLATPS
*
*  Arguments
*  =========
*
*  DOTYPE  (input) LOGICAL array, dimension (NTYPES)
*          The matrix types to be used for testing.  Matrices of type j
*          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
*          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix column dimension N.
*
*  NRHS    (input) INTEGER
*          The number of right hand side vectors to be generated for
*          each linear system.
*
*  THRESH  (input) REAL
*          The threshold value for the test ratios.  A result is
*          included in the output file if RESULT >= THRESH.  To have
*          every test ratio printed, use THRESH = 0.
*
*  TSTERR  (input) LOGICAL
*          Flag that indicates whether error exits are to be tested.
*
*  NMAX    (input) INTEGER
*          The leading dimension of the work arrays.  NMAX >= the
*          maximumm value of N in NVAL.
*
*  AP      (workspace) COMPLEX array, dimension (NMAX*(NMAX+1)/2)
*
*  AINVP   (workspace) COMPLEX array, dimension (NMAX*(NMAX+1)/2)
*
*  B       (workspace) COMPLEX array, dimension (NMAX*NRHS)
*
*  X       (workspace) COMPLEX array, dimension (NMAX*NRHS)
*
*  XACT    (workspace) COMPLEX array, dimension (NMAX*NRHS)
*
*  WORK    (workspace) COMPLEX array, dimension
*                      (NMAX*max(3,NRHS))
*
*  RWORK   (workspace) REAL array, dimension
*                      (max(NMAX,2*NRHS))
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NTYPE1, NTYPES
      PARAMETER          ( NTYPE1 = 10, NTYPES = 18 )
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 9 )
      INTEGER            NTRAN
      PARAMETER          ( NTRAN = 3 )
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          DIAG, NORM, TRANS, UPLO, XTYPE
      CHARACTER*3        PATH
      INTEGER            I, IDIAG, IMAT, IN, INFO, ITRAN, IUPLO, K, K1,
     $                   LAP, LDA, N, NERRS, NFAIL, NRUN, NT
      REAL               AINVNM, ANORM, RCOND, RCONDC, RCONDI, RCONDO,
     $                   SCALE
*     ..
*     .. Local Arrays ..
      CHARACTER          TRANSS( NTRAN ), UPLOS( 2 )
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      REAL               RESULT( NTESTS )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      REAL               CLANTP
      EXTERNAL           LSAME, CLANTP
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAERH, ALAHD, ALASUM, CCOPY, CERRTR, CGET04,
     $                   CLACPY, CLARHS, CLATPS, CLATTP, CTPCON, CTPRFS,
     $                   CTPT01, CTPT02, CTPT03, CTPT05, CTPT06, CTPTRI,
     $                   CTPTRS
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*6        SRNAMT
      INTEGER            INFOT, IOUNIT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, IOUNIT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Data statements ..
      DATA               ISEEDY / 1988, 1989, 1990, 1991 /
      DATA               UPLOS / 'U', 'L' / , TRANSS / 'N', 'T', 'C' /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      PATH( 1: 1 ) = 'C'
      PATH( 2: 3 ) = 'TP'
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
     $   CALL CERRTR( PATH, NOUT )
      INFOT = 0
*
      DO 90 IN = 1, NN
*
*        Do for each value of N in NVAL
*
         N = NVAL( IN )
         LDA = MAX( 1, N )
         LAP = LDA*( LDA+1 ) / 2
         XTYPE = 'N'
*
         DO 50 IMAT = 1, NTYPE1
*
*           Do the tests only if DOTYPE( IMAT ) is true.
*
            IF( .NOT.DOTYPE( IMAT ) )
     $         GO TO 50
*
            DO 40 IUPLO = 1, 2
*
*              Do first for UPLO = 'U', then for UPLO = 'L'
*
               UPLO = UPLOS( IUPLO )
*
*              Call CLATTP to generate a triangular test matrix.
*
               SRNAMT = 'CLATTP'
               CALL CLATTP( IMAT, UPLO, 'No transpose', DIAG, ISEED, N,
     $                      AP, X, WORK, RWORK, INFO )
*
*              Set IDIAG = 1 for non-unit matrices, 2 for unit.
*
               IF( LSAME( DIAG, 'N' ) ) THEN
                  IDIAG = 1
               ELSE
                  IDIAG = 2
               END IF
*
*+    TEST 1
*              Form the inverse of A.
*
               IF( N.GT.0 )
     $            CALL CCOPY( LAP, AP, 1, AINVP, 1 )
               SRNAMT = 'CTPTRI'
               CALL CTPTRI( UPLO, DIAG, N, AINVP, INFO )
*
*              Check error code from CTPTRI.
*
               IF( INFO.NE.0 )
     $            CALL ALAERH( PATH, 'CTPTRI', INFO, 0, UPLO // DIAG, N,
     $                         N, -1, -1, -1, IMAT, NFAIL, NERRS, NOUT )
*
*              Compute the infinity-norm condition number of A.
*
               ANORM = CLANTP( 'I', UPLO, DIAG, N, AP, RWORK )
               AINVNM = CLANTP( 'I', UPLO, DIAG, N, AINVP, RWORK )
               IF( ANORM.LE.ZERO .OR. AINVNM.LE.ZERO ) THEN
                  RCONDI = ONE
               ELSE
                  RCONDI = ( ONE / ANORM ) / AINVNM
               END IF
*
*              Compute the residual for the triangular matrix times its
*              inverse.  Also compute the 1-norm condition number of A.
*
               CALL CTPT01( UPLO, DIAG, N, AP, AINVP, RCONDO, RWORK,
     $                      RESULT( 1 ) )
               K1 = 1
*
               DO 30 ITRAN = 1, NTRAN
*
*                 Do for op(A) = A, A**T, or A**H.
*
                  TRANS = TRANSS( ITRAN )
                  IF( ITRAN.EQ.1 ) THEN
                     NORM = 'O'
                     RCONDC = RCONDO
                  ELSE
                     NORM = 'I'
                     RCONDC = RCONDI
                     K1 = 2
                  END IF
*
*+    TEST 2
*                 Solve and compute residual for op(A)*x = b.
*
                  SRNAMT = 'CLARHS'
                  CALL CLARHS( PATH, XTYPE, UPLO, TRANS, N, N, 0, IDIAG,
     $                         NRHS, AP, LAP, XACT, LDA, B, LDA, ISEED,
     $                         INFO )
                  XTYPE = 'C'
                  CALL CLACPY( 'Full', N, NRHS, B, LDA, X, LDA )
*
                  SRNAMT = 'CTPTRS'
                  CALL CTPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, X, LDA,
     $                         INFO )
*
*                 Check error code from CTPTRS.
*
                  IF( INFO.NE.0 )
     $               CALL ALAERH( PATH, 'CTPTRS', INFO, 0,
     $                            UPLO // TRANS // DIAG, N, N, -1, -1,
     $                            -1, IMAT, NFAIL, NERRS, NOUT )
*
                  CALL CTPT02( UPLO, TRANS, DIAG, N, NRHS, AP, X, LDA,
     $                         B, LDA, WORK, RWORK, RESULT( 2 ) )
*
*+    TEST 3
*                 Check solution from generated exact solution.
*
                  CALL CGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                         RESULT( 3 ) )
*
*+    TESTS 4, 5, and 6
*                 Use iterative refinement to improve the solution and
*                 compute error bounds.
*
                  SRNAMT = 'CTPRFS'
                  CALL CTPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDA,
     $                         X, LDA, RWORK, RWORK( NRHS+1 ), WORK,
     $                         RWORK( 2*NRHS+1 ), INFO )
*
*                 Check error code from CTPRFS.
*
                  IF( INFO.NE.0 )
     $               CALL ALAERH( PATH, 'CTPRFS', INFO, 0,
     $                            UPLO // TRANS // DIAG, N, N, -1, -1,
     $                            NRHS, IMAT, NFAIL, NERRS, NOUT )
*
                  CALL CGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                         RESULT( 4 ) )
                  CALL CTPT05( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDA,
     $                         X, LDA, XACT, LDA, RWORK,
     $                         RWORK( NRHS+1 ), RESULT( 5 ) )
*
*+    TEST 7
*                 Get an estimate of RCOND = 1/CNDNUM.
*
                  SRNAMT = 'CTPCON'
                  CALL CTPCON( NORM, UPLO, DIAG, N, AP, RCOND, WORK,
     $                         RWORK, INFO )
*
*                 Check error code from CTPCON.
*
                  IF( INFO.NE.0 )
     $               CALL ALAERH( PATH, 'CTPCON', INFO, 0,
     $                            NORM // UPLO // DIAG, N, N, -1, -1,
     $                            -1, IMAT, NFAIL, NERRS, NOUT )
*
                  CALL CTPT06( RCOND, RCONDC, UPLO, DIAG, N, AP, RWORK,
     $                         RESULT( 7 ) )
                  NT = 7
*
*                 Print information about the tests that did not pass
*                 the threshold.
*
                  DO 20 K = K1, NT
                     IF( RESULT( K ).GE.THRESH ) THEN
                        IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                     CALL ALAHD( NOUT, PATH )
                        WRITE( NOUT, FMT = 9999 )UPLO, TRANS, N, IMAT,
     $                     K, RESULT( K )
                        NFAIL = NFAIL + 1
                     END IF
   20             CONTINUE
                  NRUN = NRUN + NT - K1 + 1
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
*
*        Use pathological test matrices to test CLATPS.
*
         DO 80 IMAT = NTYPE1 + 1, NTYPES
*
*           Do the tests only if DOTYPE( IMAT ) is true.
*
            IF( .NOT.DOTYPE( IMAT ) )
     $         GO TO 80
*
            DO 70 IUPLO = 1, 2
*
*              Do first for UPLO = 'U', then for UPLO = 'L'
*
               UPLO = UPLOS( IUPLO )
               DO 60 ITRAN = 1, NTRAN
*
*                 Do for op(A) = A, A**T, or A**H.
*
                  TRANS = TRANSS( ITRAN )
*
*                 Call CLATTP to generate a triangular test matrix.
*
                  SRNAMT = 'CLATTP'
                  CALL CLATTP( IMAT, UPLO, TRANS, DIAG, ISEED, N, AP, X,
     $                         WORK, RWORK, INFO )
*
*+    TEST 8
*                 Solve the system op(A)*x = b.
*
                  SRNAMT = 'CLATPS'
                  CALL CCOPY( N, X, 1, B, 1 )
                  CALL CLATPS( UPLO, TRANS, DIAG, 'N', N, AP, B, SCALE,
     $                         RWORK, INFO )
*
*                 Check error code from CLATPS.
*
                  IF( INFO.NE.0 )
     $               CALL ALAERH( PATH, 'CLATPS', INFO, 0,
     $                            UPLO // TRANS // DIAG // 'N', N, N,
     $                            -1, -1, -1, IMAT, NFAIL, NERRS, NOUT )
*
                  CALL CTPT03( UPLO, TRANS, DIAG, N, 1, AP, SCALE,
     $                         RWORK, ONE, B, LDA, X, LDA, WORK,
     $                         RESULT( 8 ) )
*
*+    TEST 9
*                 Solve op(A)*x = b again with NORMIN = 'Y'.
*
                  CALL CCOPY( N, X, 1, B( N+1 ), 1 )
                  CALL CLATPS( UPLO, TRANS, DIAG, 'Y', N, AP, B( N+1 ),
     $                         SCALE, RWORK, INFO )
*
*                 Check error code from CLATPS.
*
                  IF( INFO.NE.0 )
     $               CALL ALAERH( PATH, 'CLATPS', INFO, 0,
     $                            UPLO // TRANS // DIAG // 'Y', N, N,
     $                            -1, -1, -1, IMAT, NFAIL, NERRS, NOUT )
*
                  CALL CTPT03( UPLO, TRANS, DIAG, N, 1, AP, SCALE,
     $                         RWORK, ONE, B( N+1 ), LDA, X, LDA, WORK,
     $                         RESULT( 9 ) )
*
*                 Print information about the tests that did not pass
*                 the threshold.
*
                  IF( RESULT( 8 ).GE.THRESH ) THEN
                     IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                  CALL ALAHD( NOUT, PATH )
                     WRITE( NOUT, FMT = 9998 )'CLATPS', UPLO, TRANS,
     $                  DIAG, 'N', N, IMAT, RESULT( 8 )
                     NFAIL = NFAIL + 1
                  END IF
                  IF( RESULT( 9 ).GE.THRESH ) THEN
                     IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                  CALL ALAHD( NOUT, PATH )
                     WRITE( NOUT, FMT = 9998 )'CLATPS', UPLO, TRANS,
     $                  DIAG, 'Y', N, IMAT, RESULT( 9 )
                     NFAIL = NFAIL + 1
                  END IF
                  NRUN = NRUN + 2
   60          CONTINUE
   70       CONTINUE
   80    CONTINUE
   90 CONTINUE
*
*     Print a summary of the results.
*
      CALL ALASUM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' UPLO=''', A1, ''', TRANS=''', A1, ''', N=', I5,
     $      ', type ', I2, ', test(', I2, ')= ', G12.5 )
 9998 FORMAT( 1X, A6, '( ''', A1, ''', ''', A1, ''', ''', A1, ''', ''',
     $      A1, ''',', I5, ', ... ), type ', I2, ', ratio =', G12.5 )
      RETURN
*
*     End of CCHKTP
*
      END
