      SUBROUTINE CDRVLS( DOTYPE, NM, MVAL, NN, NVAL, NNS, NSVAL, NNB,
     $                   NBVAL, NXVAL, THRESH, TSTERR, A, COPYA, B,
     $                   COPYB, S, COPYS, TAU, WORK, RWORK, IWORK,
     $                   NOUT )
*
*  -- LAPACK test routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NM, NN, NNB, NNS, NOUT
      REAL               THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), MVAL( * ), NBVAL( * ), NSVAL( * ),
     $                   NVAL( * ), NXVAL( * )
      REAL               COPYS( * ), RWORK( * ), S( * )
      COMPLEX            A( * ), B( * ), COPYA( * ), COPYB( * ),
     $                   TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  CDRVLS tests the least squares driver routines CGELS, CGELSX, and
*  CGELSS.
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
*          The number of values of NB and NX contained in the
*          vectors NBVAL and NXVAL.  The blocking parameters are used
*          in pairs (NB,NX).
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NXVAL   (input) INTEGER array, dimension (NNB)
*          The values of the crossover point NX.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  THRESH  (input) REAL
*          The threshold value for the test ratios.  A result is
*          included in the output file if RESULT >= THRESH.  To have
*          every test ratio printed, use THRESH = 0.
*
*  TSTERR  (input) LOGICAL
*          Flag that indicates whether error exits are to be tested.
*
*  A       (workspace) COMPLEX array, dimension (MMAX*NMAX)
*          where MMAX is the maximum value of M in MVAL and NMAX is the
*          maximum value of N in NVAL.
*
*  COPYA   (workspace) COMPLEX array, dimension (MMAX*NMAX)
*
*  B       (workspace) COMPLEX array, dimension (MMAX*NMAX)
*          where MMAX is the maximum value of M in MVAL and NMAX is the
*          maximum value of N in NVAL.
*
*  COPYB   (workspace) COMPLEX array, dimension (MMAX*NMAX)
*
*  S       (workspace) REAL array, dimension
*                      (min(MMAX,NMAX))
*
*  COPYS   (workspace) REAL array, dimension
*                      (min(MMAX,NMAX))
*
*  TAU     (workspace) COMPLEX array, dimension (MMAX)
*
*  WORK    (workspace) COMPLEX array, dimension
*                      (MMAX*NMAX + 4*NMAX + MMAX)
*
*  RWORK   (workspace) REAL array, dimension (5*NMAX-1)
*
*  IWORK   (workspace) INTEGER array, dimension (NMAX)
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 10 )
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E0, ZERO = 0.0E0 )
      COMPLEX            CONE, CZERO
      PARAMETER          ( CONE = ( 1.0E+0, 0.0E+0 ),
     $                   CZERO = ( 0.0E+0, 0.0E+0 ) )
*     ..
*     .. Local Scalars ..
      CHARACTER          TRANS
      CHARACTER*3        PATH
      INTEGER            CRANK, I, IM, IN, INB, INFO, INS, IRANK,
     $                   ISCALE, ITRAN, ITYPE, J, K, LDA, LDB, LDWORK,
     $                   LW, LWORK, M, MNMIN, N, NB, NCOLS, NERRS,
     $                   NFAIL, NRHS, NROWS, NRUN, RANK
      REAL               EPS, NORMA, NORMB, RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      REAL               RESULT( NTESTS )
*     ..
*     .. External Functions ..
      REAL               CQRT12, CQRT14, CQRT17, SASUM, SCNRM2, SLAMCH
      EXTERNAL           CQRT12, CQRT14, CQRT17, SASUM, SCNRM2, SLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAERH, ALAHD, ALASVM, CERRLS, CGELS, CGELSS,
     $                   CGELSX, CGEMM, CLACPY, CLARNV, CQRT13, CQRT15,
     $                   CQRT16, CSSCAL, SAXPY, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, REAL, SQRT
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
*     .. Data statements ..
      DATA               ISEEDY / 1988, 1989, 1990, 1991 /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      PATH( 1: 1 ) = 'C'
      PATH( 2: 3 ) = 'LS'
      NRUN = 0
      NFAIL = 0
      NERRS = 0
      DO 10 I = 1, 4
         ISEED( I ) = ISEEDY( I )
   10 CONTINUE
      EPS = SLAMCH( 'Epsilon' )
*
*     Threshold for rank estimation
*
      RCOND = SQRT( EPS ) - ( SQRT( EPS )-EPS ) / 2
*
*     Test the error exits
*
      IF( TSTERR )
     $   CALL CERRLS( PATH, NOUT )
      INFOT = 0
*
      DO 130 IM = 1, NM
         M = MVAL( IM )
         LDA = MAX( 1, M )
*
         DO 120 IN = 1, NN
            N = NVAL( IN )
            MNMIN = MIN( M, N )
            LDB = MAX( 1, M, N )
*
            DO 110 INS = 1, NNS
               NRHS = NSVAL( INS )
               LWORK = MAX( 1, ( M+NRHS )*( N+2 ),
     $                 M*N+4*MNMIN+MAX( M, N ), 2*N+M )
*
               DO 100 IRANK = 1, 2
                  DO 90 ISCALE = 1, 3
                     ITYPE = ( IRANK-1 )*3 + ISCALE
*
                     IF( IRANK.EQ.1 ) THEN
*
*                       Test CGELS
*
*                       Generate a matrix of scaling type ISCALE
*
                        CALL CQRT13( ISCALE, M, N, COPYA, LDA, NORMA,
     $                               ISEED )
                        DO 50 INB = 1, NNB
                           NB = NBVAL( INB )
                           CALL XLAENV( 1, NB )
                           CALL XLAENV( 3, NXVAL( INB ) )
*
                           DO 40 ITRAN = 1, 2
                              IF( ITRAN.EQ.1 ) THEN
                                 TRANS = 'N'
                                 NROWS = M
                                 NCOLS = N
                              ELSE
                                 TRANS = 'C'
                                 NROWS = N
                                 NCOLS = M
                              END IF
                              LW = MAX( 1, ( NROWS+NRHS )*( NCOLS+2 ),
     $                             MNMIN+MAX( M, N, NRHS )*
     $                             MAX( 1, NB ) )
                              LDWORK = MAX( 1, NCOLS )
*
*                             Set up a consistent rhs
*
                              IF( NCOLS.GT.0 ) THEN
                                 CALL CLARNV( 2, ISEED, NCOLS*NRHS,
     $                                        WORK )
                                 DO 20 J = 1, NRHS
                                    CALL CSSCAL( NCOLS,
     $                                           ONE / SCNRM2( NCOLS,
     $                                           WORK( ( J-1 )*NCOLS+
     $                                           1 ), 1 ), WORK( ( J-
     $                                           1 )*NCOLS+1 ), 1 )
   20                            CONTINUE
                              END IF
                              CALL CGEMM( TRANS, 'No transpose', NROWS,
     $                                    NRHS, NCOLS, CONE, COPYA, LDA,
     $                                    WORK, LDWORK, CZERO, B, LDB )
                              CALL CLACPY( 'Full', NROWS, NRHS, B, LDB,
     $                                     COPYB, LDB )
*
*                             Solve LS or overdetermined system
*
                              IF( M.GT.0 .AND. N.GT.0 ) THEN
                                 CALL CLACPY( 'Full', M, N, COPYA, LDA,
     $                                        A, LDA )
                                 CALL CLACPY( 'Full', NROWS, NRHS,
     $                                        COPYB, LDB, B, LDB )
                              END IF
                              SRNAMT = 'CGELS '
                              CALL CGELS( TRANS, M, N, NRHS, A, LDA, B,
     $                                    LDB, WORK, LW, INFO )
*
                              IF( INFO.NE.0 )
     $                           CALL ALAERH( PATH, 'CGELS ', INFO, 0,
     $                                        TRANS, M, N, NRHS, -1, NB,
     $                                        ITYPE, NFAIL, NERRS,
     $                                        NOUT )
*
*                             Check correctness of results
*
                              LDWORK = MAX( 1, NROWS )
                              IF( NROWS.GT.0 .AND. NRHS.GT.0 )
     $                           CALL CLACPY( 'Full', NROWS, NRHS,
     $                                        COPYB, LDB, WORK, LDWORK )
                              CALL CQRT16( TRANS, M, N, NRHS, COPYA,
     $                                     LDA, B, LDB, WORK, LDWORK,
     $                                     RWORK, RESULT( 1 ) )
*
                              IF( ( ITRAN.EQ.1 .AND. M.GE.N ) .OR.
     $                            ( ITRAN.EQ.2 .AND. M.LT.N ) ) THEN
*
*                                Solving LS system
*
                                 RESULT( 2 ) = CQRT17( TRANS, 1, M, N,
     $                                         NRHS, COPYA, LDA, B, LDB,
     $                                         COPYB, LDB, WORK, LW )
                              ELSE
*
*                                Solving overdetermined system
*
                                 RESULT( 2 ) = CQRT14( TRANS, M, N,
     $                                         NRHS, COPYA, LDA, B, LDB,
     $                                         WORK, LW )
                              END IF
*
*                             Print information about the tests that
*                             did not pass the threshold.
*
                              DO 30 K = 1, 2
                                 IF( RESULT( K ).GE.THRESH ) THEN
                                    IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                                 CALL ALAHD( NOUT, PATH )
                                    WRITE( NOUT, FMT = 9999 )TRANS, M,
     $                                 N, NRHS, NB, ITYPE, K,
     $                                 RESULT( K )
                                    NFAIL = NFAIL + 1
                                 END IF
   30                         CONTINUE
                              NRUN = NRUN + 2
   40                      CONTINUE
   50                   CONTINUE
                     END IF
*
*                    Generate a matrix of scaling type ISCALE and rank
*                    type IRANK.
*
                     CALL CQRT15( ISCALE, IRANK, M, N, NRHS, COPYA, LDA,
     $                            COPYB, LDB, COPYS, RANK, NORMA, NORMB,
     $                            ISEED, WORK, LWORK )
*
*                    workspace used: MAX(M+MIN(M,N),NRHS*MIN(M,N),2*N+M)
*
                     DO 60 J = 1, N
                        IWORK( J ) = 0
   60                CONTINUE
                     LDWORK = MAX( 1, M )
*
                     DO 80 INB = 1, NNB
                        NB = NBVAL( INB )
                        CALL XLAENV( 1, NB )
                        CALL XLAENV( 3, NXVAL( INB ) )
*
                        CALL CLACPY( 'Full', M, N, COPYA, LDA, A, LDA )
                        CALL CLACPY( 'Full', M, NRHS, COPYB, LDB, B,
     $                               LDB )
*
*                       CGELSX:  Compute the minimum-norm solution X to
*                          min( norm( A * X - B ) )
*                       using a complete orthogonal factorization.
*
                        SRNAMT = 'CGELSX'
                        CALL CGELSX( M, N, NRHS, A, LDA, B, LDB, IWORK,
     $                               RCOND, CRANK, WORK, RWORK, INFO )
*
                        IF( INFO.NE.0 )
     $                     CALL ALAERH( PATH, 'CGELSX', INFO, 0, ' ', M,
     $                                  N, NRHS, -1, NB, ITYPE, NFAIL,
     $                                  NERRS, NOUT )
*
*                       workspace used: MAX( MNMIN+3*N, 2*MNMIN+NRHS )
*
*                       Test 3:  Compute relative error in svd
*                                workspace: M*N + 4*MIN(M,N) + MAX(M,N)
*
                        RESULT( 3 ) = CQRT12( CRANK, CRANK, A, LDA,
     $                                COPYS, WORK, LWORK, RWORK )
*
*                       Test 4:  Compute error in solution
*                                workspace:  M*NRHS + M
*
                        CALL CLACPY( 'Full', M, NRHS, COPYB, LDB, WORK,
     $                               LDWORK )
                        CALL CQRT16( 'No transpose', M, N, NRHS, COPYA,
     $                               LDA, B, LDB, WORK, LDWORK, RWORK,
     $                               RESULT( 4 ) )
*
*                       Test 5:  Check norm of r'*A
*                                workspace: NRHS*(M+N)
*
                        RESULT( 5 ) = ZERO
                        IF( M.GT.CRANK )
     $                     RESULT( 5 ) = CQRT17( 'No transpose', 1, M,
     $                                   N, NRHS, COPYA, LDA, B, LDB,
     $                                   COPYB, LDB, WORK, LWORK )
*
*                       Test 6:  Check if x is in the rowspace of A
*                                workspace: (M+NRHS)*(N+2)
*
                        RESULT( 6 ) = ZERO
*
                        IF( N.GT.CRANK )
     $                     RESULT( 6 ) = CQRT14( 'No transpose', M, N,
     $                                   NRHS, COPYA, LDA, B, LDB, WORK,
     $                                   LWORK )
*
*                       CGELSS:  Compute the minimum-norm solution X to
*                          min( norm( A * X - B ) )
*                       using the SVD.
*
                        CALL CLACPY( 'Full', M, N, COPYA, LDA, A, LDA )
                        CALL CLACPY( 'Full', M, NRHS, COPYB, LDB, B,
     $                               LDB )
                        SRNAMT = 'CGELSS'
                        CALL CGELSS( M, N, NRHS, A, LDA, B, LDB, S,
     $                               RCOND, CRANK, WORK, LWORK, RWORK,
     $                               INFO )
*
                        IF( INFO.NE.0 )
     $                     CALL ALAERH( PATH, 'CGELSS', INFO, 0, ' ', M,
     $                                  N, NRHS, -1, NB, ITYPE, NFAIL,
     $                                  NERRS, NOUT )
*
*                       workspace used: 3*min(m,n) +
*                                       max(2*min(m,n),nrhs,max(m,n))
*
*                       Test 7:  Compute relative error in svd
*
                        IF( RANK.GT.0 ) THEN
                           CALL SAXPY( MNMIN, -ONE, COPYS, 1, S, 1 )
                           RESULT( 7 ) = SASUM( MNMIN, S, 1 ) /
     $                                   SASUM( MNMIN, COPYS, 1 ) /
     $                                   ( EPS*REAL( MNMIN ) )
                        ELSE
                           RESULT( 7 ) = ZERO
                        END IF
*
*                       Test 8:  Compute error in solution
*
                        CALL CLACPY( 'Full', M, NRHS, COPYB, LDB, WORK,
     $                               LDWORK )
                        CALL CQRT16( 'No transpose', M, N, NRHS, COPYA,
     $                               LDA, B, LDB, WORK, LDWORK, RWORK,
     $                               RESULT( 8 ) )
*
*                       Test 9:  Check norm of r'*A
*
                        RESULT( 9 ) = ZERO
                        IF( M.GT.CRANK )
     $                     RESULT( 9 ) = CQRT17( 'No transpose', 1, M,
     $                                   N, NRHS, COPYA, LDA, B, LDB,
     $                                   COPYB, LDB, WORK, LWORK )
*
*                       Test 10:  Check if x is in the rowspace of A
*
                        RESULT( 10 ) = ZERO
                        IF( N.GT.CRANK )
     $                     RESULT( 10 ) = CQRT14( 'No transpose', M, N,
     $                                    NRHS, COPYA, LDA, B, LDB,
     $                                    WORK, LWORK )
*
*                       Print information about the tests that did not
*                       pass the threshold.
*
                        DO 70 K = 3, 10
                           IF( RESULT( K ).GE.THRESH ) THEN
                              IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                           CALL ALAHD( NOUT, PATH )
                              WRITE( NOUT, FMT = 9998 )M, N, NRHS, NB,
     $                           ITYPE, K, RESULT( K )
                              NFAIL = NFAIL + 1
                           END IF
   70                   CONTINUE
                        NRUN = NRUN + 8
*
   80                CONTINUE
   90             CONTINUE
  100          CONTINUE
  110       CONTINUE
  120    CONTINUE
  130 CONTINUE
*
*     Print a summary of the results.
*
      CALL ALASVM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' TRANS=''', A1, ''', M=', I5, ', N=', I5, ', NRHS=', I4,
     $      ', NB=', I4, ', type', I2, ', test(', I2, ')=', G12.5 )
 9998 FORMAT( ' M=', I5, ', N=', I5, ', NRHS=', I4, ', NB=', I4,
     $      ', type', I2, ', test(', I2, ')=', G12.5 )
*
*     End of CDRVLS
*
      END
