      SUBROUTINE STIM22( LINE, NSIZES, NN, NTYPES, DOTYPE, NPARMS, NNB,
     $                   LDAS, TIMMIN, NOUT, ISEED, A, D, E, E2, Z,
     $                   WORK, LWORK, LLWORK, IWORK, TIMES, LDT1, LDT2,
     $                   LDT3, OPCNTS, LDO1, LDO2, LDO3, INFO )
*
*  -- LAPACK timing routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            INFO, LDO1, LDO2, LDO3, LDT1, LDT2, LDT3,
     $                   LWORK, NOUT, NPARMS, NSIZES, NTYPES
      REAL               TIMMIN
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * ), LLWORK( * )
      INTEGER            ISEED( * ), IWORK( * ), LDAS( * ), NN( * ),
     $                   NNB( * )
      REAL               A( * ), D( * ), E( * ), E2( * ),
     $                   OPCNTS( LDO1, LDO2, LDO3, * ),
     $                   TIMES( LDT1, LDT2, LDT3, * ), WORK( * ), Z( * )
*     ..
*
*  Purpose
*  =======
*
*     STIM22 times the LAPACK routines for the real symmetric
*     eigenvalue problem.
*
*     For each N value in NN(1:NSIZES) and .TRUE. value in
*     DOTYPE(1:NTYPES), a matrix will be generated and used to test the
*     selected routines.  Thus, NSIZES*(number of .TRUE. values in
*     DOTYPE) matrices will be generated.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          On entry, LINE contains the input line which requested
*          this routine.  This line may contain a subroutine name,
*          such as SSYTRD, indicating that only routine SSYTRD will
*          be timed, or it may contain a generic name, such as SST.
*          In this case, the rest of the line is scanned for the
*          first 19 non-blank characters, corresponding to the eight
*          combinations of subroutine and options:
*          LAPACK:
*          1: SSYTRD
*          2: SSTEQR(VECT='N')
*          3: SSTEQR(VECT='V')
*          4: SSTERF
*          5: SPTEQR(VECT='N')
*          6: SPTEQR(VECT='V')
*          7: SSTEBZ(RANGE='I')
*          8: SSTEBZ(RANGE='V')
*          9: SSTEIN
*          10: SSTEDC(COMPQ='N')
*          11: SSTEDC(COMPQ='I')
*          12: SSTEDC(COMPQ='V')
*          EISPACK:
*          13: TRED1  (compare with SSYTRD)
*          14: IMTQL1 (compare w/ SSTEQR -- VECT='N')
*          15: IMTQL2 (compare w/ SSTEQR -- VECT='V')
*          16: TQLRAT (compare with SSTERF)
*          17: TRIDIB (compare with SSTEBZ -- RANGE='I')
*          18: BISECT (compare with SSTEBZ -- RANGE='V')
*          19: TINVIT (compare with SSTEIN)
*          If a character is 'T' or 't', the corresponding routine in
*          this path is timed.  If the entire line is blank, all the
*          routines in the path are timed.
*
*  NSIZES  (input) INTEGER
*          The number of values of N contained in the vector NN.
*
*  NN      (input) INTEGER array, dimension( NSIZES )
*          The values of the matrix size N to be tested.  For each
*          N value in the array NN, and each .TRUE. value in DOTYPE,
*          a matrix A will be generated and used to test the routines.
*
*  NTYPES  (input) INTEGER
*          The number of types in DOTYPE.  Only the first MAXTYP
*          elements will be examined.  Exception: if NSIZES=1 and
*          NTYPES=MAXTYP+1, and DOTYPE=MAXTYP*f,t, then the input
*          value of A will be used.
*
*  DOTYPE  (input) LOGICAL
*          If DOTYPE(j) is .TRUE., then a matrix of type j will be
*          generated.  The matrix A has the form X**(-1) D X, where
*          X is orthogonal and D is diagonal with:
*          (j=1)  evenly spaced entries 1, ..., ULP with random signs.
*          (j=2)  geometrically spaced entries 1, ..., ULP with random
*                 signs.
*          (j=3)  "clustered" entries 1, ULP,..., ULP with random
*                 signs.
*          (j=4)  entries randomly chosen from ( ULP, 1 ).
*
*  NPARMS  (input) INTEGER
*          The number of values in each of the arrays NNB and LDAS.
*          For each matrix A generated according to NN and DOTYPE,
*          tests will be run with (NB,LDA)=
*          (NNB(1),LDAS(1)),...,(NNB(NPARMS), LDAS(NPARMS))
*
*  NNB     (input) INTEGER array, dimension( NPARMS )
*          The values of the blocksize ("NB") to be tested.
*
*  LDAS    (input) INTEGER array, dimension( NPARMS )
*          The values of LDA, the leading dimension of all matrices,
*          to be tested.
*
*  TIMMIN  (input) REAL
*          The minimum time a subroutine will be timed.
*
*  NOUT    (input) INTEGER
*          If NOUT > 0 then NOUT specifies the unit number
*          on which the output will be printed.  If NOUT <= 0, no
*          output is printed.
*
*  ISEED   (input/output) INTEGER array, dimension( 4 )
*          The random seed used by the random number generator, used
*          by the test matrix generator.  It is used and updated on
*          each call to STIM22
*
*  A       (workspace) REAL array,
*                      dimension( max(NN)*max(LDAS) )
*          The original matrix to be tested.
*
*  D       (workspace) REAL array,
*                      dimension( max(NN) )
*          The diagonal of the tridiagonal generated by SSYTRD/TRED1.
*
*  E       (workspace) REAL array,
*                      dimension( max(NN) )
*          The off-diagonal of the tridiagonal generated by
*          SSYTRD/TRED1.
*
*  E2      (workspace) REAL array,
*                      dimension( max(NN) )
*          The square of the off-diagonal of the tridiagonal generated
*          by TRED1.  (Used by TQLRAT.)
*
*  Z       (workspace) REAL array,
*                      dimension( max(NN)*max(LDAS) )
*          Various output arrays.
*
*  WORK    (workspace) REAL array, dimension( LWORK )
*
*  LWORK   (input) INTEGER
*          Number of elements in WORK.  It must be at least
*          (a)  max( (NNB + 2 )*LDAS )
*          (b)  max( 5*LDAS )
*          (c)  NSIZES*NTYPES*NPARMS
*          (d)  2*LDAS + 1 + 3*maxNN + 2*maxNN*log2(maxNN) + 3*maxNN**2
*               where maxNN = maximum matrix dimension in NN
*                     log2(x) = smallest integer power of 2 .ge. x
*
*  LLWORK  (workspace) LOGICAL array of dimension( NPARMS ),
*
*  IWORK   (workspace) INTEGER array of dimension
*          6 + 6*maxNN + 5*maxNN*log2(maxNN)
*
*  TIMES   (output) REAL array,
*                   dimension (LDT1,LDT2,LDT3,NSUBS)
*          TIMES(i,j,k,l) will be set to the run time (in seconds) for
*          subroutine l, with N=NN(k), matrix type j, and LDA=LDAS(i),
*          NBLOCK=NNB(i).
*
*  LDT1    (input) INTEGER
*          The first dimension of TIMES.  LDT1 >= min( 1, NPARMS ).
*
*  LDT2    (input) INTEGER
*          The second dimension of TIMES.  LDT2 >= min( 1, NTYPES ).
*
*  LDT3    (input) INTEGER
*          The third dimension of TIMES.  LDT3 >= min( 1, NSIZES ).
*
*  OPCNTS  (output) REAL array,
*                   dimension (LDO1,LDO2,LDO3,NSUBS)
*          OPCNTS(i,j,k,l) will be set to the number of floating-point
*          operations executed by subroutine l, with N=NN(k), matrix
*          type j, and LDA=LDAS(i), NBLOCK=NNB(i).
*
*  LDO1    (input) INTEGER
*          The first dimension of OPCNTS.  LDO1 >= min( 1, NPARMS ).
*
*  LDO2    (input) INTEGER
*          The second dimension of OPCNTS.  LDO2 >= min( 1, NTYPES ).
*
*  LDO3    (input) INTEGER
*          The third dimension of OPCNTS.  LDO3 >= min( 1, NSIZES ).
*
*  INFO    (output) INTEGER
*          Error flag.  It will be set to zero if no error occurred.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            MAXTYP, NSUBS
      PARAMETER          ( MAXTYP = 4, NSUBS = 19 )
      REAL               ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0, TWO = 2.0E+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            RUNTR1, RUNTRD
      CHARACTER          UPLO
      INTEGER            I, IC, IINFO, IL, ILWORK, IMODE, IN, IPAR,
     $                   ISUB, ITYPE, IU, J, J1, J2, J3, J4, LASTL, LDA,
     $                   LGN, LIWEDC, LWEDC, M11, MM, MMM, MTYPES, N,
     $                   NB, NSPLIT
      REAL               ABSTOL, EPS1, RLB, RUB, S1, S2, TIME, ULP,
     $                   ULPINV, UNTIME, VL, VU
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER*4        PNAMES( 4 )
      CHARACTER*9        SUBNAM( NSUBS )
      INTEGER            IDUMMA( 1 ), INPARM( NSUBS ), IOLDSD( 4 ),
     $                   KMODE( MAXTYP )
*     ..
*     .. External Functions ..
      REAL               SECOND, SLAMCH, SOPLA
      EXTERNAL           SECOND, SLAMCH, SOPLA
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMIN, BISECT, IMTQL1, IMTQL2, SCOPY, SLACPY,
     $                   SLASET, SLATMS, SPRTBE, SPTEQR, SSTEBZ, SSTEDC,
     $                   SSTEIN, SSTEQR, SSTERF, SSYTRD, TINVIT, TQLRAT,
     $                   TRED1, TRIDIB, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, INT, LOG, MAX, MIN, REAL
*     ..
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      REAL               ITCNT, OPS
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'SSYTRD', 'SSTEQR(N)', 'SSTEQR(V)',
     $                   'SSTERF', 'SPTEQR(N)', 'SPTEQR(V)',
     $                   'SSTEBZ(I)', 'SSTEBZ(V)', 'SSTEIN',
     $                   'SSTEDC(N)', 'SSTEDC(I)', 'SSTEDC(V)', 'TRED1',
     $                   'IMTQL1', 'IMTQL2', 'TQLRAT', 'TRIDIB',
     $                   'BISECT', 'TINVIT' /
      DATA               INPARM / 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     $                   1, 1, 1, 1, 1, 1 /
      DATA               PNAMES / 'LDA', 'NB', 'bad1', 'bad2' /
      DATA               KMODE / 4, 3, 1, 5 /
*     ..
*     .. Executable Statements ..
*
*
*     Extract the timing request from the input line.
*
      CALL ATIMIN( 'SST', LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   RETURN
*
*     Check that N <= LDA for the input values.
*
      DO 20 J2 = 1, NSIZES
         DO 10 J1 = 1, NPARMS
            IF( NN( J2 ).GT.LDAS( J1 ) ) THEN
               INFO = -8
               WRITE( NOUT, FMT = 9999 )LINE( 1: 6 )
 9999          FORMAT( 1X, A, ' timing run not attempted -- N > LDA',
     $               / )
               RETURN
            END IF
   10    CONTINUE
   20 CONTINUE
*
*     Check LWORK
*
      ILWORK = NSIZES*NPARMS*NTYPES
      DO 30 J1 = 1, NPARMS
         ILWORK = MAX( ILWORK, 5*LDAS( J1 ),
     $            ( NNB( J1 )+2 )*LDAS( J1 ) )
   30 CONTINUE
      IF( ILWORK.GT.LWORK ) THEN
         INFO = -18
         WRITE( NOUT, FMT = 9998 )LINE( 1: 6 )
 9998    FORMAT( 1X, A, ' timing run not attempted -- LWORK too small.',
     $         / )
         RETURN
      END IF
*
*     Check to see whether SSYTRD must be run.
*
*     RUNTRD -- if SSYTRD must be run.
*
      RUNTRD = .FALSE.
      IF( TIMSUB( 2 ) .OR. TIMSUB( 3 ) .OR. TIMSUB( 4 ) .OR.
     $    TIMSUB( 5 ) .OR. TIMSUB( 6 ) .OR. TIMSUB( 7 ) .OR.
     $    TIMSUB( 8 ) .OR. TIMSUB( 9 ) .OR. TIMSUB( 10 ) .OR.
     $    TIMSUB( 11 ) .OR. TIMSUB( 12 ) )RUNTRD = .TRUE.
*
*     Check to see whether TRED1 must be run.
*
*     RUNTR1 -- if TRED1 must be run.
*
      RUNTR1 = .FALSE.
      IF( TIMSUB( 13 ) .OR. TIMSUB( 14 ) .OR. TIMSUB( 15 ) .OR.
     $    TIMSUB( 16 ) .OR. TIMSUB( 17 ) .OR. TIMSUB( 18 ) )
     $    RUNTR1 = .TRUE.
*
*     Various Constants
*
      ULP = SLAMCH( 'Epsilon' )*SLAMCH( 'Base' )
      ULPINV = ONE / ULP
*
*     Zero out OPCNTS, TIMES
*
      DO 70 J4 = 1, NSUBS
         DO 60 J3 = 1, NSIZES
            DO 50 J2 = 1, NTYPES
               DO 40 J1 = 1, NPARMS
                  OPCNTS( J1, J2, J3, J4 ) = ZERO
                  TIMES( J1, J2, J3, J4 ) = ZERO
   40          CONTINUE
   50       CONTINUE
   60    CONTINUE
   70 CONTINUE
*
*     Do for each value of N:
*
      DO 820 IN = 1, NSIZES
*
         N = NN( IN )
         IF( N.GT.0 ) THEN
            LGN = INT( LOG( REAL( N ) ) / LOG( TWO ) )
            IF( 2**LGN.LT.N )
     $         LGN = LGN + 1
            IF( 2**LGN.LT.N )
     $         LGN = LGN + 1
            LWEDC = 1 + 4*N + 2*N*LGN + 3*N**2
            LIWEDC = 6 + 6*N + 5*N*LGN
         ELSE
            LWEDC = 8
            LIWEDC = 12
         END IF
*
*        Do for each .TRUE. value in DOTYPE:
*
         MTYPES = MIN( MAXTYP, NTYPES )
         IF( NTYPES.EQ.MAXTYP+1 .AND. NSIZES.EQ.1 )
     $      MTYPES = NTYPES
         DO 810 ITYPE = 1, MTYPES
            IF( .NOT.DOTYPE( ITYPE ) )
     $         GO TO 810
*
*           Save random number seed for error messages
*
            DO 80 J = 1, 4
               IOLDSD( J ) = ISEED( J )
   80       CONTINUE
*
*-----------------------------------------------------------------------
*
*           Time the LAPACK Routines
*
*           Generate A
*
            UPLO = 'L'
            IF( ITYPE.LE.MAXTYP ) THEN
               IMODE = KMODE( ITYPE )
               CALL SLATMS( N, N, 'S', ISEED, 'S', WORK, IMODE, ULPINV,
     $                      ONE, N, N, UPLO, A, N, WORK( N+1 ), IINFO )
            END IF
*
*           Time SSYTRD for each pair NNB(j), LDAS(j)
*
            IF( TIMSUB( 1 ) ) THEN
               DO 110 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
                  CALL XLAENV( 1, NB )
                  CALL XLAENV( 2, 2 )
                  CALL XLAENV( 3, NB )
*
*                 Time SSYTRD
*
                  IC = 0
                  OPS = ZERO
                  S1 = SECOND( )
   90             CONTINUE
                  CALL SLACPY( UPLO, N, N, A, N, Z, LDA )
                  CALL SSYTRD( UPLO, N, Z, LDA, D, E, WORK, WORK( N+1 ),
     $                         LWORK-N, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9997 )SUBNAM( 1 ), IINFO, N,
     $                  ITYPE, IPAR, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 470
                  END IF
*
                  S2 = SECOND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN )
     $               GO TO 90
*
*                 Subtract the time used in SLACPY.
*
                  S1 = SECOND( )
                  DO 100 J = 1, IC
                     CALL SLACPY( UPLO, N, N, A, N, Z, LDA )
  100             CONTINUE
                  S2 = SECOND( )
                  UNTIME = S2 - S1
*
                  TIMES( IPAR, ITYPE, IN, 1 ) = MAX( TIME-UNTIME,
     $               ZERO ) / REAL( IC )
                  OPCNTS( IPAR, ITYPE, IN, 1 ) = SOPLA( 'SSYTRD', N, 0,
     $               0, 0, NB )
  110          CONTINUE
            ELSE
               IF( RUNTRD ) THEN
                  CALL SLACPY( UPLO, N, N, A, N, Z, N )
                  CALL SSYTRD( UPLO, N, Z, N, D, E, WORK, WORK( N+1 ),
     $                         LWORK-N, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9997 )SUBNAM( 1 ), IINFO, N,
     $                  ITYPE, 0, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 470
                  END IF
               END IF
            END IF
*
*           Time SSTEQR, SSTERF, SPTEQR, SSTEBZ, SSTEIN, SSTEDC for each
*           distinct LDA=LDAS(j)
*
            IF( TIMSUB( 2 ) .OR. TIMSUB( 3 ) .OR. TIMSUB( 4 ) .OR.
     $          TIMSUB( 5 ) .OR. TIMSUB( 6 ) .OR. TIMSUB( 7 ) .OR.
     $          TIMSUB( 7 ) .OR. TIMSUB( 8 ) .OR. TIMSUB( 9 ) .OR.
     $          TIMSUB( 10 ) .OR. TIMSUB( 11 ) .OR. TIMSUB( 12 ) ) THEN
               DO 460 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 120 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  120             CONTINUE
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time SSTEQR with VECT='N'
*
                     IF( TIMSUB( 2 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = SECOND( )
  130                   CONTINUE
                        CALL SCOPY( N, D, 1, WORK, 1 )
                        CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL SSTEQR( 'N', N, WORK, WORK( LDA+1 ), Z,
     $                               LDA, WORK( 2*LDA+1 ), IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 2 ), IINFO,
     $                        N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 150
                        END IF
                        S2 = SECOND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 130
*
*                       Subtract the time used in SCOPY.
*
                        S1 = SECOND( )
                        DO 140 J = 1, IC
                           CALL SCOPY( N, D, 1, WORK, 1 )
                           CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  140                   CONTINUE
                        S2 = SECOND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 2 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / REAL( IC )
                        OPCNTS( IPAR, ITYPE, IN, 2 ) = OPS / REAL( IC )
                     END IF
*
*                    Time SSTEQR with VECT='V'
*
  150                CONTINUE
                     IF( TIMSUB( 3 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = SECOND( )
  160                   CONTINUE
                        CALL SCOPY( N, D, 1, WORK, 1 )
                        CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL SLASET( 'Full', LDA, N, ONE, TWO, Z, LDA )
                        CALL SSTEQR( 'V', N, WORK, WORK( LDA+1 ), Z,
     $                               LDA, WORK( 2*LDA+1 ), IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 3 ), IINFO,
     $                        N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 180
                        END IF
                        S2 = SECOND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 160
*
*                       Subtract the time used in SCOPY.
*
                        S1 = SECOND( )
                        DO 170 J = 1, IC
                           CALL SLASET( 'Full', LDA, N, ONE, TWO, Z,
     $                                  LDA )
                           CALL SCOPY( N, D, 1, WORK, 1 )
                           CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  170                   CONTINUE
                        S2 = SECOND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 3 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / REAL( IC )
                        OPCNTS( IPAR, ITYPE, IN, 3 ) = OPS / REAL( IC )
                     END IF
*
*                    Time SSTERF
*
  180                CONTINUE
                     IF( TIMSUB( 4 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = SECOND( )
  190                   CONTINUE
                        CALL SCOPY( N, D, 1, WORK, 1 )
                        CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL SSTERF( N, WORK, WORK( LDA+1 ), IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 4 ), IINFO,
     $                        N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 210
                        END IF
                        S2 = SECOND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 190
*
*                       Subtract the time used in SCOPY.
*
                        S1 = SECOND( )
                        DO 200 J = 1, IC
                           CALL SCOPY( N, D, 1, WORK, 1 )
                           CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  200                   CONTINUE
                        S2 = SECOND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 4 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / REAL( IC )
                        OPCNTS( IPAR, ITYPE, IN, 4 ) = OPS / REAL( IC )
                     END IF
*
*                    Time SPTEQR with VECT='N'
*
  210                CONTINUE
                     IF( TIMSUB( 5 ) ) THEN
*
*                       Modify the tridiagonal matrix to make it
*                       positive definite.
                        E2( 1 ) = ABS( D( 1 ) ) + ABS( E( 1 ) )
                        DO 220 I = 2, N - 1
                           E2( I ) = ABS( D( I ) ) + ABS( E( I ) ) +
     $                               ABS( E( I-1 ) )
  220                   CONTINUE
                        E2( N ) = ABS( D( N ) ) + ABS( E( N-1 ) )
                        IC = 0
                        OPS = ZERO
                        S1 = SECOND( )
  230                   CONTINUE
                        CALL SCOPY( N, E2, 1, WORK, 1 )
                        CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL SPTEQR( 'N', N, WORK, WORK( LDA+1 ), Z,
     $                               LDA, WORK( 2*LDA+1 ), IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 5 ), IINFO,
     $                        N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 250
                        END IF
                        S2 = SECOND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 230
*
*                       Subtract the time used in SCOPY.
*
                        S1 = SECOND( )
                        DO 240 J = 1, IC
                           CALL SCOPY( N, D, 1, WORK, 1 )
                           CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  240                   CONTINUE
                        S2 = SECOND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 5 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / REAL( IC )
                        OPCNTS( IPAR, ITYPE, IN, 5 ) = OPS / REAL( IC )
                     END IF
*
*                    Time SPTEQR with VECT='V'
*
  250                CONTINUE
                     IF( TIMSUB( 6 ) ) THEN
*
*                       Modify the tridiagonal matrix to make it
*                       positive definite.
                        E2( 1 ) = ABS( D( 1 ) ) + ABS( E( 1 ) )
                        DO 260 I = 2, N - 1
                           E2( I ) = ABS( D( I ) ) + ABS( E( I ) ) +
     $                               ABS( E( I-1 ) )
  260                   CONTINUE
                        E2( N ) = ABS( D( N ) ) + ABS( E( N-1 ) )
                        IC = 0
                        OPS = ZERO
                        S1 = SECOND( )
  270                   CONTINUE
                        CALL SCOPY( N, E2, 1, WORK, 1 )
                        CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL SPTEQR( 'V', N, WORK, WORK( LDA+1 ), Z,
     $                               LDA, WORK( 2*LDA+1 ), IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 6 ), IINFO,
     $                        N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 290
                        END IF
                        S2 = SECOND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 270
*
*                       Subtract the time used in SCOPY.
*
                        S1 = SECOND( )
                        DO 280 J = 1, IC
                           CALL SCOPY( N, D, 1, WORK, 1 )
                           CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  280                   CONTINUE
                        S2 = SECOND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 6 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / REAL( IC )
                        OPCNTS( IPAR, ITYPE, IN, 6 ) = OPS / REAL( IC )
                     END IF
*
*                    Time SSTEBZ(I)
*
  290                CONTINUE
                     IF( TIMSUB( 7 ) ) THEN
                        IL = 1
                        IU = N
                        ABSTOL = ZERO
                        IC = 0
                        OPS = ZERO
                        S1 = SECOND( )
  300                   CONTINUE
                        CALL SSTEBZ( 'I', 'B', N, VL, VU, IL, IU,
     $                               ABSTOL, D, E, MM, NSPLIT, WORK,
     $                               IWORK, IWORK( LDA+1 ),
     $                               WORK( 2*LDA+1 ), IWORK( 2*LDA+1 ),
     $                               IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 7 ), IINFO,
     $                        N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 310
                        END IF
                        S2 = SECOND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 300
                        UNTIME = ZERO
*
                        TIMES( IPAR, ITYPE, IN, 7 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / REAL( IC )
                        OPCNTS( IPAR, ITYPE, IN, 7 ) = OPS / REAL( IC )
                     END IF
*
*                    Time SSTEBZ(V)
*
  310                CONTINUE
                     IF( TIMSUB( 8 ) ) THEN
                        IF( N.EQ.1 ) THEN
                           VL = D( 1 ) - ABS( D( 1 ) )
                           VU = D( 1 ) + ABS( D( 1 ) )
                        ELSE
                           VL = D( 1 ) - ABS( E( 1 ) )
                           VU = D( 1 ) + ABS( E( 1 ) )
                           DO 320 I = 2, N - 1
                              VL = MIN( VL, D( I )-ABS( E( I ) )-
     $                             ABS( E( I-1 ) ) )
                              VU = MAX( VU, D( I )+ABS( E( I ) )+
     $                             ABS( E( I-1 ) ) )
  320                      CONTINUE
                           VL = MIN( VL, D( N )-ABS( E( N-1 ) ) )
                           VU = MAX( VU, D( N )+ABS( E( N-1 ) ) )
                        END IF
                        ABSTOL = ZERO
                        IC = 0
                        OPS = ZERO
                        S1 = SECOND( )
  330                   CONTINUE
                        CALL SSTEBZ( 'V', 'B', N, VL, VU, IL, IU,
     $                               ABSTOL, D, E, MM, NSPLIT, WORK,
     $                               IWORK, IWORK( LDA+1 ),
     $                               WORK( 2*LDA+1 ), IWORK( 2*LDA+1 ),
     $                               IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 8 ), IINFO,
     $                        N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 340
                        END IF
                        S2 = SECOND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 330
                        UNTIME = ZERO
*
                        TIMES( IPAR, ITYPE, IN, 8 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / REAL( IC )
                        OPCNTS( IPAR, ITYPE, IN, 8 ) = OPS / REAL( IC )
                     END IF
*
*                    Time SSTEIN
*
  340                CONTINUE
                     IF( TIMSUB( 9 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = SECOND( )
  350                   CONTINUE
                        CALL SSTEIN( N, D, E, MM, WORK, IWORK,
     $                               IWORK( LDA+1 ), Z, LDA,
     $                               WORK( LDA+1 ), IWORK( 2*LDA+1 ),
     $                               IWORK( 3*LDA+1 ), IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 9 ), IINFO,
     $                        N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 360
                        END IF
                        S2 = SECOND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 350
                        UNTIME = ZERO
*
                        TIMES( IPAR, ITYPE, IN, 9 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / REAL( IC )
                        OPCNTS( IPAR, ITYPE, IN, 9 ) = OPS / REAL( IC )
                     END IF
*
*                    Time SSTEDC with COMPQ='N'
*
  360                CONTINUE
                     IF( TIMSUB( 10 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = SECOND( )
  370                   CONTINUE
                        CALL SCOPY( N, D, 1, WORK, 1 )
                        CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL SSTEDC( 'N', N, WORK, WORK( LDA+1 ), Z,
     $                               LDA, WORK( 2*LDA+1 ), LWEDC, IWORK,
     $                               LIWEDC, IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 10 ),
     $                        IINFO, N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 390
                        END IF
                        S2 = SECOND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 370
*
*                       Subtract the time used in SCOPY.
*
                        S1 = SECOND( )
                        DO 380 J = 1, IC
                           CALL SCOPY( N, D, 1, WORK, 1 )
                           CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  380                   CONTINUE
                        S2 = SECOND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 10 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / REAL( IC )
                        OPCNTS( IPAR, ITYPE, IN, 10 ) = OPS / REAL( IC )
                     END IF
*
*                    Time SSTEDC with COMPQ='I'
*
  390                CONTINUE
                     IF( TIMSUB( 11 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = SECOND( )
  400                   CONTINUE
                        CALL SCOPY( N, D, 1, WORK, 1 )
                        CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL SLASET( 'Full', LDA, N, ONE, TWO, Z, LDA )
                        CALL SSTEDC( 'I', N, WORK, WORK( LDA+1 ), Z,
     $                               LDA, WORK( 2*LDA+1 ), LWEDC, IWORK,
     $                               LIWEDC, IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 11 ),
     $                        IINFO, N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 420
                        END IF
                        S2 = SECOND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 400
*
*                       Subtract the time used in SCOPY.
*
                        S1 = SECOND( )
                        DO 410 J = 1, IC
                           CALL SLASET( 'Full', LDA, N, ONE, TWO, Z,
     $                                  LDA )
                           CALL SCOPY( N, D, 1, WORK, 1 )
                           CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  410                   CONTINUE
                        S2 = SECOND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 11 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / REAL( IC )
                        OPCNTS( IPAR, ITYPE, IN, 11 ) = OPS / REAL( IC )
                     END IF
  420                CONTINUE
*
*                    Time SSTEDC with COMPQ='V'
*
                     IF( TIMSUB( 12 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = SECOND( )
  430                   CONTINUE
                        CALL SCOPY( N, D, 1, WORK, 1 )
                        CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL SLASET( 'Full', LDA, N, ONE, TWO, Z, LDA )
                        CALL SSTEDC( 'V', N, WORK, WORK( LDA+1 ), Z,
     $                               LDA, WORK( 2*LDA+1 ), LWEDC, IWORK,
     $                               LIWEDC, IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 12 ),
     $                        IINFO, N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 450
                        END IF
                        S2 = SECOND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 430
*
*                       Subtract the time used in SCOPY.
*
                        S1 = SECOND( )
                        DO 440 J = 1, IC
                           CALL SLASET( 'Full', LDA, N, ONE, TWO, Z,
     $                                  LDA )
                           CALL SCOPY( N, D, 1, WORK, 1 )
                           CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  440                   CONTINUE
                        S2 = SECOND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 12 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / REAL( IC )
                        OPCNTS( IPAR, ITYPE, IN, 12 ) = OPS / REAL( IC )
                     END IF
  450                CONTINUE
*
*
                  ELSE
                     IF( TIMSUB( 2 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 2 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 2 )
                        TIMES( IPAR, ITYPE, IN, 2 ) = TIMES( LASTL,
     $                     ITYPE, IN, 2 )
                     END IF
                     IF( TIMSUB( 3 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 3 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 3 )
                        TIMES( IPAR, ITYPE, IN, 3 ) = TIMES( LASTL,
     $                     ITYPE, IN, 3 )
                     END IF
                     IF( TIMSUB( 4 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 4 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 4 )
                        TIMES( IPAR, ITYPE, IN, 4 ) = TIMES( LASTL,
     $                     ITYPE, IN, 4 )
                     END IF
                     IF( TIMSUB( 5 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 5 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 5 )
                        TIMES( IPAR, ITYPE, IN, 5 ) = TIMES( LASTL,
     $                     ITYPE, IN, 5 )
                     END IF
                     IF( TIMSUB( 6 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 6 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 6 )
                        TIMES( IPAR, ITYPE, IN, 6 ) = TIMES( LASTL,
     $                     ITYPE, IN, 6 )
                     END IF
                     IF( TIMSUB( 7 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 7 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 7 )
                        TIMES( IPAR, ITYPE, IN, 7 ) = TIMES( LASTL,
     $                     ITYPE, IN, 7 )
                     END IF
                     IF( TIMSUB( 8 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 8 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 8 )
                        TIMES( IPAR, ITYPE, IN, 8 ) = TIMES( LASTL,
     $                     ITYPE, IN, 8 )
                     END IF
                     IF( TIMSUB( 9 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 9 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 9 )
                        TIMES( IPAR, ITYPE, IN, 9 ) = TIMES( LASTL,
     $                     ITYPE, IN, 9 )
                     END IF
                     IF( TIMSUB( 10 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 10 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 10 )
                        TIMES( IPAR, ITYPE, IN, 10 ) = TIMES( LASTL,
     $                     ITYPE, IN, 10 )
                     END IF
                     IF( TIMSUB( 11 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 11 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 11 )
                        TIMES( IPAR, ITYPE, IN, 11 ) = TIMES( LASTL,
     $                     ITYPE, IN, 11 )
                     END IF
                     IF( TIMSUB( 12 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 12 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 12 )
                        TIMES( IPAR, ITYPE, IN, 12 ) = TIMES( LASTL,
     $                     ITYPE, IN, 12 )
                     END IF
                  END IF
  460          CONTINUE
            END IF
  470       CONTINUE
*
*-----------------------------------------------------------------------
*
*           Time the EISPACK Routines
*
*           Skip routines if N <= 0 (EISPACK requirement)
*
            IF( N.LE.0 )
     $         GO TO 810
*
*           Time TRED1 for each LDAS(j)
*
            IF( TIMSUB( 13 ) ) THEN
               DO 510 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 480 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  480             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time TRED1
*
                     IC = 0
                     OPS = ZERO
                     S1 = SECOND( )
  490                CONTINUE
                     CALL SLACPY( 'L', N, N, A, N, Z, LDA )
                     CALL TRED1( LDA, N, Z, D, E, E2 )
                     S2 = SECOND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 490
*
*                    Subtract the time used in SLACPY.
*
                     S1 = SECOND( )
                     DO 500 J = 1, IC
                        CALL SLACPY( 'L', N, N, A, N, Z, LDA )
  500                CONTINUE
                     S2 = SECOND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 13 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / REAL( IC )
                     OPCNTS( IPAR, ITYPE, IN, 13 ) = OPS / REAL( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 13 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 13 )
                     TIMES( IPAR, ITYPE, IN, 13 ) = TIMES( LASTL, ITYPE,
     $                  IN, 13 )
                  END IF
  510          CONTINUE
            ELSE
               IF( RUNTR1 ) THEN
                  CALL SLACPY( 'L', N, N, A, N, Z, LDA )
                  CALL TRED1( LDA, N, Z, D, E, E2 )
               END IF
            END IF
*
*           Time IMTQL1 for each LDAS(j)
*
            IF( TIMSUB( 14 ) ) THEN
               DO 550 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 520 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  520             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time IMTQL1
*
                     IC = 0
                     OPS = ZERO
                     S1 = SECOND( )
  530                CONTINUE
                     CALL SCOPY( N, D, 1, WORK, 1 )
                     CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                     CALL IMTQL1( N, WORK, WORK( LDA+1 ), IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 14 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 560
                     END IF
                     S2 = SECOND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 530
*
*                    Subtract the time used in SCOPY.
*
                     S1 = SECOND( )
                     DO 540 J = 1, IC
                        CALL SCOPY( N, D, 1, WORK, 1 )
                        CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  540                CONTINUE
                     S2 = SECOND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 14 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / REAL( IC )
                     OPCNTS( IPAR, ITYPE, IN, 14 ) = OPS / REAL( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 14 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 14 )
                     TIMES( IPAR, ITYPE, IN, 14 ) = TIMES( LASTL, ITYPE,
     $                  IN, 14 )
                  END IF
  550          CONTINUE
            END IF
*
*           Time IMTQL2 for each LDAS(j)
*
  560       CONTINUE
            IF( TIMSUB( 15 ) ) THEN
               DO 600 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 570 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  570             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time IMTQL2
*
                     IC = 0
                     OPS = ZERO
                     S1 = SECOND( )
  580                CONTINUE
                     CALL SCOPY( N, D, 1, WORK, 1 )
                     CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                     CALL SLASET( 'Full', N, N, ONE, TWO, Z, LDA )
                     CALL IMTQL2( LDA, N, WORK, WORK( LDA+1 ), Z,
     $                            IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 15 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 610
                     END IF
                     S2 = SECOND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 580
*
*                    Subtract the time used in SCOPY.
*
                     S1 = SECOND( )
                     DO 590 J = 1, IC
                        CALL SCOPY( N, D, 1, WORK, 1 )
                        CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL SLASET( 'Full', N, N, ONE, TWO, Z, LDA )
  590                CONTINUE
                     S2 = SECOND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 15 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / REAL( IC )
                     OPCNTS( IPAR, ITYPE, IN, 15 ) = OPS / REAL( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 15 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 15 )
                     TIMES( IPAR, ITYPE, IN, 15 ) = TIMES( LASTL, ITYPE,
     $                  IN, 15 )
                  END IF
  600          CONTINUE
            END IF
*
*           Time TQLRAT for each LDAS(j)
*
  610       CONTINUE
            IF( TIMSUB( 16 ) ) THEN
               DO 650 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 620 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  620             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time TQLRAT
*
                     IC = 0
                     OPS = ZERO
                     S1 = SECOND( )
  630                CONTINUE
                     CALL SCOPY( N, D, 1, WORK, 1 )
                     CALL SCOPY( N-1, E2, 1, WORK( LDA+1 ), 1 )
                     CALL TQLRAT( N, WORK, WORK( LDA+1 ), IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 16 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 660
                     END IF
                     S2 = SECOND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 630
*
*                    Subtract the time used in SCOPY.
*
                     S1 = SECOND( )
                     DO 640 J = 1, IC
                        CALL SCOPY( N, D, 1, WORK, 1 )
                        CALL SCOPY( N-1, E2, 1, WORK( LDA+1 ), 1 )
  640                CONTINUE
                     S2 = SECOND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 16 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / REAL( IC )
                     OPCNTS( IPAR, ITYPE, IN, 16 ) = OPS / REAL( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 16 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 16 )
                     TIMES( IPAR, ITYPE, IN, 16 ) = TIMES( LASTL, ITYPE,
     $                  IN, 16 )
                  END IF
  650          CONTINUE
            END IF
*
*           Time TRIDIB for each LDAS(j)
*
  660       CONTINUE
            IF( TIMSUB( 17 ) ) THEN
               DO 700 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 670 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  670             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time TRIDIB
*
                     IC = 0
                     OPS = ZERO
                     EPS1 = ZERO
                     RLB = ZERO
                     RUB = ZERO
                     M11 = 1
                     MM = N
                     S1 = SECOND( )
  680                CONTINUE
                     CALL SCOPY( N, D, 1, WORK, 1 )
                     CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                     CALL SCOPY( N-1, E2, 1, WORK( 2*LDA+1 ), 1 )
                     CALL TRIDIB( N, EPS1, WORK( 1 ), WORK( LDA+1 ),
     $                            WORK( 2*LDA+1 ), RLB, RUB, M11, MM,
     $                            WORK( 3*LDA+1 ), IWORK, IINFO,
     $                            WORK( 4*LDA+1 ), WORK( 5*LDA+1 ) )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 17 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 710
                     END IF
                     S2 = SECOND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 680
*
*                    Subtract the time used in SCOPY.
*
                     S1 = SECOND( )
                     DO 690 J = 1, IC
                        CALL SCOPY( N, D, 1, WORK, 1 )
                        CALL SCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL SCOPY( N-1, E2, 1, WORK( 2*LDA+1 ), 1 )
  690                CONTINUE
                     S2 = SECOND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 17 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / REAL( IC )
                     OPCNTS( IPAR, ITYPE, IN, 17 ) = OPS / REAL( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 17 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 17 )
                     TIMES( IPAR, ITYPE, IN, 17 ) = TIMES( LASTL, ITYPE,
     $                  IN, 17 )
                  END IF
  700          CONTINUE
            END IF
*
*           Time BISECT for each LDAS(j)
*
  710       CONTINUE
            IF( TIMSUB( 18 ) ) THEN
               DO 760 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 720 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  720             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time BISECT
*
                     VL = D( 1 ) - ABS( E( 2 ) )
                     VU = D( 1 ) + ABS( E( 2 ) )
                     DO 730 I = 2, N - 1
                        VL = MIN( VL, D( I )-ABS( E( I+1 ) )-
     $                       ABS( E( I ) ) )
                        VU = MAX( VU, D( I )+ABS( E( I+1 ) )+
     $                       ABS( E( I ) ) )
  730                CONTINUE
                     VL = MIN( VL, D( N )-ABS( E( N ) ) )
                     VU = MAX( VU, D( N )+ABS( E( N ) ) )
                     IC = 0
                     OPS = ZERO
                     EPS1 = ZERO
                     MM = N
                     MMM = 0
                     S1 = SECOND( )
  740                CONTINUE
                     CALL SCOPY( N, D, 1, WORK, 1 )
                     CALL SCOPY( N, E, 1, WORK( LDA+1 ), 1 )
                     CALL SCOPY( N, E2, 1, WORK( 2*LDA+1 ), 1 )
                     CALL BISECT( N, EPS1, WORK( 1 ), WORK( LDA+1 ),
     $                            WORK( 2*LDA+1 ), VL, VU, MM, MMM,
     $                            WORK( 3*LDA+1 ), IWORK, IINFO,
     $                            WORK( 4*LDA+1 ), WORK( 5*LDA+1 ) )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 18 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 770
                     END IF
                     S2 = SECOND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 740
*
*                    Subtract the time used in SCOPY.
*
                     S1 = SECOND( )
                     DO 750 J = 1, IC
                        CALL SCOPY( N, D, 1, WORK, 1 )
                        CALL SCOPY( N, E, 1, WORK( LDA+1 ), 1 )
                        CALL SCOPY( N, E2, 1, WORK( 2*LDA+1 ), 1 )
  750                CONTINUE
                     S2 = SECOND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 18 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / REAL( IC )
                     OPCNTS( IPAR, ITYPE, IN, 18 ) = OPS / REAL( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 18 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 18 )
                     TIMES( IPAR, ITYPE, IN, 18 ) = TIMES( LASTL, ITYPE,
     $                  IN, 18 )
                  END IF
  760          CONTINUE
            END IF
*
*           Time TINVIT for each LDAS(j)
*
  770       CONTINUE
            IF( TIMSUB( 19 ) ) THEN
               CALL SCOPY( N, WORK( 3*LDA+1 ), 1, WORK( 1 ), 1 )
               DO 800 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 780 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  780             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time TINVIT
*
                     IC = 0
                     OPS = ZERO
                     S1 = SECOND( )
  790                CONTINUE
                     CALL TINVIT( LDA, N, D, E, E2, MMM, WORK, IWORK, Z,
     $                            IINFO, WORK( LDA+1 ), WORK( 2*LDA+1 ),
     $                            WORK( 3*LDA+1 ), WORK( 4*LDA+1 ),
     $                            WORK( 5*LDA+1 ) )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 19 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 810
                     END IF
                     S2 = SECOND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 790
                     UNTIME = ZERO
*
                     TIMES( IPAR, ITYPE, IN, 19 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / REAL( IC )
                     OPCNTS( IPAR, ITYPE, IN, 19 ) = OPS / REAL( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 19 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 19 )
                     TIMES( IPAR, ITYPE, IN, 19 ) = TIMES( LASTL, ITYPE,
     $                  IN, 19 )
                  END IF
  800          CONTINUE
            END IF
*
  810    CONTINUE
  820 CONTINUE
*
*-----------------------------------------------------------------------
*
*     Print a table of results for each timed routine.
*
      DO 830 ISUB = 1, NSUBS
         IF( TIMSUB( ISUB ) ) THEN
            CALL SPRTBE( SUBNAM( ISUB ), MTYPES, DOTYPE, NSIZES, NN,
     $                   INPARM( ISUB ), PNAMES, NPARMS, LDAS, NNB,
     $                   IDUMMA, IDUMMA, OPCNTS( 1, 1, 1, ISUB ), LDO1,
     $                   LDO2, TIMES( 1, 1, 1, ISUB ), LDT1, LDT2, WORK,
     $                   LLWORK, NOUT )
         END IF
  830 CONTINUE
*
 9997 FORMAT( ' STIM22: ', A, ' returned INFO=', I6, '.', / 9X, 'N=',
     $      I6, ', ITYPE=', I6, ', IPAR=', I6, ', ISEED=(',
     $      3( I5, ',' ), I5, ')' )
*
      RETURN
*
*     End of STIM22
*
      END
