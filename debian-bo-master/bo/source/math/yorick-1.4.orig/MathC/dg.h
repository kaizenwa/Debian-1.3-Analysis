/*
   DG.H
   Headers for LAPACK and BLAS routines in this directory.

   $Id$
 */
#ifndef DG_H_LAPACK
#define DG_H_LAPACK

/*--- dgtsv.c (tridiagonal solver) ---*/
extern void dgtsv( long n, long nrhs, double dl[], double d[], double du[],
		  double b[], long ldb, long *info );

/*--- dgesv.c (LU decomposition solver) ---*/
extern void dgesv( long n, long nrhs, double a[], long lda, long ipiv[],
		  double b[], long ldb, long *info );
extern void dgetf2( long m, long n, double a[], long lda, long ipiv[],
		   long *info );
extern void dgetrf( long m, long n, double a[], long lda, long ipiv[],
		   long *info );
extern void dgetrs( char trans, long n, long nrhs, double a[], long lda,
		   long ipiv[], double b[], long ldb, long *info );
extern void dlaswp( long n, double a[], long lda, long k1, long k2,
		   long ipiv[], long incx );
extern long ilaenv( long ispec, char *name, char *opts,
		   long n1, long n2, long n3,long n4 );

/*--- dgecon.c (estimate condition number) ---*/
extern void dgecon( char norm, long n, double a[], long lda, double anorm,
		   double *rcond, double work[], long iwork[],long *info );
extern void dlacon( long n, double v[], double x[], long isgn[],
		   double *est, long *kase );
extern void dlatrs( char uplo, char trans, char diag, char normin, long n,
		   double a[], long lda, double x[], double *scale,
		   double cnorm[], long *info );
extern void drscl( long n, double sa, double sx[], long incx );

extern void dlabad( double *small, double *large );
extern double   dlamch( char cmach );
extern void dlamc1( long *beta, long *t, int *rnd, int *ieee1 );
extern void dlamc2( long *beta, long *t, int *rnd, double *eps,
		   long *emin, double *rmin, long *emax, double *rmax );
extern double   dlamc3( double a, double b );
extern void dlamc4( long *emin, double start, long base );
extern void dlamc5( long beta, long p, long emin, int ieee,
		   long *emax, double *rmax );

/*--- dgels.c (QR/LQ solver - least squares sense) ---*/
extern void dgels( char trans, long m, long n, long nrhs,
		  double a[], long lda, double b[], long ldb,
		  double work[], long lwork,long *info );
extern void dormqr( char side, char trans, long m, long n, long k,
		   double a[], long lda, double tau[], double c[], long ldc,
		   double work[], long lwork, long *info );
extern void dorm2r( char side, char trans, long m, long n, long k,
		   double a[], long lda, double tau[],
		   double c[], long ldc,double work[], long *info );
extern void dormlq( char side, char trans, long m, long n, long k,
		   double a[], long lda, double tau[], double c[], long ldc,
		   double work[], long lwork, long *info );
extern void dorml2( char side, char trans, long m, long n, long k,
		   double a[], long lda, double tau[], double c[], long ldc,
		   double work[], long *info );
extern void dgeqrf( long m, long n, double a[], long lda, double tau[],
		   double work[], long lwork, long *info );
extern void dgeqr2( long m, long n, double a[], long lda, double tau[],
		   double work[], long *info );
extern void dgelqf( long m, long n, double a[], long lda, double tau[],
		   double work[], long lwork, long *info );
extern void dgelq2( long m, long n, double a[], long lda, double tau[],
		   double work[], long *info );
extern void dlarf( char side, long m, long n, double v[], long incv,
		  double tau, double c[], long ldc, double work[] );
extern void dlarfg( long n, double *alpha, double x[], long incx,
		   double *tau );
extern void dlarft( char direct, char storev, long n, long k,
		   double v[], long ldv, double tau[], double t[], long ldt );
extern void dlarfb( char side, char trans, char direct, char storev,
		   long m, long n, long k, double v[], long ldv,
		   double t[], long ldt, double c[], long ldc,
		   double work[], long ldwork );

extern double   dlapy2( double x, double y );
extern double   dlange( char norm, long m, long n, double a[], long lda,
		       double work[] );
extern void dlascl( char type, long kl, long ku, double cfrom, double cto,
		   long m, long n, double a[], long lda, long *info );
extern void dlaset( char uplo, long m, long n, double alpha, double beta,
		   double a[], long lda );
extern void dlassq( long n, double x[], long incx,
		   double *scale, double *sumsq );

/*--- dgelss.c (SVD solver - least squares sense) ---*/
extern void dlabrd( long m, long n, long nb, double a[], long lda,
		   double d[], double e[], double tauq[], double taup[],
		   double x[], long ldx, double y[],long ldy );
extern void dgebd2( long m, long n, double a[], long lda,
		   double d[], double e[], double tauq[], double taup[],
		   double work[], long *info );
extern void dgebrd( long m, long n, double a[], long lda,
		   double d[], double e[], double tauq[], double taup[],
		   double work[], long lwork,long *info );
extern void dormbr( char vect, char side, char trans, long m, long n, long k,
		   double a[], long lda, double tau[], double c[],long ldc,
		   double work[], long lwork, long *info );
extern void dorg2r( long m, long n, long k, double a[], long lda,
		   double tau[], double work[], long *info );
extern void dorgqr( long m, long n, long k, double a[], long lda,
		   double tau[], double work[], long lwork, long *info );
extern void dorgl2( long m, long n, long k, double a[], long lda,
		   double tau[], double work[], long *info );
extern void dorglq( long m, long n, long k, double a[], long lda,
		   double tau[], double work[], long lwork, long *info );
extern void dorgbr( char vect, long m, long n, long k, double a[], long lda,
		   double tau[], double work[], long lwork, long *info );
extern void dlartg( double f, double g, double *cs, double *sn, double *r );
extern void dlasr( char side, char pivot, char direct, long m, long n,
		  double c[], double s[], double a[], long lda );
extern void dlasv2( double f, double g, double h,
		   double *ssmin, double *ssmax, double *snr,
		   double *csr, double *snl, double *csl );
extern void dlas2( double f, double g, double h,
		  double *ssmin, double *ssmax );
extern void dbdsqr( char uplo, long n, long ncvt, long nru, long ncc,
		   double d[], double e[], double vt[], long ldvt,
		   double u[],long ldu, double c[], long ldc,
		   double work[], long *info );
extern void dlacpy( char uplo, long m, long n, double a[], long lda,
		   double b[], long ldb );
extern void dgelss( long m, long n, long nrhs, double a[], long lda,
		   double b[], long ldb, double s[], double rcond,
		   long *rank,double work[], long lwork, long *info );

/*--- dgesvd.c (returns SVD) ---*/
extern void dgesvd( char jobu, char jobvt, long m, long n,
		   double a[], long lda, double s[], double u[], long ldu,
		   double vt[], long ldvt,
		   double work[], long lwork, long *info );

/*--- LAPACK error routine (see dgyor.c) ---*/
extern void xerbla( char *srname, long info );

/*--- dgblas.c (BLAS routines) ---*/
extern long  idamax(long n,double dx[],long incx);

extern void  dcopy(long n,double dx[],long incx,double dy[],long incy);
extern void  dswap (long n,double dx[],long incx,double dy[],long incy);
extern double   dasum(long n,double dx[],long incx);
extern double   dnrm2 ( long n, double dx[], long incx);
extern double   ddot(long n,double dx[],long incx,double dy[],long incy);
extern void  dscal(long n,double da,double dx[],long incx);
extern void daxpy(long n,double da,double dx[],long incx,
		  double dy[],long incy);
extern void  drot (long n,double dx[],long incx,double dy[],long incy,
		   double c,double s);
extern void dger  ( long m, long n, double alpha, double x[], long incx,
		   double y[], long incy, double a[], long lda );
extern void dgemv ( char trans, long m, long n, double alpha,
		   double a[], long lda, double x[], long incx,
		   double beta, double y[], long incy );
extern void dgemm ( char transa, char transb, long m, long n, long k,
		   double alpha, double a[], long lda, double b[], long ldb,
		   double beta, double c[], long ldc );
extern void dtrsv ( char uplo, char trans, char diag, long n,
		   double a[], long lda, double x[], long incx );
extern void dtrsm ( char side, char uplo, char transa, char diag,
		   long m, long n, double alpha, double a[], long lda,
		   double b[], long ldb );
extern void dtrmv ( char uplo, char trans, char diag, long n,
		   double a[], long lda, double x[], long incx );
extern void dtrmm ( char side, char uplo, char transa, char diag,
		   long m, long n, double alpha, double a[], long lda,
		   double b[], long ldb );

#endif
