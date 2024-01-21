/*
   DGBLAS.C
   BLAS routines used by LAPACK matrix solvers.

   $Id$
 */

#include "dg.h"

/* 24/May/96

   Profiling studies make it clear that most of the time in the
   linear algebra routines is actually spent here, so improving
   performance means improving these BLAS routines.  The level 2
   and 3 routines are particularly critical.

   Therefore, I have made a pass through this file (1) converting
   the automatically translated code into idiomatic C language code,
   and (2) hand unrolling the inner loops of the level 2 and 3 routines.
   Number (2) is necessary since few C compilers seem to perform that
   operation, even when instructed to unroll loops (C optimizers are
   notoriously conservative).  The level 1 routines were mostly
   hand unrolled anyway -- presumeably fossils of a time when Fortran
   compilers were equally unsophistocated.
 */


/*-----prototypes of functions defined here-----*/
extern long  idamax(long n,double dx[],long incx);
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
extern void  dswap (long n,double dx[],long incx,double dy[],long incy);
extern void  dscal(long n,double da,double dx[],long incx);
extern void  drot (long n,double dx[],long incx,double dy[],long incy,
		   double c,double s);
extern double   dnrm2 ( long n, double dx[], long incx);
extern void dger  ( long m, long n, double alpha, double x[], long incx,
		   double y[], long incy, double a[], long lda );
extern void dgemv ( char trans, long m, long n, double alpha,
		   double a[], long lda, double x[], long incx,
		   double beta, double y[], long incy );
extern void dgemm ( char transa, char transb, long m, long n, long k,
		   double alpha, double a[], long lda, double b[], long ldb,
		   double beta, double c[], long ldc );
extern double   ddot(long n,double dx[],long incx,double dy[],long incy);
extern void  dcopy(long n,double dx[],long incx,double dy[],long incy);
extern void daxpy(long n,double da,double dx[],long incx,
		  double dy[],long incy);
extern double   dasum(long n,double dx[],long incx);
/*-----end of prototypes-----*/



/*---converted nutty string switches to single characters (lower case)---*/
#define lsame(x,y) ((x)==(y))



/*-----Fortran intrinsics converted-----*/
#define abs(x) ((x)>=0?(x):-(x))
#define dabs(x) ((double)((x)>=0?(x):-(x)))
extern double sqrt(double);
#define max(x,y) ((x)>(y)?(x):(y))
/*-----end of Fortran intrinsics-----*/



double   dasum(long n,double dx[],long incx)
{
  /*c
    c     takes the sum of the absolute values.
    c     jack dongarra, linpack, 3/11/78.
    c     modified 3/93 to return if incx .le. 0.
    c*/
  double  dtemp= 0.0;
  long i;
  if ( n<=0 || incx<=0 ) return dtemp;
  if (incx!=1) {
    /*        increment not equal to 1 */
    for (i=0 ; n-- ; i+=incx) dtemp+= dabs(dx[i]);
  } else {
    /*        increment equal to 1 */
    /* unroll loop */
    long m= n%6;
    for (i=0 ; i<m ; i++) dtemp+= dabs(dx[i]);
    for (    ; i<n ; i+=6)
      dtemp+= dabs(dx[i]) + dabs(dx[i+1]) + dabs(dx[i+2]) +
	    dabs(dx[i+3]) + dabs(dx[i+4]) + dabs(dx[i+5]);
  }
  return dtemp;
}



void daxpy(long n,double da,double dx[],long incx,double dy[],long incy)
{
  /*c
    c     constant times a vector plus a vector.
    c     uses unrolled loops for increments equal to one.
    c     jack dongarra, linpack, 3/11/78.
    c*/
  long i;
  if (n<=0) return;
  if (da == 0.0) return;
  if (incx!=1 || incy!=1) {
    /*        unequal increments or equal increments not equal to 1 */
    long iy;
    if (incx<0) dx-= (n-1)*incx;
    if (incy<0) dy-= (n-1)*incy;
    for (i=iy=0 ; n-- ; i+=incx,iy+=incy) dy[iy]+= da*dx[i];
  } else {
    /*        both increments equal to 1 */
    /* unroll loop */
    long m= n%4;
    for (i=0 ; i<m ; i++) dy[i]+= da*dx[i];
    for (    ; i<n ; i+=4) {
      dy[i  ]+= da*dx[i];
      dy[i+1]+= da*dx[i+1];
      dy[i+2]+= da*dx[i+2];
      dy[i+3]+= da*dx[i+3];
    }
  }
}



void  dcopy(long n,double dx[],long incx,double dy[],long incy)
{
  /*c
    c     copies a vector, x, to a vector, y.
    c     uses unrolled loops for increments equal to one.
    c     jack dongarra, linpack, 3/11/78.
    c*/
  long i;
  if (n<=0) return;
  if (incx!=1 || incy!=1) {
  /*        unequal increments or equal increments not equal to 1 */
    long iy;
    if (incx<0) dx-= (n-1)*incx;
    if (incy<0) dy-= (n-1)*incx;
    for (i=iy=0 ; n-- ; i+=incx,iy+=incy) dy[iy]= dx[i];
  } else {
    /*        both increments equal to 1 */
    /* unroll loop */
    long m= n%7;
    for (i=0 ; i<m ; i++) dy[i]= dx[i];
    for (    ; i<n ; i+=7) {
      dy[i  ]= dx[i];
      dy[i+1]= dx[i+1];
      dy[i+2]= dx[i+2];
      dy[i+3]= dx[i+3];
      dy[i+4]= dx[i+4];
      dy[i+5]= dx[i+5];
      dy[i+6]= dx[i+6];
    }
  }
}



double   ddot(long n,double dx[],long incx,double dy[],long incy)
{
  /*c
    c     forms the dot product of two vectors.
    c     uses unrolled loops for increments equal to one.
    c     jack dongarra, linpack, 3/11/78.
    c*/
  double  dtemp= 0.0;
  long i;
  if (n<=0) return dtemp;
  if (incx!=1 || incy!=1) {
    /*        unequal increments or equal increments not equal to 1 */
    long iy;
    if (incx<0) dx-= (n-1)*incx;
    if (incy<0) dy-= (n-1)*incx;
    for (i=iy=0 ; n-- ; i+=incx,iy+=incy) dtemp+= dy[iy]*dx[i];
  } else {
    /*        both increments equal to 1 */
    /* unroll loop */
    long m= n%5;
    for (i=0 ; i<m ; i++) dtemp+= dy[i]*dx[i];
    for (    ; i<n ; i+=5)
      dtemp+= dy[i]*dx[i] + dy[i+1]*dx[i+1] + dy[i+2]*dx[i+2] +
	dy[i+3]*dx[i+3] + dy[i+4]*dx[i+4];
  }
  return dtemp;
}



void dgemm ( char transa, char transb, long m, long n, long k,
	    double alpha, double a[], long lda, double b[], long ldb,
	    double beta, double c[], long ldc )
{
  /**
   *  Purpose
   *  =======
   *
   *  DGEMM  performs one of the matrix-matrix operations
   *
   *     C := alpha*op( A )*op( B ) + beta*C,
   *
   *  where  op( X ) is one of
   *
   *     op( X ) = X   or   op( X ) = X',
   *
   *  alpha and beta are scalars, and A, B and C are matrices, with op( A )
   *  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
   *
   *  Parameters
   *  ==========
   *
   *  TRANSA - CHARACTER*1.
   *           On entry, TRANSA specifies the form of op( A ) to be used in
   *           the matrix multiplication as follows:
   *
   *              TRANSA = 'N' or 'n',  op( A ) = A.
   *
   *              TRANSA = 'T' or 't',  op( A ) = A'.
   *
   *              TRANSA = 'C' or 'c',  op( A ) = A'.
   *
   *           Unchanged on exit.
   *
   *  TRANSB - CHARACTER*1.
   *           On entry, TRANSB specifies the form of op( B ) to be used in
   *           the matrix multiplication as follows:
   *
   *              TRANSB = 'N' or 'n',  op( B ) = B.
   *
   *              TRANSB = 'T' or 't',  op( B ) = B'.
   *
   *              TRANSB = 'C' or 'c',  op( B ) = B'.
   *
   *           Unchanged on exit.
   *
   *  M      - INTEGER.
   *           On entry,  M  specifies  the number  of rows  of the  matrix
   *           op( A )  and of the  matrix  C.  M  must  be at least  zero.
   *           Unchanged on exit.
   *
   *  N      - INTEGER.
   *           On entry,  N  specifies the number  of columns of the matrix
   *           op( B ) and the number of columns of the matrix C. N must be
   *           at least zero.
   *           Unchanged on exit.
   *
   *  K      - INTEGER.
   *           On entry,  K  specifies  the number of columns of the matrix
   *           op( A ) and the number of rows of the matrix op( B ). K must
   *           be at least  zero.
   *           Unchanged on exit.
   *
   *  ALPHA  - DOUBLE PRECISION.
   *           On entry, ALPHA specifies the scalar alpha.
   *           Unchanged on exit.
   *
   *  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
   *           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
   *           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
   *           part of the array  A  must contain the matrix  A,  otherwise
   *           the leading  k by m  part of the array  A  must contain  the
   *           matrix A.
   *           Unchanged on exit.
   *
   *  LDA    - INTEGER.
   *           On entry, LDA specifies the first dimension of A as declared
   *           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
   *           LDA must be at least  max( 1, m ), otherwise  LDA must be at
   *           least  max( 1, k ).
   *           Unchanged on exit.
   *
   *  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
   *           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
   *           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
   *           part of the array  B  must contain the matrix  B,  otherwise
   *           the leading  n by k  part of the array  B  must contain  the
   *           matrix B.
   *           Unchanged on exit.
   *
   *  LDB    - INTEGER.
   *           On entry, LDB specifies the first dimension of B as declared
   *           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
   *           LDB must be at least  max( 1, k ), otherwise  LDB must be at
   *           least  max( 1, n ).
   *           Unchanged on exit.
   *
   *  BETA   - DOUBLE PRECISION.
   *           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
   *           supplied as zero then C need not be set on input.
   *           Unchanged on exit.
   *
   *  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
   *           Before entry, the leading  m by n  part of the array  C must
   *           contain the matrix  C,  except when  beta  is zero, in which
   *           case C need not be set on entry.
   *           On exit, the array  C  is overwritten by the  m by n  matrix
   *           ( alpha*op( A )*op( B ) + beta*C ).
   *
   *  LDC    - INTEGER.
   *           On entry, LDC specifies the first dimension of C as declared
   *           in  the  calling  (sub)  program.   LDC  must  be  at  least
   *           max( 1, m ).
   *           Unchanged on exit.
   **/
  /**
   *  Level 3 Blas routine.
   *
   *  -- Written on 8-February-1989.
   *     Jack Dongarra, Argonne National Laboratory.
   *     Iain Duff, AERE Harwell.
   *     Jeremy Du Croz, Numerical Algorithms Group Ltd.
   *     Sven Hammarling, Numerical Algorithms Group Ltd.
   **/
  extern void xerbla(char *,long);
  int            nota, notb;
  long            i, info, j, l, ncola, nrowa, nrowb;
  double    temp;
  /**
   *     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
   *     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
   *     and  columns of  A  and the  number of  rows  of  B  respectively.
   **/
  nota  = lsame( transa, 'n' );
  notb  = lsame( transb, 'n' );
  if( nota ){
    nrowa = m;
    ncola = k;
  } else {
    nrowa = k;
    ncola = m;
  }
  if( notb ){
    nrowb = k;
  } else {
    nrowb = n;
  }
  /**
   *     Test the input parameters.
   **/
  info = 0;
  if(      ( !nota                 )&&
     ( !lsame( transa, 'c' ) )&&
     ( !lsame( transa, 't' ) )      ){
    info = 1;
  } else if( ( !notb                 )&&
	    ( !lsame( transb, 'c' ) )&&
	    ( !lsame( transb, 't' ) )      ){
    info = 2;
  } else if( m  <0               ){
    info = 3;
  } else if( n  <0               ){
    info = 4;
  } else if( k  <0               ){
    info = 5;
  } else if( lda<max( 1, nrowa ) ){
    info = 8;
  } else if( ldb<max( 1, nrowb ) ){
    info = 10;
  } else if( ldc<max( 1, m     ) ){
    info = 13;
  }
  if( info!=0 ){
    xerbla( "dgemm ", info );
    return;
  }
  /**
   *     Quick return if possible.
   **/
  if( ( m==0 )||( n==0 )||
     ( ( ( alpha==0.0 )||( k==0 ) )&&( beta==1.0 ) ) )
    return;
  /**
   *     And if  alpha.eq.0.0.
   **/
  if( alpha==0.0 ){
    if( beta==0.0 ){
      for ( ; n-- ; c+=ldc)
	for (i=0 ; i<m ; i++) c[i]= 0.0;
    } else {
      for ( ; n-- ; c+=ldc)
	for (i=0 ; i<m ; i++) c[i]*= beta;
    }
    return;
  }
  /**
   *     Start the operations.
   **/
  if( notb ){
    if( nota ){
      /**
       *           Form  C := alpha*A*B + beta*C.
       **/
      double *al;
      long m3= m&3;
      for ( ; n-- ; b+=ldb,c+=ldc) {
	if( beta==0.0 ){
	  for (i=0 ; i<m ; i++) c[i]= 0.0;
	} else if( beta!=1.0 ){
	  for (i=0 ; i<m ; i++) c[i]*= beta;
	}
	for (l=0,al=a ; l<k ; l++,al+=lda) {
	  if ( b[l]!=0.0 ) {
	    temp= alpha*b[l];
	    /* unroll innermost loop */
	    for (i=0 ; i<m3 ; i++) c[i]+= temp*al[i];
	    for (    ; i<m  ; i+=4) {
	      c[i]+= temp*al[i];
	      c[i+1]+= temp*al[i+1];
	      c[i+2]+= temp*al[i+2];
	      c[i+3]+= temp*al[i+3];
	    }
	  }
	}
      }
    } else {
      /**
       *           Form  C := alpha*A'*B + beta*C
       **/
      double *ai;
      long k3= k&3;
      for ( ; n-- ; b+=ldb,c+=ldc) {
	for (i=0,ai=a ; i<m ; i++,ai+=lda) {
	  temp= 0.0;
	  /* unroll innermost loop */
	  for (l=0 ; l<k3 ; l++) temp+= ai[l]*b[l];
	  for (    ; l<k  ; l+=4)
	    temp+= ai[l]*b[l] + ai[l+1]*b[l+1] +
	      ai[l+2]*b[l+2] + ai[l+3]*b[l+3];
	  if( beta==0.0 ) c[i]= alpha*temp;
	  else c[i]= alpha*temp + beta*c[i];
	}
      }
    }
  } else {
    if( nota ){
      /**
       *           Form  C := alpha*A*B' + beta*C
       **/
      double *al, *bl;
      long m3= m&3;
      for (j=0 ; j<n ; j++,c+=ldc) {
	if( beta==0.0 ) for (i=0 ; i<m ; i++) c[i]= 0.0;
	else if( beta!=1.0 ) for (i=0 ; i<m ; i++) c[i]*= beta;
	for (l=0,al=a,bl=b ; l<k ; l++,al+=lda,bl+=ldb) {
	  if( bl[j]!=0.0 ){
	    temp = alpha*bl[j];
	    /* unroll innermost loop */
	    for (i=0 ; i<m3 ; i++) c[i]+= temp*al[i];
	    for (    ; i<m  ; i+=4) {
	      c[i]+= temp*al[i];
	      c[i+1]+= temp*al[i+1];
	      c[i+2]+= temp*al[i+2];
	      c[i+3]+= temp*al[i+3];
	    }
	  }
	}
      }
    } else {
      /**
       *           Form  C := alpha*A'*B' + beta*C
       **/
      double *ai, *bl;
      long k3= k&3;
      for (j=0 ; j<n ; j++,c+=ldc) {
	for (i=0,ai=a ; i<m ; i++,ai+=lda) {
	  temp= 0.0;
	  /* unroll innermost loop */
	  for (l=0,bl=b+j ; l<k3 ; l++,bl+=ldb) temp+= ai[l]*bl[0];
	  for (           ; l<k  ; l+=4,bl+=4*ldb)
	    temp+= ai[l]*bl[0] + ai[l+1]*bl[ldb] +
	      ai[l+2]*bl[2*ldb] + ai[l+3]*bl[3*ldb];
	  if( beta==0.0 ) c[i]= alpha*temp;
	  else c[i]= alpha*temp + beta*c[i];
	}
      }
    }
  }

  /**
   *     End of DGEMM .
   **/
}



void dgemv ( char trans, long m, long n, double alpha,
	    double a[], long lda, double x[], long incx,
	    double beta, double y[], long incy )
{
  /**
   *  Purpose
   *  =======
   *
   *  DGEMV  performs one of the matrix-vector operations
   *
   *     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
   *
   *  where alpha and beta are scalars, x and y are vectors and A is an
   *  m by n matrix.
   *
   *  Parameters
   *  ==========
   *
   *  TRANS  - CHARACTER*1.
   *           On entry, TRANS specifies the operation to be performed as
   *           follows:
   *
   *              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
   *
   *              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
   *
   *              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
   *
   *           Unchanged on exit.
   *
   *  M      - INTEGER.
   *           On entry, M specifies the number of rows of the matrix A.
   *           M must be at least zero.
   *           Unchanged on exit.
   *
   *  N      - INTEGER.
   *           On entry, N specifies the number of columns of the matrix A.
   *           N must be at least zero.
   *           Unchanged on exit.
   *
   *  ALPHA  - DOUBLE PRECISION.
   *           On entry, ALPHA specifies the scalar alpha.
   *           Unchanged on exit.
   *
   *  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
   *           Before entry, the leading m by n part of the array A must
   *           contain the matrix of coefficients.
   *           Unchanged on exit.
   *
   *  LDA    - INTEGER.
   *           On entry, LDA specifies the first dimension of A as declared
   *           in the calling (sub) program. LDA must be at least
   *           max( 1, m ).
   *           Unchanged on exit.
   *
   *  X      - DOUBLE PRECISION array of DIMENSION at least
   *           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
   *           and at least
   *           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
   *           Before entry, the incremented array X must contain the
   *           vector x.
   *           Unchanged on exit.
   *
   *  INCX   - INTEGER.
   *           On entry, INCX specifies the increment for the elements of
   *           X. INCX must not be zero.
   *           Unchanged on exit.
   *
   *  BETA   - DOUBLE PRECISION.
   *           On entry, BETA specifies the scalar beta. When BETA is
   *           supplied as zero then Y need not be set on input.
   *           Unchanged on exit.
   *
   *  Y      - DOUBLE PRECISION array of DIMENSION at least
   *           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
   *           and at least
   *           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
   *           Before entry with BETA non-zero, the incremented array Y
   *           must contain the vector y. On exit, Y is overwritten by the
   *           updated vector y.
   *
   *  INCY   - INTEGER.
   *           On entry, INCY specifies the increment for the elements of
   *           Y. INCY must not be zero.
   *           Unchanged on exit.
   **/
  /**
   *  Level 2 Blas routine.
   *
   *  -- Written on 22-October-1986.
   *     Jack Dongarra, Argonne National Lab.
   *     Jeremy Du Croz, Nag Central Office.
   *     Sven Hammarling, Nag Central Office.
   *     Richard Hanson, Sandia National Labs.
   **/
  extern void xerbla(char *,long);
  double    temp;
  long            i, info, j, lenx, leny, m3;
  /**
   *     Test the input parameters.
   **/
  info = 0;
  if     ( !lsame( trans, 'n' )&&
	  !lsame( trans, 't' )&&
	  !lsame( trans, 'c' )      ){
    info = 1;
  } else if( m<0 ){
    info = 2;
  } else if( n<0 ){
    info = 3;
  } else if( lda<max( 1, m ) ){
    info = 6;
  } else if( incx==0 ){
    info = 8;
  } else if( incy==0 ){
    info = 11;
  }
  if( info!=0 ){
    xerbla( "dgemv ", info );
    return;
  }
  /**
   *     Quick return if possible.
   **/
  if( ( m==0 )||( n==0 )||
     ( ( alpha==0.0 )&&( beta==1.0 ) ) )
    return;
  /**
   *     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
   *     up the start points in  X  and  Y.
   **/
  if( lsame( trans, 'n' ) ){
    lenx = n;
    leny = m;
  } else {
    lenx = m;
    leny = n;
  }
  if( incx<0 ) x-= (lenx-1)*incx;
  if( incy<0 ) y-= (lenx-1)*incy;
  m3= m&3;
  /**
   *     Start the operations. In this version the elements of A are
   *     accessed sequentially with one pass through A.
   *
   *     First form  y := beta*y.
   **/
  if( beta!=1.0 ){
    if( incy==1 ){
      if( beta==0.0 ){
	for (i=0 ; i<leny ; i++) y[i]= 0.0;
      } else {
	for (i=0 ; i<leny ; i++) y[i]*= beta;
      }
    } else {
      if( beta==0.0 ){
	for (i=0 ; leny-- ; i+=incy) y[i]= 0.0;
      } else {
	for (i=0 ; leny-- ; i+=incy) y[i]*= beta;
      }
    }
  }
  if( alpha==0.0 )
    return;
  if( lsame( trans, 'n' ) ){
    /**
     *        Form  y := alpha*A*x + y.
     **/
    if( incy==1 ){
      for (j=0 ; n-- ; j+=incx,a+=lda) {
	if( x[j]!=0.0 ){
	  temp= alpha*x[j];
	  /* unroll innermost loop */
	  for (i=0 ; i<m3 ; i++) y[i]+= temp*a[i];
	  for (    ; i<m  ; i+=4) {
	    y[i  ]+= temp*a[i];
	    y[i+1]+= temp*a[i+1];
	    y[i+2]+= temp*a[i+2];
	    y[i+3]+= temp*a[i+3];
	  }
	}
      }
    } else {
      long iy;
      for (j=0 ; n-- ; j+=incx,a+=lda) {
	if( x[j]!=0.0 ){
	  temp = alpha*x[j];
	  /* unroll innermost loop */
	  for (i=iy=0 ; i<m3 ; i++,iy+=incy) y[iy]+= temp*a[i];
	  for (       ; i<m  ; i+=4,iy+=4*incy) {
	    y[iy       ]+= temp*a[i];
	    y[iy+  incy]+= temp*a[i+1];
	    y[iy+2*incy]+= temp*a[i+2];
	    y[iy+3*incy]+= temp*a[i+3];
	  }
	}
      }
    }
  } else {
    /**
     *        Form  y := alpha*A'*x + y.
     **/
    if( incx==1 ){
      for (j=0 ; n-- ; j+=incy,a+=lda) {
	temp= 0.0;
	/* unroll innermost loop */
	for (i=0 ; i<m3 ; i++) temp+= a[i]*x[i];
	for (    ; i<m  ; i+=4)
	  temp+= a[i]*x[i] + a[i+1]*x[i+1] + a[i+2]*x[i+2] + a[i+3]*x[i+3];
	y[j]+= alpha*temp;
      }
    } else {
      long ix;
      for (j=0 ; n-- ; j+=incy,a+=lda) {
	temp = 0.0;
	/* unroll innermost loop */
	for (i=ix=0 ; i<m3 ; i++,ix+=incx) temp+= a[i]*x[ix];
	for (       ; i<m  ; i+=4,ix+=4*incx)
	  temp+= a[i]*x[ix] + a[i+1]*x[ix+incx] +
	    a[i+2]*x[ix+2*incx] + a[i+3]*x[ix+3*incx];
	y[j]+= alpha*temp;
      }
    }
  }

  /**
   *     End of DGEMV .
   **/
}



void dger  ( long m, long n, double alpha, double x[], long incx,
	    double y[], long incy, double a[], long lda )
{
  /**
   *  Purpose
   *  =======
   *
   *  DGER   performs the rank 1 operation
   *
   *     A := alpha*x*y' + A,
   *
   *  where alpha is a scalar, x is an m element vector, y is an n element
   *  vector and A is an m by n matrix.
   *
   *  Parameters
   *  ==========
   *
   *  M      - INTEGER.
   *           On entry, M specifies the number of rows of the matrix A.
   *           M must be at least zero.
   *           Unchanged on exit.
   *
   *  N      - INTEGER.
   *           On entry, N specifies the number of columns of the matrix A.
   *           N must be at least zero.
   *           Unchanged on exit.
   *
   *  ALPHA  - DOUBLE PRECISION.
   *           On entry, ALPHA specifies the scalar alpha.
   *           Unchanged on exit.
   *
   *  X      - DOUBLE PRECISION array of dimension at least
   *           ( 1 + ( m - 1 )*abs( INCX ) ).
   *           Before entry, the incremented array X must contain the m
   *           element vector x.
   *           Unchanged on exit.
   *
   *  INCX   - INTEGER.
   *           On entry, INCX specifies the increment for the elements of
   *           X. INCX must not be zero.
   *           Unchanged on exit.
   *
   *  Y      - DOUBLE PRECISION array of dimension at least
   *           ( 1 + ( n - 1 )*abs( INCY ) ).
   *           Before entry, the incremented array Y must contain the n
   *           element vector y.
   *           Unchanged on exit.
   *
   *  INCY   - INTEGER.
   *           On entry, INCY specifies the increment for the elements of
   *           Y. INCY must not be zero.
   *           Unchanged on exit.
   *
   *  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
   *           Before entry, the leading m by n part of the array A must
   *           contain the matrix of coefficients. On exit, A is
   *           overwritten by the updated matrix.
   *
   *  LDA    - INTEGER.
   *           On entry, LDA specifies the first dimension of A as declared
   *           in the calling (sub) program. LDA must be at least
   *           max( 1, m ).
   *           Unchanged on exit.
   **/
  /**
   *  Level 2 Blas routine.
   *
   *  -- Written on 22-October-1986.
   *     Jack Dongarra, Argonne National Lab.
   *     Jeremy Du Croz, Nag Central Office.
   *     Sven Hammarling, Nag Central Office.
   *     Richard Hanson, Sandia National Labs.
   **/
  extern void xerbla(char *,long);
  double    temp;
  long            i, info, j, m3;
  /**
   *     Test the input parameters.
   **/
  info = 0;
  if     ( m<0 ){
    info = 1;
  } else if( n<0 ){
    info = 2;
  } else if( incx==0 ){
    info = 5;
  } else if( incy==0 ){
    info = 7;
  } else if( lda<max( 1, m ) ){
    info = 9;
  }
  if( info!=0 ){
    xerbla( "dger  ", info );
    return;
  }
  /**
   *     Quick return if possible.
   **/
  if( ( m==0 )||( n==0 )||( alpha==0.0 ) )
    return;
  /**
   *     Start the operations. In this version the elements of A are
   *     accessed sequentially with one pass through A.
   **/
  m3= m&3;
  if( incy<0 ) y-= (n-1)*incy;
  if( incx==1 ){
    for (j=0 ; n-- ; j+=incy,a+=lda)
      if( y[j]!=0.0 ){
	temp= alpha*y[j];
	/* unroll innermost loop */
	for (i=0 ; i<m3 ; i++) a[i]+= x[i]*temp;
	for (    ; i<m  ; i+=4) {
	  a[i  ]+= x[i]*temp;
	  a[i+1]+= x[i+1]*temp;
	  a[i+2]+= x[i+2]*temp;
	  a[i+3]+= x[i+3]*temp;
	}
      }
  } else {
    long ix;
    if( incx<0 ) x-= (m-1)*incx;
    for (j=0 ; n-- ; j+=incy,a+=lda) {
      if( y[j]!=0.0 ){
	temp = alpha*y[j];
	/* unroll innermost loop */
	for (i=ix=0 ; i<m3 ; i++,ix+=incx) a[i]+= x[ix]*temp;
	for (       ; i<m  ; i+=4,ix+=4*incx) {
	  a[i  ]+= x[ix]*temp;
	  a[i+1]+= x[ix+incx]*temp;
	  a[i+2]+= x[ix+2*incx]*temp;
	  a[i+3]+= x[ix+3*incx]*temp;
	}
      }
    }
  }

  /**
   *     End of DGER  .
   **/
}



double   dnrm2 ( long n, double dx[], long incx)
{
  /*c
    c     euclidean norm of the n-vector stored in dx_1() with storage
    c     increment incx .
    c     if    n .le. 0 return with result = 0.
    c     if n .ge. 1 then incx must be .ge. 1
    c
    c           c.l.lawson, 1978 jan 08
    c     modified to correct failure to update ix, 1/25/92.
    c     modified 3/93 to return if incx .le. 0.
    c
    c     four phase method     using two built-in constants that are
    c     hopefully applicable to all machines.
    c         cutlo = maximum of  sqrt(u/eps)  over all known machines.
    c         cuthi = minimum of  sqrt(v)      over all known machines.
    c     where
    c         eps = smallest no. such that eps + 1. .gt. 1.
    c         u   = smallest positive no.   (underflow limit)
    c         v   = largest  no.            (overflow  limit)
    c
    c     brief outline of algorithm..
    c
    c     phase 1    scans zero components.
    c     move to phase 2 when a component is nonzero and .le. cutlo
    c     move to phase 3 when a component is .gt. cutlo
    c     move to phase 4 when a component is .ge. cuthi/m
    c     where m = n for x() real and m = 2*n for complex.
    c
    c     values for cutlo and cuthi..
    c     from the environmental parameters listed in the imsl converter
    c     document the limiting values are as follows..
    c     cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are
    c                   univac and dec at 2**(-103)
    c                   thus cutlo = 2**(-51) = 4.44089e-16
    c     cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.
    c                   thus cuthi = 2**(63.5) = 1.30438e19
    c     cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.
    c                   thus cutlo = 2**(-33.5) = 8.23181d-11
    c     cuthi, d.p.   same as s.p.  cuthi = 1.30438d19
    c     data cutlo, cuthi / 8.232d-11,  1.304d19 /
    c     data cutlo, cuthi / 4.441e-16,  1.304e19 /  */
#undef cutlo
#define cutlo 8.232e-11
#undef cuthi
#define cuthi 1.304e19
  double  hitest, sum, adx, xmax;

  if( n<=0 || incx<=0 ) return 0.0;
  hitest= cuthi/n;  /* n will change as calculation proceeds */

  adx= dabs(dx[0]);

  /* phase 1 */
  for (dx+=incx,n-- ; adx==0.0 && n ; dx+=incx,n--) adx= dabs(dx[0]);
  if (!n) return adx;  /* note that adx can be any size here */

  /* phase 2, adx is first non-zero element */
  if (adx <= cutlo) {
    xmax= adx;
    sum= 1.0;
    for (;;) {
      adx= dabs(dx[0]);
      if (adx > cutlo) break;
      if (adx > xmax) {
	/* renormalize to new largest element */
	sum= 1.0 + sum * (xmax/adx)*(xmax/adx);
	xmax= adx;
      } else {
	/* continue with current normalization */
	sum+= (adx/xmax)*(adx/xmax);
      }
      n--;
      if (!n) return xmax*sqrt(sum);
      dx+= incx;
    }
    /* here, sum>=1.0, but xmax<=cutlo, so xmax*xmax might underflow */
    sum*= xmax;  /* remove phase 2 normalization */
    sum*= xmax;  /* don't want the compiler to do sum*(xmax*xmax) */
    n--;
    dx+= incx;

  } else {
    sum= 0.0;
  }

  /* phase 3, adx is first element>cutlo */
  if (adx<hitest) {
    sum+= adx*adx;
    for (;;) {
      if (!n) return sqrt(sum);
      adx= dabs(dx[0]);
      n--;
      dx+= incx;
      if (adx>=hitest) break;
      sum+= adx*adx;
    }
  }

  /* phase 4, adx is first element greater than hitest */
  xmax= adx;
  sum/= xmax;   /* again, try to avoid sum/(xmax*xmax) */
  sum/= xmax;
  sum+= 1.0;
  while (n) {
    adx= dabs(dx[0]);
    if (adx > xmax) {
      /* renormalize to new largest element */
      sum= 1.0 + sum * (xmax/adx)*(xmax/adx);
      xmax= adx;
    } else {
      /* continue with current normalization */
      sum+= (adx/xmax)*(adx/xmax);
    }
    n--;
    dx+= incx;
  }
  return xmax*sqrt(sum);
}



void  drot (long n,double dx[],long incx,double dy[],long incy,
	    double c,double s)
{
  /*c
    c     applies a plane rotation.
    c     jack dongarra, linpack, 3/11/78.
    c*/
  double  dtemp;

  if (n<=0) return;
  if (incx!=1 || incy!=1) {
    /*       unequal increments or equal increments not equal to 1 */
    long ix, iy;
    if(incx<0) dx-= (n-1)*incx;
    if(incy<0) dy-= (n-1)*incy;
    for (ix=iy=0 ; n-- ; ix+=incx,iy+=incy) {
      dtemp = c*dx[ix] + s*dy[iy];
      dy[iy]= c*dy[iy] - s*dx[ix];
      dx[ix]= dtemp;
    }
  } else {
    /*       both increments equal to 1 */
    long i;
    for (i=0 ; i<n ; i++) {
      dtemp= c*dx[i] + s*dy[i];
      dy[i]= c*dy[i] - s*dx[i];
      dx[i]= dtemp;
    }
  }
  return;
}



void  dscal(long n,double da,double dx[],long incx)
{
  /*c
    c     scales a vector by a constant.
    c     uses unrolled loops for increment equal to one.
    c     jack dongarra, linpack, 3/11/78.
    c     modified 3/93 to return if incx .le. 0.
    c*/
  long i;
  if (n<=0) return;
  if (incx!=1) {
  /*        increment not equal to 1 */
    if (incx<0) dx-= (n-1)*incx;
    for (i=0 ; n-- ; i+=incx) dx[i]*= da;
  } else {
    /*        increment equal to 1 */
    /* unroll loop */
    long m= n%5;
    for (i=0 ; i<m ; i++) dx[i]*= da;
    for (    ; i<n ; i+=5) {
      dx[i  ]*= da;
      dx[i+1]*= da;
      dx[i+2]*= da;
      dx[i+3]*= da;
      dx[i+4]*= da;
    }
  }
}



void  dswap (long n,double dx[],long incx,double dy[],long incy)
{
  /*c
    c     interchanges two vectors.
    c     uses unrolled loops for increments equal one.
    c     jack dongarra, linpack, 3/11/78.
    c*/
  double  dtemp;
  long i;
  if (n<=0) return;
  if (incx!=1 || incy!=1) {
    /*        unequal increments or equal increments not equal to 1 */
    long iy;
    if (incx<0) dx-= (n-1)*incx;
    if (incy<0) dy-= (n-1)*incx;
    for (i=iy=0 ; n-- ; i+=incx,iy+=incy) {
      dtemp= dx[i]; dx[i]= dy[iy]; dy[iy]= dtemp;
    }
  } else {
    /*        both increments equal to 1 */
    /* unroll loop */
    long m= n%3;
    for (i=0 ; i<m ; i++) { dtemp= dx[i]; dx[i]= dy[i]; dy[i]= dtemp; }
    for (    ; i<n ; i+=3) {
      dtemp= dx[i  ]; dx[i  ]= dy[i  ]; dy[i  ]= dtemp;
      dtemp= dx[i+1]; dx[i+1]= dy[i+1]; dy[i+1]= dtemp;
      dtemp= dx[i+2]; dx[i+2]= dy[i+2]; dy[i+2]= dtemp;
    }
  }
}



void dtrmm ( char side, char uplo, char transa, char diag,
	    long m, long n, double alpha, double a[], long lda,
	    double b[], long ldb )
{
  /**
   *  Purpose
   *  =======
   *
   *  DTRMM  performs one of the matrix-matrix operations
   *
   *     B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
   *
   *  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
   *  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
   *
   *     op( A ) = A   or   op( A ) = A'.
   *
   *  Parameters
   *  ==========
   *
   *  SIDE   - CHARACTER*1.
   *           On entry,  SIDE specifies whether  op( A ) multiplies B from
   *           the left or right as follows:
   *
   *              SIDE = 'L' or 'l'   B := alpha*op( A )*B.
   *
   *              SIDE = 'R' or 'r'   B := alpha*B*op( A ).
   *
   *           Unchanged on exit.
   *
   *  UPLO   - CHARACTER*1.
   *           On entry, UPLO specifies whether the matrix A is an upper or
   *           lower triangular matrix as follows:
   *
   *              UPLO = 'U' or 'u'   A is an upper triangular matrix.
   *
   *              UPLO = 'L' or 'l'   A is a lower triangular matrix.
   *
   *           Unchanged on exit.
   *
   *  TRANSA - CHARACTER*1.
   *           On entry, TRANSA specifies the form of op( A ) to be used in
   *           the matrix multiplication as follows:
   *
   *              TRANSA = 'N' or 'n'   op( A ) = A.
   *
   *              TRANSA = 'T' or 't'   op( A ) = A'.
   *
   *              TRANSA = 'C' or 'c'   op( A ) = A'.
   *
   *           Unchanged on exit.
   *
   *  DIAG   - CHARACTER*1.
   *           On entry, DIAG specifies whether or not A is unit triangular
   *           as follows:
   *
   *              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
   *
   *              DIAG = 'N' or 'n'   A is not assumed to be unit
   *                                  triangular.
   *
   *           Unchanged on exit.
   *
   *  M      - INTEGER.
   *           On entry, M specifies the number of rows of B. M must be at
   *           least zero.
   *           Unchanged on exit.
   *
   *  N      - INTEGER.
   *           On entry, N specifies the number of columns of B.  N must be
   *           at least zero.
   *           Unchanged on exit.
   *
   *  ALPHA  - DOUBLE PRECISION.
   *           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
   *           zero then  A is not referenced and  B need not be set before
   *           entry.
   *           Unchanged on exit.
   *
   *  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
   *           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
   *           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
   *           upper triangular part of the array  A must contain the upper
   *           triangular matrix  and the strictly lower triangular part of
   *           A is not referenced.
   *           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
   *           lower triangular part of the array  A must contain the lower
   *           triangular matrix  and the strictly upper triangular part of
   *           A is not referenced.
   *           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
   *           A  are not referenced either,  but are assumed to be  unity.
   *           Unchanged on exit.
   *
   *  LDA    - INTEGER.
   *           On entry, LDA specifies the first dimension of A as declared
   *           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
   *           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
   *           then LDA must be at least max( 1, n ).
   *           Unchanged on exit.
   *
   *  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
   *           Before entry,  the leading  m by n part of the array  B must
   *           contain the matrix  B,  and  on exit  is overwritten  by the
   *           transformed matrix.
   *
   *  LDB    - INTEGER.
   *           On entry, LDB specifies the first dimension of B as declared
   *           in  the  calling  (sub)  program.   LDB  must  be  at  least
   *           max( 1, m ).
   *           Unchanged on exit.
   **/
  /**
   *  Level 3 Blas routine.
   *
   *  -- Written on 8-February-1989.
   *     Jack Dongarra, Argonne National Laboratory.
   *     Iain Duff, AERE Harwell.
   *     Jeremy Du Croz, Numerical Algorithms Group Ltd.
   *     Sven Hammarling, Numerical Algorithms Group Ltd.
   **/
  extern void xerbla(char *,long);
  int            lside, nounit, upper;
  long            i, info, j, k, nrowa;
  double    temp;
  /**
   *     Test the input parameters.
   **/
  lside  = lsame( side  , 'l' );
  if( lside ){
    nrowa = m;
  } else {
    nrowa = n;
  }
  nounit = lsame( diag  , 'n' );
  upper  = lsame( uplo  , 'u' );

  info   = 0;
  if(      ( !lside                )&&
     ( !lsame( side  , 'r' ) )      ){
    info = 1;
  } else if( ( !upper                )&&
	    ( !lsame( uplo  , 'l' ) )      ){
    info = 2;
  } else if( ( !lsame( transa, 'n' ) )&&
	    ( !lsame( transa, 't' ) )&&
	    ( !lsame( transa, 'c' ) )      ){
    info = 3;
  } else if( ( !lsame( diag  , 'u' ) )&&
	    ( !lsame( diag  , 'n' ) )      ){
    info = 4;
  } else if( m  <0               ){
    info = 5;
  } else if( n  <0               ){
    info = 6;
  } else if( lda<max( 1, nrowa ) ){
    info = 9;
  } else if( ldb<max( 1, m     ) ){
    info = 11;
  }
  if( info!=0 ){
    xerbla( "dtrmm ", info );
    return;
  }
  /**
   *     Quick return if possible.
   **/
  if( n==0 )
    return;
  /**
   *     And when  alpha.eq.0.0.
   **/
  if( alpha==0.0 ){
    for ( ; n-- ; b+=ldb)
      for (i=0 ; i<m ; i++) b[i]= 0.0;
    return;
  }
  /**
   *     Start the operations.
   **/
  if( lside ){
    if( lsame( transa, 'n' ) ){
      /**
       *           Form  B := alpha*A*B.
       **/
      double *ak;
      if( upper ){
	for ( ; n-- ; b+=ldb) {
	  for (k=0,ak=a ; k<m ; k++,ak+=lda) {
	    if( b[k]!=0.0 ){
	      temp= alpha*b[k];
	      /* hand unroll inner loop */
	      for (i=0 ; i<(k&3) ; i++) b[i]+= temp*ak[i];
	      for (    ; i<k     ; i+=4) {
		b[i  ]+= temp*ak[i];
		b[i+1]+= temp*ak[i+1];
		b[i+2]+= temp*ak[i+2];
		b[i+3]+= temp*ak[i+3];
	      }
	      if( nounit ) temp*= ak[k];
	      b[k]= temp;
	    }
	  }
	}
      } else {
	long count;
	a+= (m-1)*lda;
	for ( ; n-- ; b+=ldb) {
	  for (k=m-1,ak=a ; k>=0 ; k--,ak-=lda) {
	    if( b[k]!=0.0 ){
	      temp= alpha*b[k];
	      b[k]= temp;
	      if( nounit ) b[k]*= ak[k];
	      /* hand unroll inner loop */
	      count= (m-1-k)&3;
	      for (i=k+1 ; count-- ; i++) b[i]+= temp*ak[i];
	      for (      ; i<m     ; i+=4) {
		b[i  ]+= temp*ak[i];
		b[i+1]+= temp*ak[i+1];
		b[i+2]+= temp*ak[i+2];
		b[i+3]+= temp*ak[i+3];
	      }
	    }
	  }
	}
      }
    } else {
      /**
       *           Form  B := alpha*B*A'.
       **/
      double *ai;
      if( upper ){
	a+= (m-1)*lda;
	for ( ; n-- ; b+=ldb) {
	  for (i=m-1,ai=a ; i>=0 ; i--,a-=lda) {
	    temp= b[i];
	    if( nounit ) temp*= ai[i];
	    /* hand unroll inner loop */
	    for (k=0 ; k<(i&3) ; k++) temp+= ai[k]*b[k];
	    for (    ; k<i     ; k+=4)
	      temp+= ai[k]*b[k] + ai[k+1]*b[k+1] + ai[k+2]*b[k+2] +
		ai[k+3]*b[k+3];
	    b[i]= alpha*temp;
	  }
	}
      } else {
	long count;
	for ( ; n-- ; b+=ldb) {
	  for (i=0,ai=a ; i<m ; i++,a+=lda) {
	    temp= b[i];
	    if( nounit ) temp*= ai[i];
	    /* hand unroll inner loop */
	    count= (m-1-i)&3;
	    for (k=i+1 ; count-- ; k++) temp+= ai[k]*b[k];
	    for (      ; k<m     ; k+=4)
	      temp+= ai[k]*b[k] + ai[k+1]*b[k+1] + ai[k+2]*b[k+2] +
		ai[k+3]*b[k+3];
	    b[i]= alpha*temp;
	  }
	}
      }
    }
  } else {
    if( lsame( transa, 'n' ) ){
      /**
       *           Form  B := alpha*B*A.
       **/
      double *bj, *bk;
      if( upper ){
	for (j=n-1,a+=j*lda,bj=b+j*ldb ; j>=0 ; j--,a-=lda,bj-=ldb) {
	  temp= alpha;
	  if( nounit ) temp*= a[j];
	  for (i=0 ; i<m ; i++) bj[i]*= temp;
	  for (k=0,bk=b ; k<j ; k++,bk+=ldb) {
	    if( a[k]!=0.0 ){
	      temp= alpha*a[k];
	      /* unroll inner loop */
	      for (i=0 ; i<(m&3) ; i++) bj[i]+= temp*bk[i];
	      for (    ; i<m     ; i+=4) {
		bj[i  ]+= temp*bk[i];
		bj[i+1]+= temp*bk[i+1];
		bj[i+2]+= temp*bk[i+2];
		bj[i+3]+= temp*bk[i+3];
	      }
	    }
	  }
	}
      } else {
	for (j=0,bj=b ; j<n ; j++,a+=lda,bj+=ldb) {
	  temp= alpha;
	  if( nounit ) temp*= a[j];
	  for (i=0 ; i<m ; i++) bj[i]*= temp;
	  for (k=j+1,bk=b+k*ldb ; k<n ; k++,bk+=ldb) {
	    if( a[k]!=0.0 ){
	      temp= alpha*a[k];
	      /* unroll inner loop */
	      for (i=0 ; i<(m&3) ; i++) bj[i]+= temp*bk[i];
	      for (    ; i<m     ; i+=4) {
		bj[i  ]+= temp*bk[i];
		bj[i+1]+= temp*bk[i+1];
		bj[i+2]+= temp*bk[i+2];
		bj[i+3]+= temp*bk[i+3];
	      }
	    }
	  }
	}
      }
    } else {
      /**
       *           Form  B := alpha*B*A'.
       **/
      double *bj, *bk;
      if( upper ){
	for (k=0,bk=b ; k<n ; k++,a+=lda,bk+=ldb) {
	  for (j=0,bj=b ; j<k ; j++,bj+=ldb) {
	    if( a[j]!=0.0 ){
	      temp= alpha*a[j];
	      /* unroll inner loop */
	      for (i=0 ; i<(m&3) ; i++) bj[i]+= temp*bk[i];
	      for (    ; i<m     ; i+=4) {
		bj[i  ]+= temp*bk[i];
		bj[i+1]+= temp*bk[i+1];
		bj[i+2]+= temp*bk[i+2];
		bj[i+3]+= temp*bk[i+3];
	      }
	    }
	  }
	  temp= alpha;
	  if( nounit ) temp*= a[k];
	  if( temp!=1.0 ) for (i=0 ; i<m ; i++) bk[i]*= temp;
	}
      } else {
	for (k=n-1,a+=k*lda,bk=b+k*ldb ; k>=0 ; k--,a-=lda,bk-=ldb) {
	  for (j=k+1,bj=b+j*ldb ; j<n ; j++,bj+=ldb) {
	    if( a[j]!=0.0 ){
	      temp = alpha*a[j];
	      /* unroll inner loop */
	      for (i=0 ; i<(m&3) ; i++) bj[i]+= temp*bk[i];
	      for (    ; i<m     ; i+=4) {
		bj[i  ]+= temp*bk[i];
		bj[i+1]+= temp*bk[i+1];
		bj[i+2]+= temp*bk[i+2];
		bj[i+3]+= temp*bk[i+3];
	      }
	    }
	  }
	  temp= alpha;
	  if( nounit ) temp*= a[k];
	  if( temp!=1.0 ) for (i=0 ; i<m ; i++) bk[i]*= temp;
	}
      }
    }
  }

  return;
  /**
   *     End of DTRMM .
   **/
}



void dtrmv ( char uplo, char trans, char diag, long n,
	    double a[], long lda, double x[], long incx )
{
  /**
   *  Purpose
   *  =======
   *
   *  DTRMV  performs one of the matrix-vector operations
   *
   *     x := A*x,   or   x := A'*x,
   *
   *  where x is an n element vector and  A is an n by n unit, or non-unit,
   *  upper or lower triangular matrix.
   *
   *  Parameters
   *  ==========
   *
   *  UPLO   - CHARACTER*1.
   *           On entry, UPLO specifies whether the matrix is an upper or
   *           lower triangular matrix as follows:
   *
   *              UPLO = 'U' or 'u'   A is an upper triangular matrix.
   *
   *              UPLO = 'L' or 'l'   A is a lower triangular matrix.
   *
   *           Unchanged on exit.
   *
   *  TRANS  - CHARACTER*1.
   *           On entry, TRANS specifies the operation to be performed as
   *           follows:
   *
   *              TRANS = 'N' or 'n'   x := A*x.
   *
   *              TRANS = 'T' or 't'   x := A'*x.
   *
   *              TRANS = 'C' or 'c'   x := A'*x.
   *
   *           Unchanged on exit.
   *
   *  DIAG   - CHARACTER*1.
   *           On entry, DIAG specifies whether or not A is unit
   *           triangular as follows:
   *
   *              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
   *
   *              DIAG = 'N' or 'n'   A is not assumed to be unit
   *                                  triangular.
   *
   *           Unchanged on exit.
   *
   *  N      - INTEGER.
   *           On entry, N specifies the order of the matrix A.
   *           N must be at least zero.
   *           Unchanged on exit.
   *
   *  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
   *           Before entry with  UPLO = 'U' or 'u', the leading n by n
   *           upper triangular part of the array A must contain the upper
   *           triangular matrix and the strictly lower triangular part of
   *           A is not referenced.
   *           Before entry with UPLO = 'L' or 'l', the leading n by n
   *           lower triangular part of the array A must contain the lower
   *           triangular matrix and the strictly upper triangular part of
   *           A is not referenced.
   *           Note that when  DIAG = 'U' or 'u', the diagonal elements of
   *           A are not referenced either, but are assumed to be unity.
   *           Unchanged on exit.
   *
   *  LDA    - INTEGER.
   *           On entry, LDA specifies the first dimension of A as declared
   *           in the calling (sub) program. LDA must be at least
   *           max( 1, n ).
   *           Unchanged on exit.
   *
   *  X      - DOUBLE PRECISION array of dimension at least
   *           ( 1 + ( n - 1 )*abs( INCX ) ).
   *           Before entry, the incremented array X must contain the n
   *           element vector x. On exit, X is overwritten with the
   *           tranformed vector x.
   *
   *  INCX   - INTEGER.
   *           On entry, INCX specifies the increment for the elements of
   *           X. INCX must not be zero.
   *           Unchanged on exit.
   **/
  /**
   *  Level 2 Blas routine.
   *
   *  -- Written on 22-October-1986.
   *     Jack Dongarra, Argonne National Lab.
   *     Jeremy Du Croz, Nag Central Office.
   *     Sven Hammarling, Nag Central Office.
   *     Richard Hanson, Sandia National Labs.
   **/
  extern void xerbla(char *,long);
  double    temp;
  long            i, info, ix, j, jx;
  int            nounit;
  /**
   *     Test the input parameters.
   **/
  info = 0;
  if     ( !lsame( uplo , 'u' )&&
	  !lsame( uplo , 'l' )      ){
    info = 1;
  } else if( !lsame( trans, 'n' )&&
	    !lsame( trans, 't' )&&
	    !lsame( trans, 'c' )      ){
    info = 2;
  } else if( !lsame( diag , 'u' )&&
	    !lsame( diag , 'n' )      ){
    info = 3;
  } else if( n<0 ){
    info = 4;
  } else if( lda<max( 1, n ) ){
    info = 6;
  } else if( incx==0 ){
    info = 8;
  }
  if( info!=0 ){
    xerbla( "dtrmv ", info );
    return;
  }
  /**
   *     Quick return if possible.
   **/
  if( n==0 ) return;

  nounit = lsame( diag, 'n' );
  /**
   *     Set up the start point in X if the increment is negative.
   **/
  if( incx<=0 ) x-= (n-1)*incx;
  /**
   *     Start the operations. In this version the elements of A are
   *     accessed sequentially with one pass through A.
   **/
  if( lsame( trans, 'n' ) ){
    /**
     *        Form  x := A*x.
     **/
    if( lsame( uplo, 'u' ) ){
      if( incx==1 ){
	for (j=0 ; j<n ; j++,a+=lda) {
	  if( x[j]!=0.0 ){
	    temp= x[j];
	    /* unroll inner loop */
	    for (i=0 ; i<(j&3) ; i++) x[i]+= temp*a[i];
	    for (    ; i<j     ; i+=4) {
	      x[i  ]+= temp*a[i];
	      x[i+1]+= temp*a[i+1];
	      x[i+2]+= temp*a[i+2];
	      x[i+3]+= temp*a[i+3];
	    }
	    if( nounit ) x[j]*= a[j];
	  }
	}
      } else {
	for (j=jx=0 ; j<n ; j++,jx+=incx,a+=lda) {
	  if( x[jx]!=0.0 ){
	    temp= x[jx];
	    /* unroll inner loop */
	    for (i=ix=0 ; i<(j&3) ; i++,ix+=incx) x[ix]+= temp*a[i];
	    for (       ; i<j ; i+=4,ix+=4*incx) {
	      x[ix       ]+= temp*a[i];
	      x[ix+  incx]+= temp*a[i+1];
	      x[ix+2*incx]+= temp*a[i+2];
	      x[ix+3*incx]+= temp*a[i+3];
	    }
	    if( nounit ) x[jx]*= a[j];
	  }
	}
      }
    } else {
      long count;
      if( incx==1 ){
	for (j=n-1,a+=j*lda ; j>=0 ; j--,a-=lda) {
	  if( x[j]!=0.0 ){
	    temp= x[j];
	    /* unroll inner loop */
	    count= (n-1-j)&3;
	    for (i=n-1 ; count-- ; i--) x[i]+= temp*a[i];
	    for (      ; i>j     ; i-=4) {
	      x[i  ]+= temp*a[i];
	      x[i-1]+= temp*a[i-1];
	      x[i-2]+= temp*a[i-2];
	      x[i-3]+= temp*a[i-3];
	    }
	    if( nounit ) x[j]*= a[j];
	  }
	}
      } else {
	for (j=n-1,jx=j*incx,a+=j*lda ; j>=0 ; j--,jx-=incx,a-=lda) {
	  if( x[jx]!=0.0 ){
	    temp= x[jx];
	    /* unroll inner loop */
	    count= (n-1-j)&3;
	    for (i=n-1,ix=i*incx ; count-- ; i--,ix-=incx) x[ix]+= temp*a[i];
	    for (                ; i>j     ; i-=4,ix-=4*incx) {
	      x[ix       ]+= temp*a[i];
	      x[ix-  incx]+= temp*a[i-1];
	      x[ix-2*incx]+= temp*a[i-2];
	      x[ix-3*incx]+= temp*a[i-3];
	    }
	    if( nounit ) x[jx]*= a[j];
	  }
	}
      }
    }
  } else {
    /**
     *        Form  x := A'*x.
     **/
    long count;
    if( lsame( uplo, 'u' ) ){
      if( incx==1 ){
	for (j=n-1,a+=j*lda ; j>=0 ; j--,a-=lda) {
	  temp= x[j];
	  if( nounit ) temp*= a[j];
	  if (j) {
	    /* unroll inner loop */
	    count= (j-1)&3;
	    for (i=j-1 ; count-- ; i--) temp+= a[i]*x[i];
	    for (      ; i>0 ; i-=4) temp+= a[i]*x[i] +
	      a[i-1]*x[i-1] + a[i-2]*x[i-2] + a[i-3]*x[i-3];
	  }
	  x[j]= temp;
	}
      } else {
	for (j=n-1,jx=j*incx,a+=j*lda ; j>=0 ; j--,jx-=incx,a-=lda) {
	  temp= x[jx];
	  if( nounit ) temp*= a[j];
	  if (j) {
	    /* unroll inner loop */
	    count= (j-1)&3;
	    for (i=j-1,ix=i*incx ; count-- ; i--,ix-=incx) temp+= a[i]*x[ix];
	    for (                ; i>0 ; i-=4,ix-=4*incx) temp+= a[i]*x[ix] +
	      a[i-1]*x[i-incx] + a[i-2]*x[i-2*incx] + a[i-3]*x[i-3*incx];
	  }
	  x[jx]= temp;
	}
      }
    } else {
      if( incx==1 ){
	for (j=0 ; j<n ; j++,a+=lda) {
	  temp= x[j];
	  if( nounit ) temp*= a[j];
	  /* unroll inner loop */
	  count= (n-1-j)&3;
	  for (i=j+1 ; count-- ; i++) temp+= a[i]*x[i];
	  for (      ; i<n     ; i+=4) temp+= a[i]*x[i] +
	    a[i+1]*x[i+1] + a[i+2]*x[i+2] + a[i+3]*x[i+3];
	  x[j]= temp;
	}
      } else {
	for (j=jx=0 ; j<n ; j++,jx+=incx,a+=lda) {
	  temp= x[jx];
	  if( nounit ) temp*= a[j];
	  /* unroll inner loop */
	  count= (n-1-j)&3;
	  for (i=j+1,ix=i*incx ; count-- ; i++,ix+=incx) temp+= a[i]*x[ix];
	  for (             ; i<n     ; i+=4,ix+=4*incx) temp+= a[i]*x[ix] +
	    a[i+1]*x[i+incx] + a[i+2]*x[i+2*incx] + a[i+3]*x[i+3*incx];
	  x[jx]= temp;
	}
      }
    }
  }

  return;
  /**
   *     End of DTRMV .
   **/
}



void dtrsm ( char side, char uplo, char transa, char diag,
	    long m, long n, double alpha, double a[], long lda,
	    double b[], long ldb )
{
  /**
   *  Purpose
   *  =======
   *
   *  DTRSM  solves one of the matrix equations
   *
   *     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
   *
   *  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
   *  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
   *
   *     op( A ) = A   or   op( A ) = A'.
   *
   *  The matrix X is overwritten on B.
   *
   *  Parameters
   *  ==========
   *
   *  SIDE   - CHARACTER*1.
   *           On entry, SIDE specifies whether op( A ) appears on the left
   *           or right of X as follows:
   *
   *              SIDE = 'L' or 'l'   op( A )*X = alpha*B.
   *
   *              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
   *
   *           Unchanged on exit.
   *
   *  UPLO   - CHARACTER*1.
   *           On entry, UPLO specifies whether the matrix A is an upper or
   *           lower triangular matrix as follows:
   *
   *              UPLO = 'U' or 'u'   A is an upper triangular matrix.
   *
   *              UPLO = 'L' or 'l'   A is a lower triangular matrix.
   *
   *           Unchanged on exit.
   *
   *  TRANSA - CHARACTER*1.
   *           On entry, TRANSA specifies the form of op( A ) to be used in
   *           the matrix multiplication as follows:
   *
   *              TRANSA = 'N' or 'n'   op( A ) = A.
   *
   *              TRANSA = 'T' or 't'   op( A ) = A'.
   *
   *              TRANSA = 'C' or 'c'   op( A ) = A'.
   *
   *           Unchanged on exit.
   *
   *  DIAG   - CHARACTER*1.
   *           On entry, DIAG specifies whether or not A is unit triangular
   *           as follows:
   *
   *              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
   *
   *              DIAG = 'N' or 'n'   A is not assumed to be unit
   *                                  triangular.
   *
   *           Unchanged on exit.
   *
   *  M      - INTEGER.
   *           On entry, M specifies the number of rows of B. M must be at
   *           least zero.
   *           Unchanged on exit.
   *
   *  N      - INTEGER.
   *           On entry, N specifies the number of columns of B.  N must be
   *           at least zero.
   *           Unchanged on exit.
   *
   *  ALPHA  - DOUBLE PRECISION.
   *           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
   *           zero then  A is not referenced and  B need not be set before
   *           entry.
   *           Unchanged on exit.
   *
   *  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
   *           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
   *           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
   *           upper triangular part of the array  A must contain the upper
   *           triangular matrix  and the strictly lower triangular part of
   *           A is not referenced.
   *           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
   *           lower triangular part of the array  A must contain the lower
   *           triangular matrix  and the strictly upper triangular part of
   *           A is not referenced.
   *           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
   *           A  are not referenced either,  but are assumed to be  unity.
   *           Unchanged on exit.
   *
   *  LDA    - INTEGER.
   *           On entry, LDA specifies the first dimension of A as declared
   *           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
   *           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
   *           then LDA must be at least max( 1, n ).
   *           Unchanged on exit.
   *
   *  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
   *           Before entry,  the leading  m by n part of the array  B must
   *           contain  the  right-hand  side  matrix  B,  and  on exit  is
   *           overwritten by the solution matrix  X.
   *
   *  LDB    - INTEGER.
   *           On entry, LDB specifies the first dimension of B as declared
   *           in  the  calling  (sub)  program.   LDB  must  be  at  least
   *           max( 1, m ).
   *           Unchanged on exit.
   **/
  /**
   *  Level 3 Blas routine.
   **/
  /**
   *  -- Written on 8-February-1989.
   *     Jack Dongarra, Argonne National Laboratory.
   *     Iain Duff, AERE Harwell.
   *     Jeremy Du Croz, Numerical Algorithms Group Ltd.
   *     Sven Hammarling, Numerical Algorithms Group Ltd.
   **/
  extern void xerbla(char *,long);
  int            lside, nounit, upper;
  long            i, info, j, k, nrowa;
  double    temp;
  /**
   *     Test the input parameters.
   **/
  lside  = lsame( side  , 'l' );
  if( lside ){
    nrowa = m;
  } else {
    nrowa = n;
  }
  nounit = lsame( diag  , 'n' );
  upper  = lsame( uplo  , 'u' );

  info   = 0;
  if(      ( !lside                )&&
     ( !lsame( side  , 'r' ) )      ){
    info = 1;
  } else if( ( !upper                )&&
	    ( !lsame( uplo  , 'l' ) )      ){
    info = 2;
  } else if( ( !lsame( transa, 'n' ) )&&
	    ( !lsame( transa, 't' ) )&&
	    ( !lsame( transa, 'c' ) )      ){
    info = 3;
  } else if( ( !lsame( diag  , 'u' ) )&&
	    ( !lsame( diag  , 'n' ) )      ){
    info = 4;
  } else if( m  <0               ){
    info = 5;
  } else if( n  <0               ){
    info = 6;
  } else if( lda<max( 1, nrowa ) ){
    info = 9;
  } else if( ldb<max( 1, m     ) ){
    info = 11;
  }
  if( info!=0 ){
    xerbla( "dtrsm ", info );
    return;
  }
  /**
   *     Quick return if possible.
   **/
  if( n==0 ) return;
  /**
   *     And when  alpha.eq.0.0.
   **/
  if( alpha==0.0 ){
    for ( ; n-- ; b+=ldb) {
      for (i=0 ; i<m ; i++) b[i]= 0.0;
    }
    return;
  }
  /**
   *     Start the operations.
   **/
  if( lside ){
    if( lsame( transa, 'n' ) ){
      /**
       *           Form  B := alpha*inv( A )*B.
       **/
      double *ak;
      if( upper ){
	for (j=0 ; j<n ; j++,b+=ldb) {
	  if( alpha!=1.0 ) for (i=0 ; i<m ; i++) b[i]*= alpha;
	  for (k=m-1,ak=a+k*lda ; k>=0 ; k--,ak-=lda) {
	    if( b[k]!=0.0 ){
	      if( nounit ) b[k]/= ak[k];
	      /* unroll inner loop */
	      for (i=0 ; i<(k&3) ; i++) b[i]-= b[k]*ak[i];
	      for (    ; i<k     ; i+=4) {
		b[i  ]-= b[k]*ak[i];
		b[i+1]-= b[k]*ak[i+1];
		b[i+2]-= b[k]*ak[i+2];
		b[i+3]-= b[k]*ak[i+3];
	      }
	    }
	  }
	}
      } else {
	long count;
	for (j=0 ; j<n ; j++,b+=ldb) {
	  if( alpha!=1.0 ) for (i=0 ; i<m ; i++) b[i]*= alpha;
	  for (k=0,ak=a ; k<m ; k++,ak+=lda) {
	    if( b[k]!=0.0 ){
	      if( nounit ) b[k]/= ak[k];
	      /* unroll inner loop */
	      count= (m-1-k)&3;
	      for (i=k+1 ; count-- ; i++) b[i]-= b[k]*ak[i];
	      for (      ; i<m     ; i+=4) {
		b[i  ]-= b[k]*ak[i];
		b[i+1]-= b[k]*ak[i+1];
		b[i+2]-= b[k]*ak[i+2];
		b[i+3]-= b[k]*ak[i+3];
	      }
	    }
	  }
	}
      }
    } else {
      /**
       *           Form  B := alpha*inv( A' )*B.
       **/
      double *ai;
      if( upper ){
	for (j=0 ; j<n ; j++,b+=ldb) {
	  for (i=0,ai=a ; i<m ; i++,ai+=lda) {
	    temp= alpha*b[i];
	    /* unroll inner loop */
	    for (k=0 ; k<(i&3) ; k++) temp-= ai[k]*b[k];
	    for (    ; k<i     ; k+=4) temp-= ai[k]*b[k] +
	       ai[k+1]*b[k+1] + ai[k+2]*b[k+2] + ai[k+3]*b[k+3];
	    if( nounit ) temp/= ai[i];
	    b[i]= temp;
	  }
	}
      } else {
	long count;
	for (j=0 ; j<n ; j++,b+=ldb) {
	  for (i=m-1,ai=a+i*lda ; i>=0 ; i--,ai-=lda) {
	    temp= alpha*b[i];
	    /* unroll inner loop */
	    count= (m-1-i)&3;
	    for (k=i+1 ; count-- ; k++) temp-= ai[k]*b[k];
	    for (      ; k<m     ; k+=4) temp-= ai[k]*b[k] +
	       ai[k+1]*b[k+1] + ai[k+2]*b[k+2] + ai[k+3]*b[k+3];
	    if( nounit ) temp/= ai[i];
	    b[i]= temp;
	  }
	}
      }
    }
  } else {
    if( lsame( transa, 'n' ) ){
      /**
       *           Form  B := alpha*B*inv( A ).
       **/
      double *bj, *bk;
      if( upper ){
	for (j=0,bj=b ; j<n ; j++,a+=lda,bj+=ldb) {
	  if( alpha!=1.0 ) for (i=0 ; i<m ; i++) bj[i]*= alpha;
	  for (k=0,bk=b ; k<j ; k++,bk+=ldb) {
	    if( a[k]!=0.0 ){
	      /* unroll inner loop */
	      for (i=0 ; i<(m&3) ; i++) bj[i]-= a[k]*bk[i];
	      for (    ; i<m     ; i+=4) {
		bj[i  ]-= a[k]*bk[i];
		bj[i+1]-= a[k]*bk[i+1];
		bj[i+2]-= a[k]*bk[i+2];
		bj[i+3]-= a[k]*bk[i+3];
	      }
	    }
	  }
	  if( nounit ){
	    temp= 1.0/a[j];
	    for (i=0 ; i<m ; i++) bj[i]*= temp;
	  }
	}
      } else {
	for (j=n-1,a+=j*lda,bj=b+j*ldb ; j>=0 ; j--,a-=lda,bj-=ldb) {
	  if( alpha!=1.0 ) for (i=0 ; i<m ; i++) bj[i]*= alpha;
	  for (k=j+1,bk=b+k*ldb ; k<n ; k++,bk+=ldb) {
	    if( a[k]!=0.0 ){
	      /* unroll inner loop */
	      for (i=0 ; i<(m&3) ; i++) bj[i]-= a[k]*bk[i];
	      for (    ; i<m     ; i+=4) {
		bj[i  ]-= a[k]*bk[i];
		bj[i+1]-= a[k]*bk[i+1];
		bj[i+2]-= a[k]*bk[i+2];
		bj[i+3]-= a[k]*bk[i+3];
	      }
	    }
	  }
	  if( nounit ){
	    temp= 1.0/a[j];
	    for (i=0 ; i<m ; i++) bj[i]*= temp;
	  }
	}
      }
    } else {
      /**
       *           Form  B := alpha*B*inv( A' ).
       **/
      double *bj, *bk;
      if( upper ){
	for (k=n-1,a+=k*lda,bk=b+k*ldb ; k>=0 ; k--,a-=lda,bk-=ldb) {
	  if( nounit ){
	    temp= 1.0/a[k];
	    for (i=0 ; i<m ; i++) bk[i]*= temp;
	  }
	  for (j=0,bj=b ; j<k ; j++,bj+=ldb) {
	    if( a[j]!=0.0 ){
	      temp= a[j];
	      /* unroll inner loop */
	      for (i=0 ; i<(m&3) ; i++) bj[i]-= temp*bk[i];
	      for (    ; i<m     ; i+=4) {
		bj[i  ]-= temp*bk[i];
		bj[i+1]-= temp*bk[i+1];
		bj[i+2]-= temp*bk[i+2];
		bj[i+3]-= temp*bk[i+3];
	      }
	    }
	  }
	  if( alpha!=1.0 ) for (i=0 ; i<m ; i++) bk[i]*= alpha;
	}
      } else {
	for (k=0,bk=b ; k<n ; k++,a+=lda,bk+=ldb) {
	  if( nounit ){
	    temp= 1.0/a[k];
	    for (i=0 ; i<m ; i++) bk[i]*= temp;
	  }
	  for (j=k+1,bj=b+j*ldb ; j<n ; j++,bj+=ldb) {
	    if( a[j]!=0.0 ){
	      temp= a[j];
	      /* unroll inner loop */
	      for (i=0 ; i<(m&3) ; i++) bj[i]-= temp*bk[i];
	      for (    ; i<m     ; i+=4) {
		bj[i  ]-= temp*bk[i];
		bj[i+1]-= temp*bk[i+1];
		bj[i+2]-= temp*bk[i+2];
		bj[i+3]-= temp*bk[i+3];
	      }
	    }
	  }
	  if( alpha!=1.0 ) for (i=0 ; i<m ; i++) bk[i]*= alpha;
	}
      }
    }
  }

  /**
   *     End of DTRSM .
   **/
}



void dtrsv ( char uplo, char trans, char diag, long n,
	    double a[], long lda, double x[], long incx )
{
  /**
   *  Purpose
   *  =======
   *
   *  DTRSV  solves one of the systems of equations
   *
   *     A*x = b,   or   A'*x = b,
   *
   *  where b and x are n element vectors and A is an n by n unit, or
   *  non-unit, upper or lower triangular matrix.
   *
   *  No test for singularity or near-singularity is included in this
   *  routine. Such tests must be performed before calling this routine.
   *
   *  Parameters
   *  ==========
   *
   *  UPLO   - CHARACTER*1.
   *           On entry, UPLO specifies whether the matrix is an upper or
   *           lower triangular matrix as follows:
   *
   *              UPLO = 'U' or 'u'   A is an upper triangular matrix.
   *
   *              UPLO = 'L' or 'l'   A is a lower triangular matrix.
   *
   *           Unchanged on exit.
   *
   *  TRANS  - CHARACTER*1.
   *           On entry, TRANS specifies the equations to be solved as
   *           follows:
   *
   *              TRANS = 'N' or 'n'   A*x = b.
   *
   *              TRANS = 'T' or 't'   A'*x = b.
   *
   *              TRANS = 'C' or 'c'   A'*x = b.
   *
   *           Unchanged on exit.
   *
   *  DIAG   - CHARACTER*1.
   *           On entry, DIAG specifies whether or not A is unit
   *           triangular as follows:
   *
   *              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
   *
   *              DIAG = 'N' or 'n'   A is not assumed to be unit
   *                                  triangular.
   *
   *           Unchanged on exit.
   *
   *  N      - INTEGER.
   *           On entry, N specifies the order of the matrix A.
   *           N must be at least zero.
   *           Unchanged on exit.
   *
   *  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
   *           Before entry with  UPLO = 'U' or 'u', the leading n by n
   *           upper triangular part of the array A must contain the upper
   *           triangular matrix and the strictly lower triangular part of
   *           A is not referenced.
   *           Before entry with UPLO = 'L' or 'l', the leading n by n
   *           lower triangular part of the array A must contain the lower
   *           triangular matrix and the strictly upper triangular part of
   *           A is not referenced.
   *           Note that when  DIAG = 'U' or 'u', the diagonal elements of
   *           A are not referenced either, but are assumed to be unity.
   *           Unchanged on exit.
   *
   *  LDA    - INTEGER.
   *           On entry, LDA specifies the first dimension of A as declared
   *           in the calling (sub) program. LDA must be at least
   *           max( 1, n ).
   *           Unchanged on exit.
   *
   *  X      - DOUBLE PRECISION array of dimension at least
   *           ( 1 + ( n - 1 )*abs( INCX ) ).
   *           Before entry, the incremented array X must contain the n
   *           element right-hand side vector b. On exit, X is overwritten
   *           with the solution vector x.
   *
   *  INCX   - INTEGER.
   *           On entry, INCX specifies the increment for the elements of
   *           X. INCX must not be zero.
   *           Unchanged on exit.
   **/
  /**
   *  Level 2 Blas routine.
   *
   *  -- Written on 22-October-1986.
   *     Jack Dongarra, Argonne National Lab.
   *     Jeremy Du Croz, Nag Central Office.
   *     Sven Hammarling, Nag Central Office.
   *     Richard Hanson, Sandia National Labs.
   **/
  extern void xerbla(char *,long);
  double    temp;
  long            i, info, ix, j, jx;
  int            nounit;
  /**
   *     Test the input parameters.
   **/
  info = 0;
  if     ( !lsame( uplo , 'u' )&&
	  !lsame( uplo , 'l' )      ){
    info = 1;
  } else if( !lsame( trans, 'n' )&&
	    !lsame( trans, 't' )&&
	    !lsame( trans, 'c' )      ){
    info = 2;
  } else if( !lsame( diag , 'u' )&&
	    !lsame( diag , 'n' )      ){
    info = 3;
  } else if( n<0 ){
    info = 4;
  } else if( lda<max( 1, n ) ){
    info = 6;
  } else if( incx==0 ){
    info = 8;
  }
  if( info!=0 ){
    xerbla( "dtrsv ", info );
    return;
  }
  /**
   *     Quick return if possible.
   **/
  if( n==0 ) return;

  nounit = lsame( diag, 'n' );
  /**
   *     Set up the start point in X if the increment is not unity.
   **/
  if( incx<=0 ) x-= (n-1)*incx;
  /**
   *     Start the operations. In this version the elements of A are
   *     accessed sequentially with one pass through A.
   **/
  if( lsame( trans, 'n' ) ){
    /**
     *        Form  x := inv( A )*x.
     **/
    if( lsame( uplo, 'u' ) ){
      long count;
      if( incx==1 ){
	for (j=n-1,a+=j*lda ; j>=0 ; j--,a-=lda) {
	  if( x[j]!=0.0 ){
	    if( nounit ) x[j]/= a[j];
	    if (!j) break;
	    temp= x[j];
	    /* unroll inner loop */
	    count= (j-1)&3;
	    for (i=j-1 ; count-- ; i--) x[i]-= temp*a[i];
	    for (      ; i>0    ; i-=4) {
	      x[i  ]-= temp*a[i];
	      x[i-1]-= temp*a[i-1];
	      x[i-2]-= temp*a[i-2];
	      x[i-3]-= temp*a[i-3];
	    }
	  }
	}
      } else {
	for (j=n-1,jx=j*incx,a+=j*lda ; j>=0 ; j--,jx-=incx,a-=lda) {
	  if( x[jx]!=0.0 ){
	    if( nounit ) x[jx]/= a[j];
	    if (!j) break;
	    temp= x[jx];
	    /* unroll inner loop */
	    count= (j-1)&3;
	    for (i=j-1,ix=i*incx ; count-- ; i--,ix-=incx) x[ix]-= temp*a[i];
	    for (                ; i>0    ; i-=4,ix-=4*incx) {
	      x[ix       ]-= temp*a[i];
	      x[ix-  incx]-= temp*a[i-1];
	      x[ix-2*incx]-= temp*a[i-2];
	      x[ix-3*incx]-= temp*a[i-3];
	    }
	  }
	}
      }
    } else {
      long count;
      if( incx==1 ){
	for (j=0 ; j<n ; j++,a+=lda) {
	  if( x[j]!=0.0 ){
	    if( nounit ) x[j]/= a[j];
	    temp= x[j];
	    /* unroll inner loop */
	    count= (n-1-j)&3;
	    for (i=j+1 ; count-- ; i++) x[i]-= temp*a[i];
	    for (      ; i<n     ; i+=4) {
	      x[i  ]-= temp*a[i];
	      x[i+1]-= temp*a[i+1];
	      x[i+2]-= temp*a[i+2];
	      x[i+3]-= temp*a[i+3];
	    }
	  }
	}
      } else {
	for (j=jx=0 ; j<n ; j++,jx+=incx,a+=lda) {
	  if( x[jx]!=0.0 ){
	    if( nounit ) x[jx]/= a[j];
	    temp= x[jx];
	    /* unroll inner loop */
	    count= (n-1-j)&3;
	    for (i=j+1,ix=i*incx ; count-- ; i++,ix+=incx) x[ix]-= temp*a[i];
	    for (                ; i<n     ; i+=4,ix+=4*incx) {
	      x[ix       ]-= temp*a[i];
	      x[ix+  incx]-= temp*a[i+1];
	      x[ix+2*incx]-= temp*a[i+2];
	      x[ix+3*incx]-= temp*a[i+3];
	    }
	  }
	}
      }
    }
  } else {
    /**
     *        Form  x := inv( A' )*x.
     **/
    if( lsame( uplo, 'u' ) ){
      if( incx==1 ){
	for (j=0 ; j<n ; j++,a+=lda) {
	  temp= x[j];
	  /* unroll inner loop */
	  for (i=0 ; i<(j&3) ; i++) temp-= a[i]*x[i];
	  for (    ; i<j     ; i+=4) temp-= a[i]*x[i] +
	    a[i+1]*x[i+1] + a[i+2]*x[i+2] + a[i+3]*x[i+3];
	  if( nounit ) temp/= a[j];
	  x[j]= temp;
	}
      } else {
	for (j=jx=0 ; j<n ; j++,jx+=incx,a+=lda) {
	  temp= x[jx];
	  /* unroll inner loop */
	  for (i=ix=0 ; i<(j&3) ; i++,ix+=incx) temp-= a[i]*x[ix];
	  for (       ; i<j     ; i+=4,ix+=4*incx) temp-= a[i]*x[ix] +
	    a[i+1]*x[ix+incx] + a[i+2]*x[ix+2*incx] + a[i+3]*x[ix+3*incx];
	  if( nounit ) temp/= a[j];
	  x[jx]= temp;
	}
      }
    } else {
      long count;
      if( incx==1 ){
	for (j=n-1,a+=j*lda ; j>=0 ; j--,a-=lda) {
	  temp= x[j];
	  /* unroll inner loop */
	  count= (n-1-j)&3;
	  for (i=n-1 ; count-- ; i--) temp-= a[i]*x[i];
	  for (      ; i>j     ; i-=4) temp-= a[i]*x[i] +
	    a[i-1]*x[i-1] + a[i-2]*x[i-2] + a[i-3]*x[i-3];
	  if( nounit ) temp/= a[j];
	  x[j]= temp;
	}
      } else {
	for (j=n-1,jx=j*incx,a+=j*lda ; j>=0 ; j--,jx-=incx,a-=lda) {
	  temp= x[jx];
	  /* unroll inner loop */
	  count= (n-1-j)&3;
	  for (i=n-1,ix=i*incx ; count-- ; i--,ix-=incx) temp-= a[i]*x[ix];
	  for (                ; i>j  ; i-=4,ix-=4*incx) temp-= a[i]*x[ix] +
	    a[i-1]*x[ix-incx] + a[i-2]*x[ix-2*incx] + a[i-3]*x[ix-3*incx];
	  if( nounit ) temp/= a[j];
	  x[jx]= temp;
	}
      }
    }
  }

  /**
   *     End of DTRSV .
   **/
}



long  idamax(long n,double dx[],long incx)
{
  /*c
    c     finds the index of element having max. absolute value.
    c     jack dongarra, linpack, 3/11/78.
    c     modified 3/93 to return if incx .le. 0.
    c*/
  long iamax= 0;
  double dmax;
  long i;
  if ( n<=0 || incx<=0 ) return iamax;
  if (incx!=1) {
    /*        increment not equal to 1 */
    long ix;
    dmax= dabs(dx[0]);
    for (i=1,ix=incx ; --n ; i++,ix+=incx)
      if (dabs(dx[ix])>dmax) {
	dmax= dabs(dx[ix]);
	iamax= i;
      }
  } else {
    /*        increment equal to 1 */
    dmax= dabs(dx[0]);
    for (i=1 ; --n ; i++)
      if (dabs(dx[i])>dmax) {
	dmax= dabs(dx[i]);
	iamax= i;
      }
  }
  return iamax+1;
}
