/*
   CFFT2.C
   FFT with strides.

   $Id$
 */

/*C
C     EXTENSION TO SWARZTRAUBER FFT ROUTINES TO ALLOW FOR
C     MULTIDIMENSIONAL FFTS
C
C     CALL CFFT2(IDIR, C, ISTD, N, N2, WS)
C
C     IDIR    1 FOR FORWARD TRANSFORM (CFFTF),
C            -1 FOR BACKWARD TRANSFORM (CFFTB)
C     C      THE COMPLEX ARRAY TO BE TRANSFORMED (IN PLACE)
C     ISTD   THE STRIDE OF THE DIMENSION OF C TO BE TRANSFORMED
C            ISTD=1 MEANS THE FIRST DIMENSION OF C IS TO BE TRANSFORMED,
C            ISTD=LEN1 WHERE LEN1 IS THE LENGTH OF THE FIRST DIMENSION
C                      MEANS TO TRANSFORM THE SECOND DIMENSION
C            ISTD=LEN1*LEN2 TRANSFORMS THE THIRD DIMENSION, AND SO ON
C     N      THE LENGTH OF THE DIMENSION TO BE TRANSFORMED
C     N2     THE PRODUCT OF ALL DIMENSION LENGTHS AFTER THE DIMENSION
C            TO BE TRANSFORMED -- THE TRANSFORM OF LENGTH N WILL BE
C            REPEATED A TOTAL OF ISTD*N2 TIMES
C     WS     THE WORKING ARRAY FILLED BY CFFTI
C            IF ISTD=1, ONLY 4*N+15 ELEMENTS OF STORAGE WILL BE USED,
C            AS FOR CFFTF OR CFFTB
C            IF ISTD>1, WS NEEDS TO HAVE 6*N+15 ELEMENTS OF STORAGE
C            (AS A WORKING ARRAY FOR THE DIMENSION BEING TRANSFORMED)
C*/



/*-----prototypes of functions defined here-----*/
extern void cfft2(long idir, double c[], long istd, long n, long n2,
		  double ws[]);
/*-----end of prototypes-----*/

extern void cfftb1 (long n,double c[],double ch[],double wa[],long ifac[]);
extern void cfftf1 (long n,double c[],double ch[],double wa[],long ifac[]);


/*-----Fortran intrinsics converted-----*/
/*-----end of Fortran intrinsics-----*/



void cfft2(long idir, double c[], long istd, long n, long n2, double ws[])
{
  /*      implicit double  (a-h,o-z);*/
#undef ws_1
#define ws_1(a1) ws[a1-1]
#undef c_4
#define c_4(a1,a2,a3,a4) c[a1-1+2*(a2-1+istd*(a3-1+n*(a4-1)))]
  /*-----implicit-declarations-----*/
  long j;
  long i0;
  long i2;
  long iw3;
  long iw2;
  long iw1;
  /*-----end-of-declarations-----*/
  if (n == 1) return;
  iw1 = n+n+1;
  iw2 = iw1+n+n;
  iw3 = iw2+15;
  for (i2=1 ; i2<=n2 ; i2+=1) {
    if (istd < 2) goto L_600;
    /*C        IF (ISTD .GE. 2) THEN*/
    for (i0=1 ; i0<=istd ; i0+=1) {
      for (j=1 ; j<=n ; j+=1) {
	ws_1(iw3+2*j-2)= c_4(1,i0,j,i2);
	ws_1(iw3+2*j-1)= c_4(2,i0,j,i2);
      }
      if (idir < 0) goto L_200;
      /*C             IF (IDIR .GT. 0) THEN*/
      cfftf1 (n,&ws_1(iw3),ws,&ws_1(iw1),(long *)&ws_1(iw2));
      goto L_300;
      /*C             ELSE (IDIR .LT. 0)*/
    L_200:
      cfftb1 (n,&ws_1(iw3),ws,&ws_1(iw1),(long *)&ws_1(iw2));
      /*C             ENDIF (IDIR)*/
    L_300:
      for (j=1 ; j<=n ; j+=1) {
	c_4(1,i0,j,i2)= ws_1(iw3+2*j-2);
	c_4(2,i0,j,i2)= ws_1(iw3+2*j-1);
      }
    }
    goto L_800;
    /*C       ELSE (ISTD .LT. 2)*/
  L_600:
    if (idir < 0) goto L_650;
    /*C          IF (IDIR .GT. 0) THEN*/
    cfftf1 (n,&c_4(1,1,1,i2),ws,&ws_1(iw1),(long *)&ws_1(iw2));
    goto L_700;
    /*C          ELSE (IDIR .GT. 0)*/
  L_650:
    cfftb1 (n,&c_4(1,1,1,i2),ws,&ws_1(iw1),(long *)&ws_1(iw2));
    /*C          ENDIF (IDIR)*/
  L_700:
    /*C       ENDIF (ISTD)*/
  L_800:;
  }
  return;
}
