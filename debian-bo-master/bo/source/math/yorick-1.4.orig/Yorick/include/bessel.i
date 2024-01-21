/*
   BESSEL.I
   A few Bessel functions.

   $Id: bessel.i,v 1.1 1993/08/27 18:50:06 munro Exp $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

/* Taken from Numerical Recipes.  */

/* ------------------------------------------------------------------------ */

func bessj0(x)
/* DOCUMENT bessj0(x)
     returns Bessel function J0 at points X.
   SEE ALSO: bessj
 */
{
  ax= abs(x);
  small= (ax<8.0);
  list= where(small);
  if (numberof(list)) {
    xx= x(list);
    y= xx*xx;
    s= poly(y, 57568490574.0, -13362590354.0, 651619640.7, -11214424.18,
	    77392.33017, -184.9052456) /
       poly(y, 57568490411.0, 1029532985.0, 9494680.718, 59272.64853,
	    267.8532712, 1.0);
  }
  list= where(!small);
  if (numberof(list)) {
    x= x(list);
    ax= abs(x);
    z= 8.0/ax;
    y= z*z;
    xx= ax-0.785398164;  /* pi/4, rounded incorrectly */
    l= sqrt(0.636619772/ax) *
       (cos(xx)*poly(y, 1.0, -0.1098628627e-2,
		     0.2734510407e-4, -0.2073370639e-5, 0.2093887211e-6) -
	sin(xx)*z*poly(y, -0.1562499995e-1, 0.1430488765e-3,
		       -0.6911147651e-5, 0.7621095161e-6, -0.934935152e-7));
  }
  return merge(s, l, small);
}

func bessj1(x)
/* DOCUMENT bessj1(x)
     returns Bessel function J1 at points X.
   SEE ALSO: bessj
 */
{
  ax= abs(x);
  small= (ax<8.0);
  list= where(small);
  if (numberof(list)) {
    xx= x(list);
    y= xx*xx;
    s= xx * poly(y, 72362614232.0, -7895059235.0, 242396853.1, -2972611.439,
		 15704.48260, -30.16036606) /
       poly(y, 144725228442.0, 2300535178.0, 18583304.74, 99447.43394,
	    376.9991397, 1.0);
  }
  list= where(!small);
  if (numberof(list)) {
    x= x(list);
    ax= abs(x);
    z= 8.0/ax;
    y= z*z;
    xx= ax-2.356194491;  /* 3*pi/4 */
    l= sign(x) * sqrt(0.636619772/ax) *
       (cos(xx)*poly(y, 1.0, 0.183105e-2, -0.3516396496e-4,
		     0.2457520174e-5, -0.240337019e-6) -
	sin(xx)*z*poly(y, 0.04687499995, -0.2002690873e-3, 0.8449199096e-5,
		       -0.88228987e-6, 0.105787412e-6));
  }
  return merge(s, l, small);
}

func bessj(n, x)
/* DOCUMENT bessj(n, x)
     returns Bessel function Jn of order N at points X.  N must be scalar.
   SEE ALSO: bessy, bessi, bessk, bessj0, bessj1
 */
{
  if (n>1) {
    ax= abs(x);
    big= (ax > n);
    list= where(big);
    if (numberof(list)) {
      /* upward recurrence */
      ax= abs(x(list));
      tox= 2.0/ax;
      bjm= bessj0(ax);
      bj= bessj1(ax);
      for (i=1 ; i<n ; i++) {
	bjp= i*tox*bj-bjm;
	bjm= bj;
	bj= bjp;
      }
    }
    list= where(!big);
    if (numberof(list)) {
      ax= abs(x(list));
      zero= (ax==0.0);
      list= where(zero);
      if (numberof(list)) {
	bj0= ax(list);  /* == 0.0 */
      }
      list= where(!zero);
      if (numberof(list)) {
	/* downward recurrence */
	ax= ax(list);
	tox= 2.0/ax;
	m= 2*((n+long(sqrt(bess_acc*n)))/2);
	jsum= 0;
	bjp= ans= add= array(0.0, numberof(ax));
	bj1= array(1.0, numberof(ax));
	for (i=m ; i>0 ; i--) {
	  bjm= i*tox*bj1-bjp;
	  bjp= bj1;
	  bj1= bjm;
	  renorm= (abs(bj1)>bess_big);
	  list= where(renorm);
	  if (numberof(list)) {
	    bj1(list)/= bess_big;
	    bjp(list)/= bess_big;
	    ans(list)/= bess_big;
	    add(list)/= bess_big;
	  }
	  if (jsum) add+= bj1;
	  jsum= !jsum;
	  if (i==n) ans= bjp;
	}
	bj1= ans/(2.0*add-bj1);
      }
      bj1= merge(bj0, bj1, zero);
    }
    bj= merge(bj, bj1, big);
    if (n%2) bj*= sign(x);
    return bj;
  } else if (n==1) {
    return bessj1(x);
  } else if (!n) {
    return bessj0(x);
  }
}

/* ------------------------------------------------------------------------ */

func bessy0(x)
/* DOCUMENT bessy0(x)
     returns Bessel function Y0 at points X.
   SEE ALSO: bessy
 */
{
  ax= abs(x);
  small= (ax<8.0);
  list= where(small);
  if (numberof(list)) {
    xx= x(list);
    y= xx*xx;
    s= poly(y, -2957821389.0, 7062834065.0, -512359803.6, 10879881.29,
	    -86327.92757, 228.4622733) /
       poly(y, 40076544269.0, 745249964.8, 7189466.438, 47447.26470,
	    226.1030244, 1.0) + 0.636619772*bessj0(x)*log(x);
  }
  list= where(!small);
  if (numberof(list)) {
    x= x(list);
    ax= abs(x);
    z= 8.0/ax;
    y= z*z;
    xx= ax-0.785398164;  /* pi/4, rounded incorrectly */
    l= sqrt(0.636619772/ax) *
       (sin(xx)*poly(y, 1.0, -0.1098628627e-2, 0.2734510407e-4,
		     -0.2073370639e-5, 0.2093887211e-6) -
	cos(xx)*z*poly(y, -0.1562499995e-1, 0.1430488765e-3,
		       -0.6911147651e-5, 0.7621095161e-6, -0.934935152e-7));
  }
  return merge(s, l, small);
}

func bessy1(x)
/* DOCUMENT bessy1(x)
     returns Bessel function Y1 at points X.
   SEE ALSO: bessy
 */
{
  ax= abs(x);
  small= (ax<8.0);
  list= where(small);
  if (numberof(list)) {
    xx= x(list);
    y= xx*xx;
    s= xx * poly(y, -0.4900604943e13, 0.1275274390e13, -0.5153438139e11,
		 0.7349264551e9, -0.4237922726e7, 0.8511937935e4) /
       poly(y, 0.2499580570e14, 0.4244419664e12, 0.3733650367e10,
	    0.2245904002e8, 0.1020426050e6, 0.3549632885e3, 1.0) +
       0.636619772*(bessj1(x)*log(x)-1.0/x);
  }
  list= where(!small);
  if (numberof(list)) {
    x= x(list);
    ax= abs(x);
    z= 8.0/ax;
    y= z*z;
    xx= ax-2.356194491;  /* 3*pi/4 */
    l= sqrt(0.636619772/x) *
       (sin(xx)*poly(y, 1.0, 0.183105e-2, -0.3516396496e-4,
		     0.2457520174e-5, -0.240337019e-6) +
	cos(xx)*z*poly(y, 0.04687499995, -0.2002690873e-3, 0.8449199096e-5,
		       -0.88228987e-6, 0.105787412e-6));
  }
  return merge(s, l, small);
}

func bessy(n, x)
/* DOCUMENT bessy(n, x)
     returns Bessel function Yn of order N at points X.  N must be scalar.
   SEE ALSO: bessj, bessi, bessk, bessy0, bessy1
 */
{
  if (n>1) {
    /* upward recurrence */
    tox= 2.0/x;
    bym= bessy0(x);
    by= bessy1(x);
    for (i=1 ; i<n ; i++) {
      byp= i*tox*by-bym;
      bym= by;
      by= byp;
    }
    return by;
  } else if (n==1) {
    return bessy1(x);
  } else if (!n) {
    return bessy0(x);
  }
}

/* ------------------------------------------------------------------------ */

func bessi0(x)
/* DOCUMENT bessi0(x)
     returns Bessel function I0 at points X.
   SEE ALSO: bessi
 */
{
  ax= abs(x);
  small= (ax<3.75);
  list= where(small);
  if (numberof(list)) {
    xx= x(list)/3.75;
    y= xx*xx;
    s= poly(y, 1.0, 3.5156229, 3.0899424, 1.2067492, 0.2659732,
	    0.360768e-1, 0.45813e-2);
  }
  list= where(!small);
  if (numberof(list)) {
    x= x(list);
    ax= abs(x);
    y= 3.75/ax;
    l= (exp(ax)/sqrt(ax)) * poly(y, 0.39894228, 0.1328592e-1, 0.225319e-2,
				 -0.157565e-2, 0.916281e-2, -0.2057706e-1,
				 0.2635537e-1, -0.1647633e-1, 0.392377e-2);
  }
  return merge(s, l, small);
}

func bessi1(x)
/* DOCUMENT bessi1(x)
     returns Bessel function I1 at points X.
   SEE ALSO: bessi
 */
{
  ax= abs(x);
  small= (ax<3.75);
  list= where(small);
  if (numberof(list)) {
    xx= x(list);
    y= xx/3.75;
    y*= y;
    s= abs(xx) * poly(y, 0.5, 0.87890594, 0.51498869, 0.15084934,
		      0.2658733e-1, 0.301532e-2, 0.32411e-3);
  }
  list= where(!small);
  if (numberof(list)) {
    ax= ax(list);
    y= 3.75/ax;
    l= (exp(ax)/sqrt(ax)) *
       poly(y, 0.39894228, -0.3988024e-1, -0.362018e-2, 0.163801e-2,
	    -0.1031555e-1, 0.2282967e-1, -0.2895312e-1, 0.1787654e-1,
	    -0.420059e-2);
  }
  return sign(x) * merge(s, l, small);
}

func bessi(n, x)
/* DOCUMENT bessi(n, x)
     returns Bessel function In of order N at points X.  N must be scalar.
   SEE ALSO: bessk, bessj, bessy, bessi0, bessi1
 */
{
  if (n>1) {
    zero= (x==0.0);
    list= where(zero);
    if (numberof(list)) {
      bi0= x(list);  /* == 0.0 */
    }
    list= where(!zero);
    if (numberof(list)) {
      /* downward recurrence */
      x= x(list);
      ax= abs(x);
      tox= 2.0/ax;
      m= 2*(n+long(sqrt(bess_acc*n)));
      bip= ans= array(0.0, numberof(ax));
      bi= array(1.0, numberof(ax));
      for (i=m ; i>0 ; i--) {
	bim= i*tox*bi+bip;
	bip= bi;
	bi= bim;
	list= where(abs(bi) > bess_big);
	if (numberof(list)) {
	  ans(list)/= bess_big;
	  bi(list)/= bess_big;
	  bip(list)/= bess_big;
	}
	if (i==n) ans= bip;
      }
      bi= ans*bessi0(x)/bi;
      if (n%2) bi*= sign(x);
    }
    return merge(bi0, bi, zero);
  } else if (n==1) {
    return bessi1(x);
  } else if (!n) {
    return bessi0(x);
  }
}

/* ------------------------------------------------------------------------ */

func bessk0(x)
/* DOCUMENT bessk0(x)
     returns Bessel function K0 at points X.
   SEE ALSO: bessk
 */
{
  small= (x<=2.0);
  list= where(small);
  if (numberof(list)) {
    xx= x(list);
    y= xx*xx/4.0;
    s= (-log(xx/2.0)*bessi0(xx)) +
       poly(y, -0.57721566, 0.42278420, 0.23069756, 0.3488590e-1, 0.262698e-2,
	    0.10750e-3, 0.74e-5);
  }
  list= where(!small);
  if (numberof(list)) {
    x= x(list);
    y= 2.0/x;
    l= (exp(-x)/sqrt(x)) *
       poly(y, 1.25331414, -0.7832358e-1, 0.2189568e-1, -0.1062446e-1,
	    0.587872e-2, -0.251540e-2, 0.53208e-3);
  }
  return merge(s, l, small);
}

func bessk1(x)
/* DOCUMENT bessk1(x)
     returns Bessel function K1 at points X.
   SEE ALSO: bessk
 */
{
  small= (x<=2.0);
  list= where(small);
  if (numberof(list)) {
    xx= x(list);
    y= xx*xx/4.0;
    s= (log(xx/2.0)*bessi1(xx)) +
       (1.0/xx) * poly(y, 1.0, 0.15443144, -0.67278579, -0.18156897,
		       -0.1919402e-1, -0.110404e-2, -0.4686e-4);
  }
  list= where(!small);
  if (numberof(list)) {
    x= x(list);
    y= 2.0/x;
    l= (exp(-x)/sqrt(x)) *
       poly(y, 1.25331414, 0.23498619, -0.3655620e-1, 0.1504268e-1,
	    -0.780353e-2, 0.325614e-2, -0.68245e-3);
  }
  return merge(s, l, small);
}

func bessk(n, x)
/* DOCUMENT bessk(n, x)
     returns Bessel function Kn of order N at points X.  N must be scalar.
   SEE ALSO: bessi, bessj, bessy, bessi0, bessi1
 */
{
  if (n>1) {
    /* upward recurrence */
    tox= 2.0/x;
    bkm= bessk0(x);
    bk= bessk1(x);
    for (i=1 ; i<n ; i++) {
      bkp= i*tox*bk+bkm;
      bkm= bk;
      bk= bkp;
    }
    return bk;
  } else if (n==1) {
    return bessk1(x);
  } else if (!n) {
    return bessk0(x);
  }
}

/* ------------------------------------------------------------------------ */

bess_acc= 40.0;
bess_big= 1.e10;

/* ------------------------------------------------------------------------ */
