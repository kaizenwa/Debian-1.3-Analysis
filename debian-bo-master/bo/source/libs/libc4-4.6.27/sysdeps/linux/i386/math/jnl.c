/*
	floating point Bessel's function of
	the first and second kinds and of
	integer order.

	int n;
	double x;
	jn(n,x);

	returns the value of Jn(x) for all
	integer values of n and all real values
	of x.

	There are no error returns.
	Calls j0l, j1l.

	For n=0, j0l(x) is called,
	for n=1, j1l(x) is called,
	for n<x, forward recursion us used starting
	from values of j0l(x) and j1l(x).
	for n>x, a continued fraction approximation to
	j(n,x)/j(n-1,x) is evaluated and then backward
	recursion is used starting from a supposed value
	for j(n,x). The resulting value of j(0,x) is
	compared with the actual value to correct the
	supposed value of j(n,x).

	yn(n,x) is similar in all respects, except
	that forward recursion is used for all
	values of n>1.
*/

#include "mathl.h"

/* long double j0l(), j1l(), y0l(), y1l(); */

long double jnl(int n, long double x)
{
	int i, sign;
	long double a, b, temp;
	long double xsq, t;

/*
                  n
   J  (x)  =  (-1)   J (x)
    -n                n
                  n
   J (-x)  =  (-1)   J (x)
    n                 n
 */
	sign = 1;
	if(n<0){
		n = -n;
		if( n & 1 )
		  sign = -1;
	}
	if( x < 0.0L )
	  {
	    if( n & 1 )
	      sign = -sign;
	    x = -x;
	  }
	if(n==0) return(j0l(x));
	if(n==1) return(sign*j1l(x));
	if(x == 0.L) return(0.L);

	if(n>x) goto recurs;

	a = j0l(x);
	b = j1l(x);
	for(i=1;i<n;i++){
		temp = b;
		b = (2.L*i/x)*b - a;
		a = temp;
	}
	return(sign*b);
recurs:

	xsq = x*x;
	for(t=0,i=n+16;i>n;i--){
		t = xsq/(2.L*i - t);
	}
	t = x/(2.L*n-t);

	a = t;
	b = 1;
	for(i=n-1;i>0;i--){
		temp = b;
		b = (2.L*i/x)*b - a;
		a = temp;
	}
	return(sign*t*j0l(x)/b);
}

long double ynl(int n, long double x)
{
	int i;
	int sign;
	long double a, b, temp;

	if (x <= 0) {
		return(-1.189731495357231765021263853E4932L);
	}
	sign = 1;
	if(n<0){
		n = -n;
		if(n%2 == 1) sign = -1;
	}
	if(n==0) return(y0l(x));
	if(n==1) return(sign*y1l(x));

	a = y0l(x);
	b = y1l(x);
	for(i=1;i<n;i++){
		temp = b;
		b = (2.L*i/x)*b - a;
		a = temp;
	}
	return(sign*b);
}
