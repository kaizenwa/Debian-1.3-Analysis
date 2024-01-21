/*
   GAMMA.I
   Gamma function, beta function, and binomial coefficients.

   $Id$
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

/* Algorithms from Numerical Recipes, Press, et. al., section 6.1,
   Cambridge University Press (1988).
 */

func ln_gamma(z)
/* DOCUMENT ln_gamma(z)
     returns natural log of the gamma function.
     Error is less than 2.e-10 for real part of z>=1.
     Use lngamma if you know that all z>=1, or if you don't care much about
     accuracy for z<1.
   SEE ALSO: lngamma, bico, beta
 */
{
  if (structof(z)==complex) big= (z.re>=1.0);
  else big= (z>=1.0);
  list= where(big);
  if (numberof(list)) lg1= lngamma(z(list));
  list= where(!big);
  if (numberof(list)) {
    z= z(list);
    lg2= log(pi/sin(pi*z)) - lngamma(1.0-z)
  }
  return merge(lg1,lg2,big);
}

func lngamma(x)
/* DOCUMENT lngamma(x)
     returns natural log of the gamma function.
     Error is less than 2.e-10 for real part of x>=1.
     Use ln_gamma if some x<1.
   SEE ALSO: ln_gamma, bico, beta
 */
{
  tmp= x+4.5;
  tmp-= (x-0.5)*log(tmp);
  ser= 1.0 + 76.18009173/x - 86.50532033/(x+1.) + 24.01409822/(x+2.) -
    1.231739516/(x+3.) + 0.120858003e-2/(x+4.) - 0.536382e-5/(x+5.);
  return -tmp+log(2.50662827465*ser);
}

func bico(n,k)
/* DOCUMENT bico(n,k)
     returns the binomial coefficient n!/(k!(n-k)!) as a double.
   SEE ALSO: ln_gamma, beta
 */
{
  return floor(0.5+exp(lngamma(n+1.)-lngamma(k+1.)-lngamma(n-k+1.)));
}

func beta(z,w)
/* DOCUMENT beta(z,w)
     returns the beta function gamma(z)gamma(w)/gamma(z+w)
   SEE ALSO: ln_gamma, bico
 */
{
  return exp(lngamma(z)+lngamma(w)-lngamma(z+w));
}

func erfc(x)
/* DOCUMENT erfc(x)
     returns the complementary error function 1-erf with fractional
     error less than 1.2e-7 everywhere.
 */
{
  z= abs(x);
  t= 1.0/(1.0+0.5*z);
  ans= t*exp(-z*z +
	     poly(t, -1.26551223, 1.00002368, 0.37409196, 0.09678418,
		  -0.18628806, 0.27886807, -1.13520398, 1.48851587,
		  -0.82215223, 0.17087277));
  z= sign(x);
  return ans*z + (1.-z);
}
