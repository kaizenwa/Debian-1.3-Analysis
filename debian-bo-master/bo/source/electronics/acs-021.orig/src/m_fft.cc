/*$Id: m_fft.cc,v 11.22 96/02/18 11:44:13 al Exp $ -*- C++ -*-
 * fast fourier transform
 */
#include "md.h"
#include "constant.h"
#include "declare.h"	/* self */
/*--------------------------------------------------------------------------*/
	void	fft(COMPLEX*,int,int);
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
void fft(COMPLEX *x, int n, int inv)
{
  int s = (inv) ? 1 : -1;
  int nxp, nxp2;
  for (nxp=n;  (nxp2=nxp/2) > 0;  nxp=nxp2){
    double wpwr = kPIx2 / nxp;
    int m;
    for (m = 0;  m < nxp2;  m++){
      double argg = m * wpwr;
      COMPLEX w = COMPLEX(cos(argg), s*sin(argg));
      int jj1;
      for (jj1 = m;  jj1+nxp-m <= n;  jj1 += nxp){
	int jj2 = jj1 + nxp2;
	COMPLEX t = x[jj1] - x[jj2];
	x[jj1] += x[jj2];
	x[jj2] = t * w;
      }
    }
  }
  /* unscramble */
  {
    int i, j, k;
    for (k = i = j = 0;  i < n-1;  i++, j += k){
      if (i < j){
	COMPLEX t = x[j];
	x[j] = x[i];
	x[i] = t;
      }
      for (k = n/2;  k <= j;  k /= 2)
	j -= k;
    }
  }
  /* fix level */
  if (!inv){
    for (int i = 0;  i < n;  i++){
      x[i] /= n;
    }
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
