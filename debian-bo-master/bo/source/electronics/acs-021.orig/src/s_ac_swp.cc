/*$Id: s_ac_swp.cc,v 11.28 96/03/03 23:08:21 al Exp $ -*- C++ -*-
 * ac sweep control
 */
#include "constant.h"
#include "l_compar.h"
#include "io.h"
#include "s_ac.h"
/*--------------------------------------------------------------------------*/
//	void	AC::sweep();
//	void	AC::first();
//	bool	AC::next();
/*--------------------------------------------------------------------------*/
void AC::sweep()
{
  head(start, stop, linswp, "Freq");
  first();
  do {
    jomega = COMPLEX(0., freq * kPIx2);
    solve();
    out(freq);
  } while (next());
}
/*--------------------------------------------------------------------------*/
void AC::first()
{
  freq = start;
}
/*--------------------------------------------------------------------------*/
bool AC::next()
{
  double realstop;

  if (IO::whence)
    return true;
  realstop = (linswp)
    ? stop - step/100.
    : stop / pow(step,.01);
  if (!inorder(start, freq, realstop))
    return false;
  freq = (linswp)
    ? freq + step
    : freq * step;
  if (inorder(freq, start, stop))
    return false;
  return true;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
