/*$Id: c_genrat.cc,v 11.22 96/02/18 11:42:07 al Exp $ -*- C++ -*-
 * set up generator for transient analysis
 */
#include "constant.h"
#include "ap.h"
#include "error.h"
#include "io.h"
#include "l_lib.h"
#include "s__.h"
#include "c_comand.h"
/*--------------------------------------------------------------------------*/
//	void	CMD::generator(CS&);
	double	gen();
/*--------------------------------------------------------------------------*/
static double freq = 0;
static double ampl = 1;
static double phaz = 0.;
static double maxv = 1.;
static double minv = 0.;
static double offset = 0.;
static double init = 0.;
static double rise = 1e-12;
static double fall = 1e-12;
static double delay = 0.;
static double width = 0.;
static double period = 0.;
/*--------------------------------------------------------------------------*/
void CMD::generator(CS& cmd)
{
  int where = (cmd.more())  ?  0  :  IO::mstdout;
  cmd.stuck();
  do{
    cmd.get("Freq",   &freq,	mPOSITIVE);
    cmd.get("Ampl",   &ampl);
    cmd.get("Phase",  &phaz);
    cmd.get("MAx",    &maxv);
    cmd.get("MIn",    &minv);
    cmd.get("Offset", &offset);
    cmd.get("Init",   &init);
    cmd.get("Rise",   &rise,	mPOSITIVE);
    cmd.get("Fall",   &fall,	mPOSITIVE);
    cmd.get("Delay",  &delay,	mPOSITIVE);
    cmd.get("Width",  &width,	mPOSITIVE);
    cmd.get("PEriod", &period,	mPOSITIVE);
  }while (cmd.more() && !cmd.stuck());
  cmd.check(bWARNING);

  mprintf(where, " freq=%s ",	ftos(freq,   "", 7, 0));
  mprintf(where, " ampl=%s ",	ftos(ampl,   "", 7, 0));
  mprintf(where, " phase=%s ",	ftos(phaz,   "", 7, 0));
  mprintf(where, " max=%s ",	ftos(maxv,   "", 7, 0));
  mprintf(where, " min=%s ", 	ftos(minv,   "", 7, 0));
  mprintf(where, " offset=%s ",	ftos(offset, "", 7, 0));
  mprintf(where, " init=%s ",	ftos(init,   "", 7, 0));
  mprintf(where, " rise=%s ",	ftos(rise,   "", 7, 0));
  mprintf(where, " fall=%s ",	ftos(fall,   "", 7, 0));
  mprintf(where, " delay=%s ",	ftos(delay,  "", 7, 0));
  mprintf(where, " width=%s ",	ftos(width,  "", 7, 0));
  mprintf(where, " period=%s ",	ftos(period, "", 7, 0));
  mprintf(where, "\n" );
}
/*--------------------------------------------------------------------------*/
double gen()
{
  double loctime;
  double level;
  
  if (SIM::time0 <= delay)
    return init;
  loctime = SIM::time0 - delay;
  if (period > 0.){
    loctime = fmod(loctime, period);
  }
  if (SIM::time0 <= delay + rise)                     /* initial rise */
    level = (maxv - 0) * (loctime/rise) + 0;
  else if (loctime <= rise)                           /* rising       */
    level = (maxv - minv) * (loctime/rise) + minv;
  else if (width==0.  ||  (loctime-=rise) <= width)   /* pulse on     */
    level = maxv;
  else if ((loctime-=width) <= fall)                  /* falling      */
    level = (minv - maxv) * (loctime/fall) + maxv;
  else                                                /* pulse off    */
    level = minv;
  level *= (freq == 0.) 
    ? ampl
    : ampl * sin(kPIx2*freq*(SIM::time0-delay) + phaz*DTOR);
  return (SIM::time0 <= delay + rise)
    ? level + (offset - init) * (loctime/rise) + init
    : level + offset;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
