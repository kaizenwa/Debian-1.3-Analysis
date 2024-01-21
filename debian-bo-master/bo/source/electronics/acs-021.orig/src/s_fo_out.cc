/*$Id: s_fo_out.cc,v 11.24 96/02/25 14:10:46 al Exp $ -*- C++ -*-
 * functions needed in addition to the transient analysis
 * to perform the fourier command.
 */
#include "constant.h"
#include "io.h"
#include "l_lib.h"
#include "u_probe.h"
#include "s_fo.h"
#include "declare.h"	/* plclose, plclear, fft */
/*--------------------------------------------------------------------------*/
//	void	FOURIER::store();
//	void	FOURIER::foout();
//	void	FOURIER::fohead(int);
//	void	FOURIER::foprint(COMPLEX*);
static  int	stepnum(double,double,double);
static	COMPLEX	find_max(COMPLEX*,int,int);
static	double  phaze(COMPLEX);
static	double  db(COMPLEX);
/*--------------------------------------------------------------------------*/
/* store: stash time domain data in preparation for Fourier Transform
 */
void FOURIER::store()
{
  for (int ii = 0;  ii < printlist.count();  ii++){
    fdata[ii][stepno] = printlist[ii].value();
  }
}
/*--------------------------------------------------------------------------*/
/* foout:  print out the results of the transform
 */
void FOURIER::foout()
{
  plclose();
  plclear();
  for (int ii = 0;  ii < printlist.count();  ii++){
    fohead(ii);
    fft(fdata[ii], timesteps-1,  0);
    foprint(fdata[ii]);
  }
}
/*--------------------------------------------------------------------------*/
/* fo_head: print output header
 * arg is index into probe array, to select probe name
 */
void FOURIER::fohead(int prob)
{
 mprintf(IO::where,"# %-10s", printlist[prob].label());
 mprintf(IO::where,"--------- actual ---------  -------- relative --------\n");
 mprintf(IO::where,"#freq       ");
 mprintf(IO::where,"value        dB      phase  value        dB      phase\n");
}
/*--------------------------------------------------------------------------*/
/* fo_print: print results of fourier analysis
 * for all points at single probe
 */
void FOURIER::foprint(COMPLEX *data)
{
  int startstep = stepnum(0., fstep, fstart);
  int stopstep  = stepnum(0., fstep, fstop );
  COMPLEX maxvalue = find_max(data,max(1,startstep),stopstep);
  if (maxvalue == 0.)
    maxvalue = 1.;
  data[0] /= 2;
  for (int ii = startstep;  ii <= stopstep;  ++ii){
    double frequency = fstep * ii;
    COMPLEX unscaled = data[ii];
    COMPLEX scaled = unscaled / maxvalue;
    unscaled *= 2;
    mprintf(IO::where, "%s%s%7.2f %8.3f %s%7.2f %8.3f\n",
	    ftos(frequency,    "           ",5,IO::formaat),
	    ftos(abs(unscaled),"           ",5,IO::formaat),
	    db(unscaled),
	    phaze(unscaled*COMPLEX(0.,1)),
	    ftos(abs(scaled),  "           ",5,IO::formaat),
	    db(scaled),
	    phaze(scaled) ) ;
  }
}
/*--------------------------------------------------------------------------*/
/* stepnum: return step number given its frequency or time
 */
static int stepnum(double start, double step, double here)
{
  return int((here-start)/step + .5);
}
/*--------------------------------------------------------------------------*/
/* find_max: find the max magnitude in a COMPLEX array
 */
static COMPLEX find_max(COMPLEX *data, int start, int stop)
{
  COMPLEX maxvalue = 0.;
  for (int ii = start;  ii <= stop;  ii++){
    if (abs(data[ii]) > abs(maxvalue)){
      maxvalue = data[ii];
    }
  }
  return maxvalue;
}
/*--------------------------------------------------------------------------*/
/* phaze: extract phase (degrees) from COMPLEX number
 * rotates 90 degrees!  (ref to sine instead of cosine)
 */
static double phaze(COMPLEX x)
{
  return arg(x)* RTOD;
}
/*--------------------------------------------------------------------------*/
static double db(COMPLEX value)
{
  return  20. * log10(max(abs(value),VOLTMIN));
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
