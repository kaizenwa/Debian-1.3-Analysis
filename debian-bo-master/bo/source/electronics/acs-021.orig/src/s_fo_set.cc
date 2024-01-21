/*$Id: s_fo_set.cc,v 11.28 96/03/03 23:08:26 al Exp $ -*- C++ -*-
 * set up  fourier analysis
 */
#include "ap.h"
#include "error.h"
#include "u_opt.h"
#include "s_fo.h"
#include "l_compar.h"
/*--------------------------------------------------------------------------*/
//	void	FOURIER::setup(CS&);
//	void	FOURIER::fftallocate();
//	void	FOURIER::fftunallocate();
static	int	to_pow_of_2(double);
/*--------------------------------------------------------------------------*/
/* fo_setup: fourier analysis: parse command string and set options
 * 	(options set by call to TRANSIENT::options)
 */
void FOURIER::setup(CS& cmd)
{
  cont = true;
  if (cmd.is_pfloat()){
    double arg1,arg2,arg3;
    arg1 = cmd.ctopf();
    arg2 = cmd.ctopf();
    arg3 = cmd.ctopf();
    if (arg3 != 0.){			    /* 3 args: all */
      fstart = arg1;
      fstop  = arg2;
      fstep  = arg3;
    }else if (arg2 != 0.){		    /* 2 args: start = 0 */
      if (arg1 >= arg2){		    /* 2 args: stop, step */
	fstart = 0.;		    	    /* 	(stop > step) */
	fstop  = arg1;
	fstep  = arg2;
      }else{ /* arg1 < arg2 */		    /* 2 args: step, stop */
	fstart = 0.;
	fstop  = arg2;
	fstep  = arg1;
      }
    }else if (arg1 == 0.){		    /* 1 arg: start */
      fstart = 0.;
      /* fstop unchanged */
      /* fstep unchanged */
    }else{				    /* 1 arg: step */
      fstart = 0.;
      fstop  = OPT::harmonics * arg1;
      fstep  = arg1;
    }
  }
  /* else (no args) : no change */
  
  if (fstep == 0.){
    error(bERROR, "frequency step = 0\n");
  }
  if (fstop == 0.){
    fstop = OPT::harmonics * fstep;
  }
  options(cmd);
  
  timesteps = to_pow_of_2(fstop*2 / fstep) + 1;
  if (cold){
    cont = false;
    tstart = 0.;
  }else{
    cont = true;
    tstart = last_time;
  }
  tstop = tstart + 1. / fstep;
  tstep = 1. / fstep / (timesteps-1);
  time1 = time0 = tstart;
  dtmax = tstep / (double)(skip);
  dtmin = max(dtmin,dtmax/dtratio);
}
/*--------------------------------------------------------------------------*/
/* allocate:  allocate space for fft
 */
void FOURIER::fftallocate()
{
  int probs = printlist.count();
  fdata = new COMPLEX*[probs];
  for (int ii = 0;  ii < probs; ii++){
    fdata[ii] = new COMPLEX[timesteps];
  }
}
/*--------------------------------------------------------------------------*/
/* unallocate:  unallocate space for fft
 */
void FOURIER::fftunallocate()
{
  for (int ii = 0;  ii < printlist.count(); ii++){
    delete fdata[ii];
  }
  delete fdata;
  fdata = NULL;
}
/*--------------------------------------------------------------------------*/
/* to_pow_of_2: round up to nearest power of 2
 * example: z=92 returns 128
 */
static int to_pow_of_2(double z)
{
  long x,y;
  x = (long)floor(z);
  for (y = 1; x > 0; x >>= 1)
    y <<= 1;
  return y;
}   
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
