/*$Id: s_tr_set.cc,v 11.28 96/03/03 23:08:29 al Exp $ -*- C++ -*-
 * set up transient and fourier analysis
 */
#include "constant.h"
#include "ap.h"
#include "error.h"
#include "io.h"
#include "u_opt.h"
#include "s_tr.h"
#include "l_compar.h"
/*--------------------------------------------------------------------------*/
//	void	TRANSIENT::setup(CS&);
//	void	TRANSIENT::options(CS&);
/*--------------------------------------------------------------------------*/
/* tr_setup: transient analysis: parse command string and set options
 * 	(options set by call to tr_options)
 */
void TRANSIENT::setup(CS& cmd)
{
  double oldrange;
  
  oldrange = tstop - tstart;
  
  cont = true;
  if (cmd.is_pfloat()){
    double arg1,arg2,arg3;
    arg1 = cmd.ctopf();
    arg2 = cmd.ctopf();
    arg3 = cmd.ctopf();
    if (arg3 != 0.){			    /* 3 args: all */
      if (arg1 == 0.  ||  arg1 > arg3){     /* eca (logical) order: */
	tstart = arg1;			    /* tstart tstop step */
	tstop  = arg2;				
	tstep  = arg3;
      }else{ 				    /* spice (illogical) order */
	tstart = arg3;		    	    /* tstep tstop tstart */
	tstop  = arg2;
	tstep  = arg1;
      }
    }else if (arg2 != 0.){		    /* 2 args */
      if (arg1 == 0.){			    /* 2 args: tstart, tstop */
	tstart = arg1;
	tstop  = arg2;
	/* tstep unchanged */
      }else if (arg1 >= arg2){		    /* 2 args: tstop, tstep */
	tstart = last_time;
	tstop  = arg1;
	tstep  = arg2;
      }else{ /* arg1 < arg2 */		    /* 2 args: tstep, tstop */
	tstart = arg3; /* 0 */	   	    /* spice order */
	tstop  = arg2;
	tstep  = arg1;
      }
    }else if (arg1 > last_time){	    /* 1 arg: tstop */
      tstart = last_time;
      tstop  = arg1;
      /* tstep unchanged */
    }else if (arg1 == 0.){		    /* 1 arg: tstart */
      tstart = 0.;
      tstop  = oldrange;
      /* tstep unchanged */
    }else{ /* arg1 < last_time, but not 0 */  /* 1 arg: tstep */
      tstart = last_time;
      tstop  = last_time + oldrange;
      tstep  = arg1;
    }
  }else{ /* no args */
    tstart = last_time;
    tstop  = last_time + oldrange;
    /* tstep unchanged */
  }
  if (cmd.is_pfloat()){
    dtmax = fabs(cmd.ctof());
    skip = (int)(tstep / dtmax + .5);
  }
  options(cmd);
  
  dtmax = tstep / (double)(skip);
  dtmin = max(dtmin,dtmax/dtratio);
  if  (tstart < last_time  ||  last_time <= 0.){
    cont = false;
    time1 = time0 = 0.;
  }else{
    cont = true;
    time1 = time0 = last_time;
  }
  if (tstep==0.)
    error(bERROR, "time step = 0\n");
}
/*--------------------------------------------------------------------------*/
/* tr_options: set options common to transient and fourier analysis
 */
void TRANSIENT::options(CS& cmd)
{
  IO::where |= IO::mstdout;
  temp = OPT::tempamb;
  alarmlist = PROBE_LISTS::alarm[mode];
  plotlist  = PROBE_LISTS::plot[mode];
  printlist = PROBE_LISTS::print[mode];
  storelist = PROBE_LISTS::store[mode];
  IO::ploton = IO::plotset  &&  plotlist.count() > 0;
  uic = echo = cold = false;
  trace = tNONE;
  dtmin = OPT::dtmin;
  dtratio = OPT::dtratio;
  cmd.stuck();
  do{
    set(cmd,"ALl",	&trace,      tALLTIME);
    cmd.get("Ambient",	&temp,	     mOFFSET, OPT::tempamb);
    cmd.set("Cold",	&cold,	     true);
    cmd.get("DTMIn",	&dtmin,	     mPOSITIVE);
    cmd.get("DTRatio",	&dtratio,    mPOSITIVE);
    cmd.set("Echo",	&echo,	     true);
    cmd.set("NOPlot",	&IO::ploton, false);
    cmd.set("PLot",	&IO::ploton, true);
    cmd.get("Reftemp",	&temp,	     mOFFSET, OPT::tnom);
    cmd.get("SKip",	&skip);
    cmd.get("Temperature", &temp,    mOFFSET, -ABS_ZERO);
    cmd.get("TRace",	(int*)&trace);
    cmd.set("UIC",	&uic,	     true);
    set(cmd,"WAtch",	&trace,      tITERATION);
    outset(cmd,"","");
  }while (cmd.more() && !cmd.stuck());
  cmd.check(bWARNING);

  if (skip < 1)
    skip = 1;
  initio(IO::where,IO::whence);
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
