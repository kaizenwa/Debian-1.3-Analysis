/*$Id: s_ac_set.cc,v 11.37 96/03/24 10:09:34 al Exp $ -*- C++ -*-
 * ac analysis setup
 */
#include "constant.h"
#include "ap.h"
#include "error.h"
#include "io.h"
#include "u_opt.h"
#include "s_ac.h"
/*--------------------------------------------------------------------------*/
//	void	AC::setup(CS&);
//	void	AC::by(CS&);
//	void	AC::decade(CS&);
//	void	AC::lin(CS&);
//	void	AC::octave(CS&);
//	void	AC::times(CS&);
/*--------------------------------------------------------------------------*/
static int needslinfix;	// flag: lin option needs patch later (spice compat)
/*--------------------------------------------------------------------------*/
void AC::setup(CS& cmd)
{
  IO::where |= IO::mstdout;
  temp = OPT::tempamb;
  alarmlist = PROBE_LISTS::alarm[mode];
  plotlist  = PROBE_LISTS::plot[mode];
  printlist = PROBE_LISTS::print[mode];
  storelist = PROBE_LISTS::store[mode];
  IO::ploton = IO::plotset  &&  plotlist.count() > 0;
  cold = cont = echo = false;

       if (cmd.pmatch("*$$"))    times(cmd);
  else if (cmd.pmatch("By"))     by(cmd);
  else if (cmd.pmatch("Decade")) decade(cmd);
  else if (cmd.pmatch("TImes"))  times(cmd);
  else if (cmd.pmatch("LIn"))	 lin(cmd);
  else if (cmd.pmatch("Octave")) octave(cmd);
  
  if (cmd.is_float()){
    start = cmd.ctof();
    stop  = cmd.ctof();
    if (stop==0.)
      stop = start;
    if (cmd.is_float())
      by(cmd);
  }

  cmd.stuck();
  do{
         if (cmd.pmatch("*$"))     times(cmd);
    else if (cmd.pmatch("By"))     by(cmd);
    else if (cmd.pmatch("Decade")) decade(cmd);
    else if (cmd.pmatch("TImes"))  times(cmd);
    else if (cmd.pmatch("LIn"))	   lin(cmd);
    else if (cmd.pmatch("Octave")) octave(cmd);
    cmd.get("Ambient",	&temp,	     mOFFSET, OPT::tempamb);
    cmd.set("Cold",	&cold,	     true);
    cmd.set("CONTinue",	&cont,	     true);
    cmd.set("Echo",	&echo,	     true);
    cmd.set("NOPlot",	&IO::ploton, false);
    cmd.set("PLot",	&IO::ploton, true);
    cmd.get("Reftemp",	&temp,	     mOFFSET, OPT::tnom);
    cmd.get("Temperature", &temp,    mOFFSET, -ABS_ZERO);
    outset(cmd,"","");
  }while (cmd.more() && !cmd.stuck());
  cmd.check(bWARNING);

  initio(IO::where,IO::whence);
  if (needslinfix){			    // LIN option is # of points.
    step=(stop-start)/(step-1.);	    // Must compute step after 
    needslinfix = false;		    // reading start and stop,
  }					    // but step must be read first
  if (step==0.){			    // for Spice compatibility
    step = stop - start;
    linswp = true;
  }
}
/*--------------------------------------------------------------------------*/
void AC::by(CS& cmd)
{
  step = cmd.ctof();
  needslinfix = false;
  linswp = true;
}
/*--------------------------------------------------------------------------*/
void AC::decade(CS& cmd)
{
  step = cmd.ctopf();
  if (step == 0.)
    step = 1.;
  step = pow(10., 1./step);
  needslinfix = false;
  linswp = false;
}
/*--------------------------------------------------------------------------*/
void AC::lin(CS& cmd)
{
  step = cmd.ctopf();		// need to fix step, later
  if (step == 0.)		// do it at the end of setup
    step = 1.;			// a kluge, but this is a patch
  needslinfix = true;		// and I am too lazy to do it
  linswp = true;		// right.
}
/*--------------------------------------------------------------------------*/
void AC::octave(CS& cmd)
{
  step = cmd.ctopf();
  if (step == 0.)
    step = 1.;
  step = pow(2.00000001, 1./step);
  needslinfix = false;
  linswp = false;
}
/*--------------------------------------------------------------------------*/
void AC::times(CS& cmd)
{
  step = cmd.ctopf();
  if (step == 0.   &&   start != 0.)
    step = stop / start;
  needslinfix = false;
  linswp = false;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
