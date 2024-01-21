/*$Id: s_dc_set.cc,v 11.37 96/03/24 10:09:37 al Exp $ -*- C++ -*-
 * dc analysis setup
 */
#include "ap.h"
#include "e_card.h"
#include "error.h"
#include "io.h"
#include "u_opt.h"
#include "u_cardst.h"
#include "s_dc.h"
/*--------------------------------------------------------------------------*/
//	void	DCOP::finish(void);
//	void	OP::setup(CS&);
//	void	DC::setup(CS&);
//	void	DCOP::options(CS&);
//	void	DCOP::by(CS&);
//	void	DCOP::decade(CS&);
//	void	DCOP::times(CS&);
/*--------------------------------------------------------------------------*/
static DCOP *This;	/* latest parameters for arg passing and "finish"   */
static CARDSTASH stash[DCNEST]; /* store std values of elements being swept */
/*--------------------------------------------------------------------------*/
void DCOP::finish(void)
{
  assert(This);
  assert(this == This);
  if (This){
    for (int ii = 0;  ii < DCNEST && exists(This->zap[ii]);  ii++){
      stash[ii].restore();
      This->zap[ii]->decprobes();
      This->zap[ii]->expand();
    }
    This = NULL;
  }
}
/*--------------------------------------------------------------------------*/
void OP::setup(CS& cmd)
{
  zap[0] = (CARD*)NULL;
  sweepval[0] = &temp;
  start[0] = (cmd.is_float()) ? cmd.ctof()-ABS_ZERO : OPT::tempamb;
  stop[0]  = (cmd.is_float()) ? cmd.ctof()-ABS_ZERO : start[0];
  step[0] = 0.;
  genout = 0.;
  options(cmd);
  if (step[0] == 0.){
    step[0] = stop[0] - start[0];
    linswp[0] = true;
  }
}
/*--------------------------------------------------------------------------*/
void DC::setup(CS& cmd)
{
  CARD *target;

  target = findbranch(cmd, CARD::first(), CARD::last());
  if (exists(target)){			/* sweep a component */
    zap[0] = target;
  }else if (cmd.is_float()){		/* sweep the generator */
    zap[0] = (CARD*)NULL;
  }					/* else leave it alone */
  
  if (cmd.is_float()){			/* set up parameters */
    start[0] = cmd.ctof();
    stop[0] = (cmd.is_float()) ? cmd.ctof() : start[0];
    step[0] = 0.;
  }					/* else leave it alone */
  
  if (exists(zap[0])){
    stash[0] = zap[0];			/* stash the std value */
    zap[0]->incprobes();		/* we need to keep track of it */
    zap[0]->x = (generic_t*)NULL;	/* zap out the extensions */
    sweepval[0] = &(zap[0]->val); 	/* point to value to patch */
  }else{
    sweepval[0] = &genout;		/* point to value to patch */
  }
  
  genout = 0.;
  temp = OPT::tempamb;
  options(cmd);
  if (step[0] == 0.){
    step[0] = stop[0] - start[0];
    linswp[0] = true;
  }
}
/*--------------------------------------------------------------------------*/
void DCOP::options(CS& cmd)
{
  This = this;
  IO::where |= IO::mstdout;
  alarmlist = PROBE_LISTS::alarm[mode];
  plotlist  = PROBE_LISTS::plot[mode];
  printlist = PROBE_LISTS::print[mode];
  storelist = PROBE_LISTS::store[mode];
  IO::ploton = IO::plotset  &&  plotlist.count() > 0;
  uic = loop = reverse = cont = false;
  trace = tNONE;
  cmd.stuck();
  do{
    if      (cmd.is_float())       by(cmd);
    else if (cmd.pmatch("*$$"))    times(cmd);
    else if (cmd.pmatch("By"))     by(cmd);
    else if (cmd.pmatch("Decade")) decade(cmd);
    else if (cmd.pmatch("TImes"))  times(cmd);
    cmd.get("Ambient",	&temp,	     mOFFSET, OPT::tempamb);
    cmd.set("Continue", &cont, true);
    cmd.set("LOop", 	&loop, true);
    cmd.set("NOPlot",	&IO::ploton, false);
    cmd.set("PLot",	&IO::ploton, true);
    cmd.get("Reftemp",	&temp,	     mOFFSET, OPT::tnom);
    cmd.set("REverse",	&reverse, true);
    cmd.get("Temperature", &temp,    mOFFSET, -ABS_ZERO);
    cmd.get("TRace",	(int*)&trace);
    set(cmd,"WAtch",	&trace,      tITERATION);
    outset(cmd,"","");
  }while (cmd.more() && !cmd.stuck());
  cmd.check(bWARNING);

  initio(IO::where,(FILE*)NULL);
}
/*--------------------------------------------------------------------------*/
void DCOP::by(CS& cmd)
{
  step[0] = cmd.ctof();
  linswp[0] = true;
  if (step[0] == 0.)
    step[0] = stop[0] - start[0];
}
/*--------------------------------------------------------------------------*/
void DCOP::decade(CS& cmd)
{
  double junk = cmd.ctopf();
  if (junk == 0.)
    junk = 1.;
  junk = pow(10., 1./junk);
  step[0] = junk;
  linswp[0] = false;
}
/*--------------------------------------------------------------------------*/
void DCOP::times(CS& cmd)
{
  step[0] = cmd.ctopf();
  linswp[0] = false;
  if (step[0] == 0.  &&  start[0] != 0.)
    step[0] = stop[0] / start[0];
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
