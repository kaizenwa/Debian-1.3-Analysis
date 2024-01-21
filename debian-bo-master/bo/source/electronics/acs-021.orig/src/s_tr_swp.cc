/*$Id: s_tr_swp.cc,v 11.28 96/03/03 23:08:30 al Exp $ -*- C++ -*-
 * sweep time and simulate.  output results.
 * manage event queue
 */
#include "constant.h"
#include "error.h"
#include "io.h"
#include "u_opt.h"
#include "u_status.h"
#include "s_tr.h"
#include "declare.h"	/* gen */
/*--------------------------------------------------------------------------*/
//	void	TRANSIENT::sweep(void);
	void	new_event(double);
//	void	TRANSIENT::first(void);
//	bool	TRANSIENT::next(void);
/*--------------------------------------------------------------------------*/
extern const char e_int[];
static double events[MAXEVENTCOUNT];	/* the event queue */
static int nei;		/* next event index */
static int eeq;		/* end of event queue */
static double nexttick;	/* next pre-scheduled user "event" */
/*--------------------------------------------------------------------------*/
void TRANSIENT::sweep(void)
{
  bool converged;
  
  head(tstart, tstop, true/*linear*/, "Time");
  bypass_ok = false;
  inc_mode = BAD;
  if (cont){			/* use the data from last time */
    restore();
  }

  first();
  phase = pINIT_DC;
  genout = gen();
  IO::suppresserrors = trace < tVERBOSE;
  converged = solve(OPT::itl[OPT::DCBIAS],trace);
  if (!converged)
    error(bWARNING, "did not converge\n");
  review();
  if (printnow){
    keep();
    out(time0);
  }
  
  while (next()){
    bypass_ok = false;
    phase = pTRAN;
    genout = gen();
    IO::suppresserrors = trace < tVERBOSE;
    converged = solve(OPT::itl[OPT::TRHIGH],trace);
    review();
    if (trace >= tREJECTED){
      printnow = true;
    }else if (!converged  ||  approxtime <= time0){
      printnow = false;
    }else if (trace >= tALLTIME){
      assert(printnow); /* set in next */
    } // else (usual case) use the value set in next
    if (printnow){
      keep();
      out(time0);
    }
  }
}
/*--------------------------------------------------------------------------*/
void new_event(double etime)
{
  int ii, jj, kk;

  /*STATUS::review.start();*/
  for (ii = nei;  ii != eeq;  ii = (ii+1)%MAXEVENTCOUNT){
    if (etime <= events[ii])
      break;
  }
  for (kk = jj = eeq;  jj != ii;  jj = kk){
    kk = jj - 1;
    if (kk < 0)
      kk += MAXEVENTCOUNT;
    events[jj] = events[kk];
  }
  events[ii] = etime;
  eeq = (eeq+1)%MAXEVENTCOUNT;
  if (eeq == nei)
    error(bERROR, e_int, "event queue overflow");
  if (OPT::foooo == 3){
    printf("(%d,%d) ", nei, eeq);
    for (ii = nei;  ii != eeq;  ii = (ii+1)%MAXEVENTCOUNT)
      printf("%g ", events[ii]);
    printf("\n");
  }
  /*STATUS::review.stop();*/
}
/*--------------------------------------------------------------------------*/
void TRANSIENT::first(void)
{
  /* usually, time0, time1 == 0, from setup */
  STATUS::review.start();
  nexttick = time0 + tstep;		/* set next user step */
  nei = eeq;				/* empty the queue */
  printnow = true;
  stepno = 0;
  STATUS::control[cSTEPCAUSE] = scUSER;
  ++STATUS::control[cSTEPS];
  STATUS::review.stop();
}
/*--------------------------------------------------------------------------*/
bool TRANSIENT::next(void)
{
  STATUS::review.start();
  /* review(tr); */			/* trunc error, etc.*/
 					/* was already done before print */
  double newtime = nexttick;				/* user time steps */
  STATUS::control[cSTEPCAUSE] = scUSER;
  
  if (nei != eeq  &&  events[nei] <= newtime){	/* the event queue */
    newtime = events[nei];
    STATUS::control[cSTEPCAUSE] = scEVENTQ;
  }
 
  if (approxtime < newtime - dtmin){
    if (approxtime < (newtime + time0) / 2.){
      newtime = approxtime;
      STATUS::control[cSTEPCAUSE] = control;
    }else{
      newtime = (newtime + time0) / 2.;
      STATUS::control[cSTEPCAUSE] = scHALF;
    }
  }
  
  if (nexttick < newtime + dtmin){	/* advance user time */
    printnow = true;			/* print if user step */
    stepno++;
    nexttick += tstep;			/* BUG??? :  does the print get */
  }else{				/* messed up if a user step is */
    printnow = trace >= tALLTIME;	/* rejected?? */
  }
  
  while (nei != eeq  &&  events[nei] <= newtime){  /* advance event queue */
    nei = (nei+1)%MAXEVENTCOUNT;
  }
 
  if (newtime < time1 + dtmin){
    error(bDANGER, "very backward time step\n");
    error(bTRACE, "newtime=%e  rejectedtime=%e  oldtime=%e  using=%e\n",
	  newtime, time0, time1, time1 + dtmin);
    newtime = time1 + dtmin;
    STATUS::control[cSTEPCAUSE] += scNO_ADVANCE;
  }
  if (newtime <= time0 - dtmin){
    error(bLOG, "backwards time step\n");
    error(bLOG, "newtime=%e  rejectedtime=%e  oldtime=%e\n",
	  newtime, time0, time1);
    STATUS::control[cSTEPCAUSE] += scREJECT;
    if (newtime < (nexttick - tstep)){
      error(bLOG, "user step rejected\n");
      nexttick -= tstep;
    }
  }else if (newtime < time0 + dtmin){
    error(bWARNING, "zero time step\n");
    error(bLOG, "newtime=%e  rejectedtime=%e  oldtime=%e\n",
	  newtime, time0, time1);
    time1 = time0;
    newtime = time0 + dtmin;
    STATUS::control[cSTEPCAUSE] += scZERO;
  }else{
    time1 = time0;
  }
  
  time0 = newtime;
  ++STATUS::control[cSTEPS];
  STATUS::review.stop();
  return (time0 <= tstop + dtmin);
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
