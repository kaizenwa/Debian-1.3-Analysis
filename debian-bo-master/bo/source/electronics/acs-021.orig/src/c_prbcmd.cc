/*$Id: c_prbcmd.cc,v 11.28 96/03/03 23:07:00 al Exp $ -*- C++ -*-
 * probe and plot commands
 * set up print and plot (select points, maintain probe lists)
 * command line operations
 */
#include "ap.h"
#include "error.h"
#include "io.h"
#include "c_comand.h"
#include "u_probe.h"
#include "u_prblst.h"
#include "s__.h"
/*--------------------------------------------------------------------------*/
//	void	CMD::alarm(CS&);
//	void	CMD::plot(CS&);
//	void	CMD::print(CS&);
static	void	do_probe(CS&,PROBELIST*);
/*--------------------------------------------------------------------------*/
PROBELIST PROBE_LISTS::alarm[sCOUNT]; /* list of alarm points */
PROBELIST PROBE_LISTS::plot[sCOUNT];  /* list of plot points */
PROBELIST PROBE_LISTS::print[sCOUNT]; /* list of print points */
PROBELIST PROBE_LISTS::store[sCOUNT]; /* list of pts to store for postproc */
/*--------------------------------------------------------------------------*/
void CMD::alarm(CS& cmd)
{
  do_probe(cmd,PROBE_LISTS::alarm);
}
/*--------------------------------------------------------------------------*/
void CMD::plot(CS& cmd)
{
  IO::plotset = true;
  do_probe(cmd,PROBE_LISTS::plot);
}
/*--------------------------------------------------------------------------*/
void CMD::print(CS& cmd)
{
  IO::plotset = false;
  do_probe(cmd,PROBE_LISTS::print);
}
/*--------------------------------------------------------------------------*/
static void do_probe(CS& cmd, PROBELIST *probes)
{
  enum {ADD, DELETE, NEW} action;
  int simtype = sNONE;

  if (cmd.match('-')){		/* handle .probe - ac ...... */
    action = DELETE;		/* etc. 		     */
    cmd.skip();
  }else if (cmd.match('+')){
    action = ADD;
    cmd.skip();
  }else{			/* no -/+ means clear, but wait for */
    action = NEW;		/* .probe ac + ..... 		    */
  }				/* which will not clear first	    */

  /**/ cmd.set("TRan",	  &simtype,	sTRAN)
    || cmd.set("AC",	  &simtype,	sAC)
    || cmd.set("DC",	  &simtype,	sDC)
    || cmd.set("OP",	  &simtype,	sOP)
    || cmd.set("FOurier", &simtype,	sFOURIER);

  if (!simtype){			/* must be all simtypes */
    if (cmd.end()){				/* list all */
      probes[sTRAN].list("tran");
      probes[sAC].list("ac");
      probes[sDC].list("dc");
      probes[sOP].list("op");
      probes[sFOURIER].list("fourier");
    }else if (cmd.pmatch("CLEAR")){		/* clear all */
      for (int ii = sSTART;  ii < sCOUNT;  ++ii)
	probes[ii].clear();
    }else{					/* error */
      cmd.warn(bERROR);
    }
  }else{
    if (cmd.end()){			/* list */
      probes[simtype].list("");
    }else if (cmd.pmatch("CLEAR")){	/* clear */
      probes[simtype].clear();
    }else{				/* add/remove */
      SIM::init();
      if (cmd.match('-')){			/* setup cases like: */
	action = DELETE;			/* .probe ac + ....  */
	cmd.skip();
      }else if (cmd.match('+')){
	action = ADD;
	cmd.skip();
      }
      if (action == NEW){			/* no +/- here or at beg. */
	probes[simtype].clear();		/* means clear first	  */
	action = ADD;
      }
      while (cmd.more()){			/* do-it */
	if (cmd.match('-')){			/* handle cases like:	    */
	  action = DELETE;			/* .pr ac +v(7) -e(6) +r(8) */
	  cmd.skip();
	}else if (cmd.match('+')){
	  action = ADD;
	  cmd.skip();
	}
	if (action == DELETE){
	  probes[simtype] -= cmd;
	}else{
	  probes[simtype] += cmd;
	}
      }
    }
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
