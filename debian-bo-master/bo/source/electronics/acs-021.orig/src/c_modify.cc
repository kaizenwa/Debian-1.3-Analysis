/*$Id: c_modify.cc,v 11.38 96/03/24 17:59:19 al Exp $ -*- C++ -*-
 */
#include "ap.h"
#include "e_card.h"
#include "error.h"
#include "io.h"
#include "l_lib.h"
#include "c_comand.h"
#include "u_cardst.h"
#include "s__.h"
#include "l_compar.h"
/*--------------------------------------------------------------------------*/
//	void	CMD::modify(CS&);
//	void	CMD::fault(CS&);
static	void	modify_fault(CS&,int);
static	double	sweep_fix(CS&,const CARD*);
static	bool	faultbranch(CARD*,double);
//	void	CMD::restore(CS&);
//	void	CMD::unfault(CS&);
/*--------------------------------------------------------------------------*/
extern const char e_int[];
extern const bool crtplot;
extern const int swp_type[];
extern const int swp_count[], swp_steps[];
extern const int swp_nest;
static CARDSTASH faultlist[MAXFAULTS];
static unsigned faultcount = 0;
#define MODIFY 1
#define FAULT 0
/*--------------------------------------------------------------------------*/
void CMD::modify(CS& cmd)
{
  modify_fault(cmd,MODIFY);
}
/*--------------------------------------------------------------------------*/
void CMD::fault(CS& cmd)
{
  modify_fault(cmd,FAULT);
}
/*--------------------------------------------------------------------------*/
static void modify_fault(CS& cmd, int command)
{
  CARD *brh;
  double value;

  SIM::uninit();
  while (cmd.is_alpha()){    
    int mark = cmd.cursor();
    int cmax = cmd.cursor();
    brh = CARD::first();
    for (;;){
      cmd.reset(mark);
      brh = findbranch(cmd, brh, CARD::root());
      cmax = max(cmax, cmd.cursor());
      if (!exists(brh))
	break;
      if (command == MODIFY){
	brh->setvalue(cmd.ctof());
      }else{ /* command == FAULT */
	value = sweep_fix(cmd,brh);
	if (!faultbranch(brh,value))
	  error(bWARNING, e_int, "fault");
      }
      brh = brh->next();
    }
    cmd.reset(cmax);
    if (mark == cmax){
      cmd.check(bWARNING);
      cmd.skiparg();
    }
  }
}   
/*--------------------------------------------------------------------------*/
/* sweep_fix: fix the value for sweep command.
 * (find value by interpolation)
 * if not sweeping, return "start" (the first arg).
 */
static double sweep_fix(CS& cmd, const CARD *brh)
{
  double start = cmd.ctof();
  double value = start;
  if (swp_steps[swp_nest] != 0   &&   cmd.is_float()){
    double last = cmd.ctof();
    double offset = (double)swp_count[swp_nest] / (double)swp_steps[swp_nest];
    if (swp_type[swp_nest]=='L'){
      if (start == 0.){
	error(bERROR, "log sweep can't pass zero\n");
	value = 0;
      }else{
	value = start * pow( (last/start), offset );
      }
    }else{
      value = start + (last-start) * offset;
    }
    if (!crtplot)
      mprintf( IO::mstdout, "%u> sweep %s =%s\n",
	      swp_count[swp_nest]+1,
	      brh->printlabel(),
	      ftos(value,"             ",7,0)
	      );
  }
  return value;
}
/*--------------------------------------------------------------------------*/
/* faultbranch: "fault" a single branch. (temporarily change a value)
 * save the existing info in "faultlist", then patch
 */
static bool faultbranch(CARD *brh, double value)
{
  if (!brh->isdevice() || brh->sub() || faultcount >= MAXFAULTS){
    return false;
  }else{
    faultlist[faultcount++] = brh;
    brh->setvaluex(value);
    return true;
  }
}
/*--------------------------------------------------------------------------*/
void CMD::restore(CS& cmd)
{
  unfault(cmd);
  unmark(cmd);
}
/*--------------------------------------------------------------------------*/
/* CMD::unfault: (command) remove faults and restore pre-fault values
 */
void CMD::unfault(CS&)
{
  while (faultcount > 0){
    faultlist[--faultcount].restore();
  }
  assert(faultcount == 0);
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
