/*$Id: s_dc_swp.cc,v 11.28 96/03/03 23:08:24 al Exp $ -*- C++ -*-
 * dc analysis sweep
 */
#include "error.h"
#include "io.h"
#include "e_card.h"
#include "u_opt.h"
#include "u_status.h"
#include "s_dc.h"
#include "l_compar.h"
/*--------------------------------------------------------------------------*/
//	void	DCOP::sweep(void);
//	void	DCOP::first(int);
//	bool	DCOP::next(int);
/*--------------------------------------------------------------------------*/
void DCOP::sweep(void)
{
  int converged;
  int itl = OPT::itl[OPT::DCBIAS];
  
  head(start[0], stop[0], linswp[0], " ");
  bypass_ok = false;
  inc_mode = BAD;
  if (cont){
    restore();
  }
  
  first(0);
  do {
    IO::suppresserrors = trace < tVERBOSE;
    converged = solve(itl,trace);
    if (!converged)
      error(bWARNING, "did not converge\n");
    keep();
    out(*sweepval[0]);
    itl = OPT::itl[OPT::DCXFER];
  } while (next(0));
}
/*--------------------------------------------------------------------------*/
void DCOP::first(int ii)
{
  *sweepval[ii] = start[ii];
  if (exists(zap[ii]))
    zap[ii]->expand();
  
  if (reverse){
    reverse = false;
    while (next(ii))
      /* nothing */;
    reverse = true;
    next(ii);
  }
  STATUS::control[cSTEPCAUSE] = scUSER;
}
/*--------------------------------------------------------------------------*/
bool DCOP::next(int ii)
{
  bool ok = false;
  if (linswp[ii]){
    double fudge = step[ii] / 10.;
    if (step[ii] == 0.){
      ok = false;
    }else{
      if (!reverse){
	*(sweepval[ii]) += step[ii];
	ok=inorder(start[ii]-fudge,*(sweepval[ii]),stop[ii]+fudge);
	if (!ok  &&  loop)
	  reverse = true;
      }
      if (reverse){
	*(sweepval[ii]) -= step[ii];
	ok=inorder(start[ii]-fudge,*(sweepval[ii]),stop[ii]+fudge);
      }
    }
  }else{
    double fudge = pow(step[ii], .1);
    if (step[ii] == 1.){
      ok = false;
    }else{
      if (!reverse){
	*(sweepval[ii]) *= step[ii];
	ok=inorder(start[ii]/fudge,*(sweepval[ii]),stop[ii]*fudge);
	if (!ok  &&  loop)
	  reverse = true;       
      }
      if (reverse){
	*(sweepval[ii]) /= step[ii];
	ok=inorder(start[ii]/fudge,*(sweepval[ii]),stop[ii]*fudge);
      }
    }
  }
  if (exists(zap[ii]))
    zap[ii]->expand();
  STATUS::control[cSTEPCAUSE] = scUSER;
  return ok;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
