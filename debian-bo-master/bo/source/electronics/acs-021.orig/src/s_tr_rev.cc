/*$Id: s_tr_rev.cc,v 11.22 96/02/18 11:45:03 al Exp $ -*- C++ -*-
 * review the solution after solution at a time point
 * Set up events, evaluate logic inputs, truncation error.
 * Recommend adjusted step size. (approxtime)
 * and say why (control)
 */
#include "error.h"
#include "e_card.h"
#include "u_opt.h"
#include "u_status.h"
#include "s_tr.h"
#include "l_compar.h"
/*--------------------------------------------------------------------------*/
//	void	TRANSIENT::review(void);
/*--------------------------------------------------------------------------*/
void TRANSIENT::review(void)
{
  static double rtime;	/* next time by iteration count and smoothing */
  double tetime;	/* next time by device dependent recommendations */
 			/* (usually truncation error) */
 
  STATUS::review.start();
  ++STATUS::iter[iTOTAL];
  if (phase == pINIT_DC){
    rtime = dtmax/ 100.;	/* set (guess) initial internal step */
    rtime = time0 + max(rtime, dtmin);
    control = scITER_A;
  }else{
    double adt; 	/* actual dt most recently used */ 
    double rdt;		/* review dt was recommended by PREVIOUS review */
    rdt = rtime - time1;
    adt = time0 - time1;
    if (adt > rdt + dtmin)
      error(bDANGER, "internal error: step control (%g,%g)\n", adt, rdt);

    if (STATUS::iter[iSTEP] > OPT::itl[OPT::TRHIGH]){ /* too many iterations */
      rtime = time1 + adt / OPT::trstepshrink;  /* try again, smaller step */
      control = scITER_R;
    }else if (STATUS::iter[iSTEP] > OPT::itl[OPT::TRLOW]){
      rtime = time0 + adt;			/* no growth */
      control = scITER_A;
    }else{					/* too few iterations */
      rtime = time0 + dtmax;			/* ok to use bigger steps */
      control = scSKIP;
      if (rtime > time0 + rdt * OPT::trstepgrow){
	rtime = time0 + rdt * OPT::trstepgrow;	/* limit to rdt*2 */
	control = scRDT;
      }
      if (rtime > time0 + adt*OPT::trstepgrow  &&  rtime > time0 + rdt){
	if (rdt > adt * OPT::trstepgrow)
	  rtime = time0 + rdt;
	else					/* limit to max(rdt,adt*2) */
	  rtime = time0 + adt * OPT::trstepgrow;
	control = scADT;
      }
    }
  }
  tetime = CARD::tr_review_all();		/* trunc error, etc. */
  if (tetime < rtime){
    control = scTE;
    approxtime = tetime;
  }else{
    approxtime = rtime;
  }
  STATUS::review.stop();
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
