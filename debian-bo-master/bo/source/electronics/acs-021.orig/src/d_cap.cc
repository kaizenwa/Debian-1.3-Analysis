/*$Id: d_cap.cc,v 11.35 96/03/22 18:09:44 al Exp $ -*- C++ -*-
 * capacitor models
 * two levels: linear (lin) and nonlinear (nl)
 * y.x = volts, y.f0 = coulombs, ev = y.f1 = farads
 */
#include "ap.h"
#include "d_cap.h"
#include "error.h"
#include "u_opt.h"
#include "s__.h"
#include "l_compar.h"
/*--------------------------------------------------------------------------*/
//	void	DEV_CAPACITANCE::expand();
//	double	DEV_CAPACITANCE::probe(const char*)const;
// 	bool	DEV_CAPACITANCE::dotr();
// 	void	DEV_CAPACITANCE::doac();
//	double	DEV_CAPACITANCE::tr_review();
//	void	DEV_CAPACITANCE::integrate();
/*--------------------------------------------------------------------------*/
void DEV_CAPACITANCE::expand()
{
  expand_pre();
  assert(loss == 0.);
  y0.f1 = val;
  y0.f0 = LINEAR;
  ev    = val;
  converged = true;
  assert(!constant); /* because of integration */
  /* m0 and acg are frequency/time dependent and cannot be set here */
}
/*--------------------------------------------------------------------------*/
bool DEV_CAPACITANCE::dotr()
{
  assert(!isbypassed());
  initial_voltage = initial_current = initial_condition = NOT_INPUT;
  advance();
  if (has_tr_eval()){
    m0.x = y0.x = volts_limited(n[OUT1],n[OUT2]);
    y0.f0 = m0.f1 * m0.x + m0.c0;	/* BUG: WHY?? */
    tr_eval();
    assert(converged == conv_check());
  }else{
    m0.x = y0.x = n[OUT1].v0() - n[OUT2].v0();
    y0.f1 = val;
    y0.f0 = y0.x * y0.f1;
    assert(converged);
  }
  store_values();
  SIM::loadq << this;
  integrate();
  return converged;
}
/*--------------------------------------------------------------------------*/
void DEV_CAPACITANCE::doac()
{
  if (has_ac_eval()){
    acbias = n[OUT1].vdc() - n[OUT2].vdc();
    ac_eval();
  }else if (has_tr_eval()){
    ev = y0.f1;
  }else{
    assert(ev == val);
  }
  acg = ev * SIM::jomega;
  acload_passive();
}
/*--------------------------------------------------------------------------*/
double	DEV_CAPACITANCE::tr_review()
{
  if (time3 <= 0.){
    timef = BIGBIG;
  }else{
    double factor = 1./12.;      /* coefficient of 3rd der, trapezoid rule */
    double dt0 = time0 - time1;
    double dt1 = time1 - time2;		/* BUG: these values should */
    double dt2 = time2 - time3;		/* be stored */
    double ddt0 = time0 - time2;
    double ddt1 = time1 - time3;
    double dddt0 = time0 - time3;

    double i0  = m0.f1  * m0.x  + m0.c0;			/* current */
    double it1 = mt1.f1 * mt1.x + mt1.c0;
    /*double it2 = mt2.f1 * mt2.x + mt2.c0;*/
    /*double didt0 = (i0  - it1) / dt0;*/			/* 1st der */
    /*double didt1 = (it1 - it2) / dt1;*/
    /*double ddiddt = (didt0 - didt1) / ddt0;*/			/* 2nd der */

    double q0  = y0.f0;						/* charge */
    double qt1 = yt1.f0;
    double qt2 = yt2.f0;
    double qt3 = yt3.f0;
    double dqdt0 = (q0   - qt1) / dt0;				/* 1st der */
    double dqdt1 = (qt1  - qt2) / dt1;
    double dqdt2 = (qt2  - qt3) / dt2;
    double ddqddt0 = (dqdt0 - dqdt1) / ddt0;			/* 2nd der */
    double ddqddt1 = (dqdt1 - dqdt2) / ddt1;
    double dddqdddt = (ddqddt0 - ddqddt1) / dddt0;		/* 3rd der */

    double currenttol = OPT::abstol + OPT::reltol * max(fabs(i0), fabs(it1));
    double chargetol = max(OPT::chgtol,max(fabs(q0),fabs(qt1)))
      * OPT::reltol / dt0;
    double tol = max(currenttol,chargetol);
    double denom = max(OPT::abstol, (factor *fabs(dddqdddt)));  /* avoid / 0 */
    double timestep = OPT::trtol * sqrt(tol / denom);

    if (timestep <= SIM::dtmin){
      error(bDANGER,"step control error:%s %g\n",printlabel(),timestep);
      error(bTRACE, "q0=%g i0=%g dq0=%g\n", q0, i0, dqdt0);
      error(bTRACE, "it=%g qt=%g tol=%g\n", currenttol, chargetol, tol);
      timestep = SIM::dtmin;
    }
    if (timestep < dt0 * OPT::trreject){
      error(bTRACE, "step rejected:%s\n", printlabel());
      error(bTRACE, "new=%g  old=%g  rej=%g\n",
	    timestep, dt0, dt0 * OPT::trreject);
      timef = time1 + timestep;
    }else{
      timef = time0 + timestep;
    }
  }
  return timef;
}
/*--------------------------------------------------------------------------*/
void DEV_CAPACITANCE::integrate()
{
  if (SIM::mode == sDC  ||  SIM::mode == sOP){
    m0.c0 = m0.f1 = 0.;
  }else if (SIM::phase == pINIT_DC){
    if (time0 == 0.){
      if (!SIM::uic){
	m0.c0 = m0.f1 = 0.;
      }else if (initial_current != NOT_INPUT){
	m0.f1 = 0.;
	m0.c0 = -initial_current;
      }else if (initial_voltage != NOT_INPUT){
	m0.f1 = 1./OPT::shortckt;
	m0.c0 = -initial_voltage * m0.f1;
      }else if (initial_condition != NOT_INPUT){
	m0.f1 = 1./OPT::shortckt;
	m0.c0 = -initial_condition * m0.f1;
      }else{
	m0.c0 = m0.f1 = 0.;
      }
    }else{
      /* leave it alone to restore */
    }
  }else{
    double oldv = mt1.x;
    double dt = time0 - time1;
    assert(dt > 0.);
    switch (method_a){
    case mGEAR:{
      m0.f1 = y0.f1 / dt;		  /* first order 	      */
      m0.c0 = -m0.f1 * oldv;		  /* (stiff) (backward Euler) */
      break;
    }
    case mTRAPEZOID:{			  /* second order 	     */
      double oldi = mt1.c0 + mt1.f1*oldv; /* (non-stiff) (trapezoid) */
      m0.f1 = 2. * y0.f1 / dt;
      m0.c0 = -m0.f1 * oldv - oldi;
      break;
    }
    case mSTIFF:
    case mUNKNOWN:
      assert(0);
      break;
    }
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
