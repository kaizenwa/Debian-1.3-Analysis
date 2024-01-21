/*$Id: d_coil.cc,v 11.35 96/03/22 18:09:47 al Exp $ -*- C++ -*-
 * inductors
 * two levels: linear (lin) and nonlinear (nl) (not really)
 * x = amps, y.f0 = flux, ev = y.f1 = henrys
 */
#include "ap.h"
#include "d_cccs.h"
#include "d_coil.h"
#include "error.h"
#include "io.h"
#include "u_opt.h"
#include "s__.h"
#include "l_compar.h"
#include "l_lib.h"
/*--------------------------------------------------------------------------*/
//		DEV_MUTUAL_L::DEV_MUTUAL_L();
//		DEV_MUTUAL_L::DEV_MUTUAL_L(CONST DEV_MUTUAL_L& p);
//	void	DEV_MUTUAL_L::parse(CS&);
//	void	DEV_MUTUAL_L::print(int where, int detail)const;
//	void	DEV_MUTUAL_L::expand();

//	void	DEV_INDUCTANCE::expand();
//	double	DEV_INDUCTANCE::probe(const char*)const;
// 	bool	DEV_INDUCTANCE::dotr();
// 	void	DEV_INDUCTANCE::trload();
// 	void	DEV_INDUCTANCE::trunload();
// 	void	DEV_INDUCTANCE::doac();
//	double	DEV_INDUCTANCE::tr_review();
//	void	DEV_INDUCTANCE::integrate();
/*--------------------------------------------------------------------------*/
DEV_MUTUAL_L::DEV_MUTUAL_L()
{
  devclass = TWOPORT;
  inputlabel[0] = outputlabel[0] = '\0';
  output = input = NULL;
}
/*--------------------------------------------------------------------------*/
DEV_MUTUAL_L::DEV_MUTUAL_L(CONST DEV_MUTUAL_L& p):COMPONENT(p)
{
  strcpy(outputlabel, p.outputlabel);
  strcpy(inputlabel,  p.inputlabel);
  output = input = NULL;
}
/*--------------------------------------------------------------------------*/
void DEV_MUTUAL_L::parse(CS& cmd)
{
  parselabel(cmd);
  cmd.ctostr(outputlabel, LABELEN, TOKENTERM);
  outputlabel[0] = to_upper(outputlabel[0]);
  cmd.ctostr(inputlabel, LABELEN, TOKENTERM);
  inputlabel[0] = to_upper(inputlabel[0]);
  val = cmd.ctof();
}
/*--------------------------------------------------------------------------*/
void DEV_MUTUAL_L::print(int where, int)const
{
  printlabel(where);
  if (output){
    output->printlabel(where);
  }else{
    mprintf(where, " %s  ", outputlabel);
  }
  if (input){
    input->printlabel(where);
  }else{
    mprintf(where, " %s  ", inputlabel);
  }
  mprintf(where, "%s", ftos(val, "", 7, 0));
  mputc('\n', where);
}
/*--------------------------------------------------------------------------*/
void DEV_MUTUAL_L::expand()
{
  output = findbranch_samescope(outputlabel,this);
  if (!output)
    error(bERROR,"%s: can't find %s\n", printlabel(), outputlabel);

  input = findbranch_samescope(inputlabel,this);
  if (!input)
    error(bERROR,"%s: can't find %s\n", printlabel(), inputlabel);

  if (output->subckt){
    delete output->subckt->next();
    delete output->subckt;
    output->subckt = NULL;
  }
  if (input->subckt){
    delete input->subckt->next();
    delete input->subckt;
    input->subckt = NULL;
  }

  double l1 = output->val;
  double l2 = input->val;
  double lm  = val * sqrt(l1 * l2);
  double det = l1 * l2 - lm * lm;
  
  ELEMENT * pri = (ELEMENT*)output->clone();
  assert(pri);
  pri->parent  = output;
  pri->ev = pri->y0.f1 = pri->val = det / l2;
  pri->y0.f0 = LINEAR;
  output->subckt = pri->insertbefore(output->subckt);

  ELEMENT * sec = (ELEMENT*)input->clone();
  assert(sec);
  sec->parent  = input;
  sec->ev = sec->y0.f1 = sec->val = det / l1;
  sec->y0.f0 = LINEAR;
  input->subckt = sec->insertbefore(input->subckt);

  DEV_CCCS * sub = new DEV_CCCS;
  assert(sub);
  sub->parent  = output;
  sub->input   = sec;
  sprintf(sub->label, "F%s", &sub->input->label[1]);
  sub->n[OUT1] = output->n[OUT1];
  sub->n[OUT2] = output->n[OUT2];
  sub->ev = sub->y0.f1 = sub->val = -lm / l1;
  sub->y0.f0 = LINEAR;
  output->subckt = sub->insertbefore(output->subckt);
  output->subckt->expand_group();
  
  sub = new DEV_CCCS;
  assert(sub);
  sub->parent  = input;
  sub->input   = pri;
  sprintf(sub->label, "F%s", &sub->input->label[1]);
  sub->n[OUT1] = input->n[OUT1];
  sub->n[OUT2] = input->n[OUT2];
  sub->ev = sub->y0.f1 = sub->val = -lm / l2;
  sub->y0.f0 = LINEAR;
  input->subckt = sub->insertbefore(input->subckt);
  input->subckt->expand_group();

  assert(!constant); /* because of integration */
}
/*--------------------------------------------------------------------------*/
void DEV_INDUCTANCE::expand()
{
  expand_pre();
  assert(loss == 0.);
  y0.f1 = val;
  y0.f0 = LINEAR;
  ev    = val;
  /* m0 and acg are frequency/time dependent and cannot be set here.
   * If this inductor is coupled, there is a subckt, which is expanded
   * by the mutual pseudo-element
   * assigning the values here becomes unnecessary, but harmless.
   */
}
/*--------------------------------------------------------------------------*/
bool DEV_INDUCTANCE::dotr()
{
  assert(!isbypassed());
  if (subckt){
    converged = subckt->dotr_group();
  }else{
    initial_voltage = initial_current = initial_condition = NOT_INPUT;
    advance();
    if (has_tr_eval()){
      m0.x = volts_limited(n[OUT1],n[OUT2]);
      y0.x = m0.c0 + m0.f1 * m0.x;
      tr_eval();
      assert(converged == conv_check());
    }else{
      m0.x = n[OUT1].v0() - n[OUT2].v0();
      y0.x = m0.c0 + m0.f1 * m0.x;
      y0.f1 = val;
      y0.f0 = y0.x * y0.f1;
      assert(converged);
    }
    if (y0.f1 == 0.){
      error(bPICKY, "%s: short circuit\n", printlabel());
    }
    store_values();
    SIM::loadq << this;
    integrate();
  }
  return converged;
}
/*--------------------------------------------------------------------------*/
void DEV_INDUCTANCE::trload()
{
  if (subckt){
    subckt->trload_group();
  }else{
    trload_passive();
  }
}
/*--------------------------------------------------------------------------*/
void DEV_INDUCTANCE::trunload()
{
  if (subckt){
    subckt->trunload_group();
  }else{
    trunload_passive();
  }
}
/*--------------------------------------------------------------------------*/
void DEV_INDUCTANCE::doac()
{
  if (subckt){
    subckt->doac_group();
  }else{
    if (has_ac_eval()){
      double dcvolts = n[OUT1].vdc() - n[OUT2].vdc();
      acbias = m0.c0 + m0.f1*dcvolts;
      ac_eval();
    }else if (has_tr_eval()){
      ev = y0.f1;
    }else{
      assert(ev == val);
    }
    if (SIM::jomega == 0.){
      acg = 1. / OPT::shortckt;
      error(bPICKY, "%s: short circuit\n", printlabel());
    }else{
      acg = 1. / (ev * SIM::jomega);
    }
    acload_passive();
  }
}
/*--------------------------------------------------------------------------*/
double DEV_INDUCTANCE::tr_review()
{
  if (subckt)
    return timef = subckt->tr_review_group();

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

    double i0  = m0.x;						/* volts */
    double it1 = mt1.x;
    /*double it2 = mt2.x;*/
    /*double didt0 = (i0  - it1) / dt0;*/			/* 1st der */
    /*double didt1 = (it1 - it2) / dt1;*/
    /*double ddiddt = (didt0 - didt1) / ddt0;*/			/* 2nd der */

    double q0  = y0.f0;						/* flux */
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
void DEV_INDUCTANCE::integrate()
{
  if (SIM::mode == sDC  ||  SIM::mode == sOP  ||  y0.f1 == 0.){
    m0.f1 = 1./OPT::shortckt;
    m0.c0 = 0.;
  }else if (SIM::phase == pINIT_DC){
    if (time0 == 0.){
      if (!SIM::uic){
	m0.f1 = 1./OPT::shortckt;
	m0.c0 = 0.;
      }else if (initial_current != NOT_INPUT){
	m0.f1 = 0.;
	m0.c0 = -initial_current;
      }else if (initial_voltage != NOT_INPUT){
	m0.f1 = 1./OPT::shortckt;
	m0.c0 = -initial_voltage * m0.f1;
      }else if (initial_condition != NOT_INPUT){
	m0.f1 = 0.;
	m0.c0 = -initial_condition;
      }else{
	m0.f1 = 1./OPT::shortckt;
	m0.c0 = 0.;
      }
    }else{
      /* leave it alone to restore */
    }
  }else{
    double dt = time0 - time1;
    switch (method_a){
    case mGEAR:
      m0.f1 = dt / y0.f1;
      m0.c0 = mt1.c0 + mt1.f1 * mt1.x; /* oldi */
      break;
    case mTRAPEZOID:
      /* oldi = mt1.c0 + mt1.f1 * mt1.x; */
      m0.f1 = dt/(2*y0.f1);
      m0.c0 = mt1.c0 + (mt1.f1 + m0.f1) * mt1.x;
      /* oldi + f1*oldv, with combined terms */
      break;
    case mSTIFF:
    case mUNKNOWN:
      assert(0);
      break;
    }
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
