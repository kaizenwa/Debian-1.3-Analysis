/*$Id: d_res.cc,v 11.37 96/03/24 10:08:56 al Exp $ -*- C++ -*-
 * functions for resistor.
 * x = amps, y.f0 = volts, ev = y.f1 = ohms
 */
#include "error.h"
#include "u_opt.h"
#include "d_res.h"
/*--------------------------------------------------------------------------*/
//	void	DEV_RESISTANCE::expand();
// 	bool	DEV_RESISTANCE::dotr();
// 	void	DEV_RESISTANCE::doac();
/*--------------------------------------------------------------------------*/
void DEV_RESISTANCE::expand()
{
  if (val == 0.){
    error(bPICKY, "%s: short circuit\n", printlabel());
    y0.f1 = OPT::shortckt;
  }else{
    y0.f1 = val;
  }
  y0.f0 = LINEAR;
  m0.f1 = 1./y0.f1;
  m0.c0 = 0.;
  ev    = val;
  acg   = m0.f1;
  assert(loss == 0.);
  constant = !has_tr_eval();
}
/*--------------------------------------------------------------------------*/
bool DEV_RESISTANCE::dotr()
{
  assert(!isbypassed());
  if (has_tr_eval()){
    m0.x = volts_limited(n[OUT1],n[OUT2]);
    y0.x = m0.c0 + m0.f1 * m0.x;
    tr_eval();
    if (y0.f1 == 0.){
      error(bPICKY, "%s: short circuit\n", printlabel());
      y0.f1 = OPT::shortckt;
    }
    assert(converged == conv_check());
    store_values();
    SIM::loadq << this;
    m0.f1 = 1./y0.f1;
    m0.c0 = y0.x - y0.f0 / y0.f1;
  }else{
    assert(conchk(m0.f1, 1./y0.f1, OPT::abstol));
    assert(m0.c0 == 0.);
    assert(y1 == y0);
    assert(converged);
  }
  return converged;
}
/*--------------------------------------------------------------------------*/
void DEV_RESISTANCE::doac()
{
  if (has_ac_eval()){
    double dcvolts = n[OUT1].vdc() - n[OUT2].vdc();
    acbias = m0.c0 + m0.f1*dcvolts;
    ac_eval();
    if (ev == 0.){
      error(bPICKY, "%s: short circuit\n", printlabel());
      ev = OPT::shortckt;
    }
    acg = 1. / ev;
  }else if (has_tr_eval()){
    ev  = y0.f1;
    acg = 1. / y0.f1;
  }else{
    //assert(acg == 1./y0.f1);
  }
  acload_passive();
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
