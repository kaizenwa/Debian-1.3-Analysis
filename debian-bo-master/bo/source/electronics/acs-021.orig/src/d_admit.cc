/*$Id: d_admit.cc,v 11.37 96/03/24 10:08:39 al Exp $ -*- C++ -*-
 * functions for admittance (type 'Y')
 * does not exist in spice
 * x = volts, y.f0 = amps, ev = y.f1 = mhos.
 */
#include "d_admit.h"
/*--------------------------------------------------------------------------*/
//	void	DEV_ADMITTANCE::expand();
// 	bool	DEV_ADMITTANCE::dotr();
// 	void	DEV_ADMITTANCE::doac();
/*--------------------------------------------------------------------------*/
void DEV_ADMITTANCE::expand()
{
  m0.f1 = y0.f1 = val;
  y0.f0 = LINEAR;
  m0.c0 = 0.;
  acg = ev = val;
  assert(loss == 0.);
  constant = !has_tr_eval();
}
/*--------------------------------------------------------------------------*/
bool DEV_ADMITTANCE::dotr()
{
  assert(!isbypassed());
  if (has_tr_eval()){
    m0.x = volts_limited(n[OUT1],n[OUT2]);
    y0.x = m0.x;
    y0.f0 = m0.f1 * m0.x + m0.c0;	/* BUG:  patch for diode */
    tr_eval();
    assert(converged == conv_check());
    store_values();
    SIM::loadq << this;
    m0.x = y0.x;
    m0.c0 = y0.f0 - y0.x * y0.f1;
    m0.f1 = y0.f1;
  }else{
    assert(y0.f1 == val);
    assert(m0.f1 == val);
    assert(m0.c0 == 0.);
    assert(y1 == y0);
    assert(converged);
  }
  return converged;
}
/*--------------------------------------------------------------------------*/
void DEV_ADMITTANCE::doac()
{
  if (has_ac_eval()){
    acbias = n[OUT1].vdc() - n[OUT2].vdc();
    ac_eval();
    acg = ev;
  }else if (has_tr_eval()){
    acg = ev = y0.f1;
  }else{
    assert(acg == val);
  }
  acload_passive(); 
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
