/*$Id: d_vs.cc,v 11.37 96/03/24 10:09:06 al Exp $ -*- C++ -*-
 * functions for fixed voltage sources
 * temporary kluge: it has resistance
 */
#include "d_vs.h"
#include "u_opt.h"
/*--------------------------------------------------------------------------*/
//	void	DEV_VS::expand();
// 	bool	DEV_VS::dotr();
// 	void	DEV_VS::doac();
/*--------------------------------------------------------------------------*/
void DEV_VS::expand()
{
  loss = 1./OPT::shortckt;
  m0.x = 0.;
  y0.x = 0.;
  y0.f1 = val;
  y0.f0 = 0.;
  m0.f1 = 0.;
  m0.c0 = -loss * y0.f1;
  acg = ev = 0.;
  constant = !has_tr_eval();
}
/*--------------------------------------------------------------------------*/
bool DEV_VS::dotr()
{
  assert(!isbypassed());
  assert(m0.x == 0.);
  assert(y0.x == 0.);
  if (has_tr_eval()){
    tr_eval();
    assert(converged == conv_check());
    store_values();
    SIM::loadq << this;
    m0.f1 = 0.;
    m0.c0 = -loss * y0.f1;
  }else{
    assert(conchk(m0.c0, -loss * y0.f1, OPT::abstol));
    assert(y1 == y0);
    assert(converged);
  }
  return converged;
}
/*--------------------------------------------------------------------------*/
void DEV_VS::doac()
{
  acload_loss();
  if (has_ac_eval()){
    acbias = 0.;
    ac_eval();
    acg = -loss * ev;
    acload_source();
  }else{
    assert(acg == 0.);
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
