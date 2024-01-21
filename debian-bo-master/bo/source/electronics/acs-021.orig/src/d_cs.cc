/*$Id: d_cs.cc,v 11.37 96/03/24 10:08:46 al Exp $ -*- C++ -*-
 *$Header: /al/acs/src/RCS/d_cs.cc,v 11.37 96/03/24 10:08:46 al Exp $
 * functions for fixed current source
 * x = 0, y.f0 = nothing, ev = y.f1 = amps.
 */
#include "d_cs.h"
/*--------------------------------------------------------------------------*/
//	void	DEV_CS::expand();
// 	bool	DEV_CS::dotr();
// 	void	DEV_CS::doac();
/*--------------------------------------------------------------------------*/
void DEV_CS::expand()
{
  assert(loss == 0.);
  m0.x = 0.;
  y0.x = 0.;
  m0.c0 = y0.f1 = val;
  m0.f1 = y0.f0 = 0.;
  acg = ev = 0.;
  constant = !has_tr_eval();
}
/*--------------------------------------------------------------------------*/
bool DEV_CS::dotr()
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
    m0.c0 = y0.f1;
  }else{
    assert(m0.c0 == val);
    assert(y1 == y0);
    assert(converged);
  }
  return converged;
}
/*--------------------------------------------------------------------------*/
void DEV_CS::doac()
{
  if (has_ac_eval()){
    acbias = 0.;
    ac_eval();
    acg = ev;
    acload_source();
  }else{
    assert(acg == 0.);
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
