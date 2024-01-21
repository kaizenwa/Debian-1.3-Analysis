/*$Id: d_cccs.cc,v 11.35 96/03/22 18:09:45 al Exp $ -*- C++ -*-
 * functions for cccs
 * this is a kluge to accomodate most of the different sense elements
 * voltage source is the worst
 */
#include "ap.h"
#include "d_cccs.h"
#include "error.h"
/*--------------------------------------------------------------------------*/
//	void	DEV_CCCS::expand();
// 	bool	DEV_CCCS::dotr();
// 	void	DEV_CCCS::doac();
/*--------------------------------------------------------------------------*/
void DEV_CCCS::expand()
{
  if (*inputlabel)
    input = (ELEMENT*)findbranch_samescope(inputlabel,this);
  if (!input)
    error(bERROR,"%s: can't find %s\n", printlabel(), inputlabel);
  if (input->is2port()){
    n[IN1] = input->n[IN1];
    n[IN2] = input->n[IN2];
  }else if (input->is1port()  ||  input->issource()){
    n[IN1] = input->n[OUT1];
    n[IN2] = input->n[OUT2];
  }else{
    assert(0);
  }
  n[IN1].e = n[IN2].e = INVALIDNODE;
  assert(loss == 0.);
  m0.f1 = y0.f1 = val;
  y0.f0 = LINEAR;
  m0.c0 = 0.;
  acg = ev = val;
  assert(!constant); /* because of incomplete analysis */
}
/*--------------------------------------------------------------------------*/
bool DEV_CCCS::dotr()
{
  assert(!isbypassed());
  m0.x = n[IN1].v0() - n[IN2].v0();
  y0.x = input->probe_tr_num("I");
  if (has_tr_eval()){
    tr_eval();
    assert(converged == conv_check());
    m0.c0 = y0.f0 - y0.x * y0.f1;
  }else{
    assert(converged);
    y0.f1 = val;
    y0.f0 = y0.x * y0.f1;
    m0.c0 = 0.;
  }
  m0.c0 += y0.f1 * input->m0.c0;
  m0.f1  = y0.f1 * (input->loss + input->m0.f1);
  store_values();
  SIM::loadq << this;
  return converged;
}
/*--------------------------------------------------------------------------*/
void DEV_CCCS::doac()
{
  if (!input->evaluated()){
    input->doac();	    /* BUG: premature load of sense element */
  }
  if (has_ac_eval()){
    acbias = n[IN1].vdc() - n[IN2].vdc();
    ac_eval();
  }else{
    ev  = y0.f1;
  }
  if (input->issource()){
    acg = ev * input->acg;
    acload_source();
    acg = ev * input->loss;
    acload_active();
  }else{
    acg = ev * input->acg;
    acload_active();
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
