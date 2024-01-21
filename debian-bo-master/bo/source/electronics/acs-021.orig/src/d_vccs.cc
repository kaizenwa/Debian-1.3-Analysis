/*$Id: d_vccs.cc,v 11.37 96/03/24 10:09:02 al Exp $ -*- C++ -*-
 * functions for vccs
 */
#include "ap.h"
#include "d_vccs.h"
#include "io.h"
/*--------------------------------------------------------------------------*/
//	void	DEV_VCCS::parse(CS&);
//	void	DEV_VCCS::print(int,int)const;
//	void	DEV_VCCS::expand();
// 	bool	DEV_VCCS::dotr();
// 	void	DEV_VCCS::doac();
/*--------------------------------------------------------------------------*/
void DEV_VCCS::parse(CS& cmd)
{
  parselabel(cmd);
  parsenodes(cmd,NUMNODES);
  parseexpr(cmd);
}
/*--------------------------------------------------------------------------*/
void DEV_VCCS::print(int where, int)const
{
  printlabel(where);
  printnodes(where,NUMNODES);
  printexpr(where);
  mputc('\n', where);
}
/*--------------------------------------------------------------------------*/
void DEV_VCCS::expand()
{
  assert(loss == 0.);
  m0.f1 = y0.f1 = val;
  y0.f0 = LINEAR;
  m0.c0 = 0.;
  acg = ev = val;
  constant = !has_tr_eval();
}
/*--------------------------------------------------------------------------*/
bool DEV_VCCS::dotr()
{ 
  assert(!isbypassed());
  if (has_tr_eval()){
    m0.x = volts_limited(n[IN1],n[IN2]);
    y0.x = m0.x;
    tr_eval();
    assert(converged == conv_check());
    store_values();
    SIM::loadq << this;
    m0.c0 = y0.f0 - y0.x * y0.f1;
    m0.f1 = y0.f1;
  }else{
    assert(m0.f1 == val);
    assert(m0.c0 == 0.);
    assert(y1 == y0);
    assert(converged);
  }
  return converged;
}
/*--------------------------------------------------------------------------*/
void DEV_VCCS::doac()
{
  if (has_ac_eval()){
    acbias = n[IN1].vdc() - n[IN2].vdc();
    ac_eval();
    acg = ev;
  }else if (has_tr_eval()){
    acg = ev = y0.f1;
  }else{
    assert(acg == val);
  }
  acload_active(); 
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
