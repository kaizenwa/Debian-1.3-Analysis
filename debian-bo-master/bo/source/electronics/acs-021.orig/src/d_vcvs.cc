/*$Id: d_vcvs.cc,v 11.37 96/03/24 10:09:05 al Exp $ -*- C++ -*-
 * functions for vcvs
 * temporary kluge: it has resistance
 */
#include "ap.h"
#include "d_vcvs.h"
#include "u_opt.h"
#include "io.h"
/*--------------------------------------------------------------------------*/
//	void	DEV_VCVS::parse(CS&);
//	void	DEV_VCVS::print(int,int)const;
//	void	DEV_VCVS::expand();
// 	bool	DEV_VCVS::dotr();
// 	void	DEV_VCVS::doac();
/*--------------------------------------------------------------------------*/
void DEV_VCVS::parse(CS& cmd)
{
  parselabel(cmd);
  parsenodes(cmd,NUMNODES);
  parseexpr(cmd);
}
/*--------------------------------------------------------------------------*/
void DEV_VCVS::print(int where, int)const
{
  printlabel(where);
  printnodes(where,NUMNODES);
  printexpr(where);
  mputc('\n', where);
}
/*--------------------------------------------------------------------------*/
void DEV_VCVS::expand()
{
  loss = 1./OPT::shortckt;
  y0.f1 = val;
  y0.f0 = LINEAR;
  m0.f1 = -loss * val;
  m0.c0 = 0.;
  ev = val;
  acg = -loss * val;
  constant = !has_tr_eval();
}
/*--------------------------------------------------------------------------*/
bool DEV_VCVS::dotr()
{
  assert(!isbypassed());
  if (has_tr_eval()){
    m0.x = volts_limited(n[IN1],n[IN2]);
    y0.x = m0.x;
    tr_eval();
    assert(converged == conv_check());
    store_values();
    SIM::loadq << this;
    m0.c0 = -loss * (y0.f0 - y0.x * y0.f1);
    m0.f1 = -loss * y0.f1;
  }else{
    assert(conchk(m0.f1, -loss*val, OPT::abstol));
    assert(m0.c0 == 0.);
    assert(y1 == y0);
    assert(converged);
  }
  return converged;
}
/*--------------------------------------------------------------------------*/
void DEV_VCVS::doac()
{
  acload_loss();
  if (has_ac_eval()){
    acbias = n[IN1].vdc() - n[IN2].vdc();
    ac_eval();
    acg = -loss * ev;
  }else if (has_tr_eval()){
    ev = y0.f1;
    acg = -loss * y0.f1;
  }else{
    assert(ev == val);
    //assert(acg == -loss*val);
  }
  acload_active(); 
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
