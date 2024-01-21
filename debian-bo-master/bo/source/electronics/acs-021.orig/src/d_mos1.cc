/*$Id: d_mos1.cc,v 11.28 96/03/03 23:07:14 al Exp $ -*- C++ -*-
 * mos model equations: spice level 1 equivalent
 */
#include "d_mos.h"
#include "io_trace.h"
/*--------------------------------------------------------------------------*/
//	void	MODEL_MOS::eval_mos1(DEV_MOS*)const;
/*--------------------------------------------------------------------------*/
void MODEL_MOS::eval_mos1(DEV_MOS *d)const
{
  assert(d);
  const MOS_COMMON* c = (const MOS_COMMON*)d->common;
  assert(c);
  const MODEL_MOS* m = this;
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  trace1(d->printlabel(), d->evaliter);
  trace3("", d->vds, d->vgs, d->vbs);
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  double sarg, dsarg_dvbs;
  {
    if (d->vbs <= 0.){
      sarg = sqrt(m->phi - d->vbs);
      dsarg_dvbs = -.5 / sarg;
      d->sbfwd = false;
      trace2("sb-ok", sarg, dsarg_dvbs);
    }else{
      sarg = m->sqrt_phi / (1. + .5 * d->vbs / m->phi);
      dsarg_dvbs = -.5 * sarg * sarg / m->phi_sqrt_phi;	/* is wrong!! */
      d->sbfwd = true;
      trace2("***sb-reversed***", sarg, dsarg_dvbs);
    }
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  {
    if (d->vbs - d->vds <= 0.){
      d->dbfwd = false;
    }else{
      d->dbfwd = true;
    }
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  d->von = m->vto + m->gamma * (sarg - m->sqrt_phi);
  d->vgst = d->vdsat = d->vgs - d->von;
  if (d->vdsat < 0.)
    d->vdsat = 0.;
  d->cutoff = (d->vgst < 0.);
  d->saturated = (d->vds > d->vdsat);
  trace3("", d->von, d->vgst, d->vdsat);
  double Lambda = (m->lambda != NOT_INPUT) ? m->lambda : 0.;
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  if (d->cutoff){
    d->gds = d->gm = d->ids = d->gmb = 0.;
    trace4("cut", d->ids, d->gm, d->gds, d->gmb);
  }else if (d->saturated){
    d->gm  = c->beta * d->vgst * (1. + Lambda * d->vds);
    d->ids = d->gm * (.5 * d->vgst);
    d->gds = .5 * c->beta * Lambda * d->vgst * d->vgst;
    d->gmb = - d->gm * m->gamma * dsarg_dvbs;
    trace4("sat", d->ids, d->gm, d->gds, d->gmb);
  }else{ /* linear */
    d->gm  = c->beta * d->vds * (1. + Lambda * d->vds);
    d->ids = d->gm * (d->vgst - .5*d->vds);
    d->gds = c->beta * ((d->vgst - d->vds) 
    		     + Lambda * d->vds * (2.*d->vgst - 1.5*d->vds));
    d->gmb = -d->gm * m->gamma * dsarg_dvbs;
    trace4("lin", d->ids, d->gm, d->gds, d->gmb);
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
