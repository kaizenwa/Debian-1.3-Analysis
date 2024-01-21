/*$Id: d_mosc.cc,v 11.30 96/03/17 19:20:48 al Exp $ -*- C++ -*-
 * mos model subcircuit functions
 */
#include "d_mos.h"
#include "io_trace.h"
/*--------------------------------------------------------------------------*/
//	void	EVAL_MOS_Ids::tr_eval(COMPONENT*)const;
//	void	EVAL_MOS_Gmf::tr_eval(COMPONENT*)const;
//	void	EVAL_MOS_Gmr::tr_eval(COMPONENT*)const;
//	void	EVAL_MOS_Gds::tr_eval(COMPONENT*)const;
//	void	EVAL_MOS_Gmbf::tr_eval(COMPONENT*)const;
//	void	EVAL_MOS_Gmbr::tr_eval(COMPONENT*)const;
//	void	EVAL_MOS_Cgb::tr_eval(COMPONENT*)const;
//	void	EVAL_MOS_Cgd::tr_eval(COMPONENT*)const;
//	void	EVAL_MOS_Cgs::tr_eval(COMPONENT*)const;
/*--------------------------------------------------------------------------*/
/* mos2_ids: drain-source current calculations
 * returns ids
 */
void EVAL_MOS_Ids::tr_eval(COMPONENT *brh)const
{
  DEV_MOS* p = (DEV_MOS*)brh->parent;
  assert(p);
  const MOS_COMMON* c = (const MOS_COMMON*)p->common;
  assert(c);
  const MODEL_MOS* m = (const MODEL_MOS*)c->model;
  assert(m);
  assert(!p->bypass || (brh->y0.f1 == m->polarity * ((p->reversed) ? -p->ids : p->ids)));

//  if (p->bypass){
//    brh->y1 = brh->y2;
//  }

  brh->y0.f1 = m->polarity * ((p->reversed) ? -p->ids : p->ids);
  //brh->y0.f0 = brh->y0.x * brh->y0.f1;
  brh->y0.f0 = LINEAR;
}
/*--------------------------------------------------------------------------*/
/* mos2_gmf: gate transconductance calculations forward mode
 * returns gm or 0
 */
void EVAL_MOS_Gmf::tr_eval(COMPONENT *brh)const
{
  DEV_MOS* p = (DEV_MOS*)brh->parent;
  assert(p);
  assert(!p->bypass || (brh->y0.f1 == (p->reversed) ? 0. : p->gm));

//  if (p->bypass){
//    brh->y1 = brh->y2;
//  }

  brh->y0.f1 = (p->reversed) ? 0. : p->gm;
  brh->y0.f0 = 0.;
}
/*--------------------------------------------------------------------------*/
/* mos2_gmr: gate transconductance calculations reversed mode
 * returns gm or 0
 */
void EVAL_MOS_Gmr::tr_eval(COMPONENT *brh)const
{
  DEV_MOS* p = (DEV_MOS*)brh->parent;
  assert(p);
  assert(!p->bypass || (brh->y0.f1 == (p->reversed) ? p->gm : 0.));

//  if (p->bypass){
//    brh->y1 = brh->y2;
//  }

  brh->y0.f1 = (p->reversed) ? p->gm : 0.;
  brh->y0.f0 = 0.;
}
/*--------------------------------------------------------------------------*/
/* mos2_gds: self-conductance calculations
 * returns gds
 */
void EVAL_MOS_Gds::tr_eval(COMPONENT *brh)const
{
  DEV_MOS* p = (DEV_MOS*)brh->parent;
  assert(p);
  assert(!p->bypass || (brh->y0.f1 == p->gds));

//  if (p->bypass){
//    brh->y1 = brh->y2;
// }

  brh->y0.f1 = p->gds;
  brh->y0.f0 = 0.;
}
/*--------------------------------------------------------------------------*/
/* mos2_gmbf: bulk transconductance calculations, forward mode
 * returns gmb or 0
 */
void EVAL_MOS_Gmbf::tr_eval(COMPONENT *brh)const
{
  DEV_MOS* p = (DEV_MOS*)brh->parent;
  assert(p);
  trace4(brh->printlabel(), p->bypass, brh->y0.f1, p->gmb, p->reversed);
  assert(!p->bypass || (brh->y0.f1 == (p->reversed) ? 0. : p->gmb));

//  if (p->bypass){
//    brh->y1 = brh->y2;
//  }

  brh->y0.f1 = (p->reversed) ? 0. : p->gmb;
  brh->y0.f0 = 0.;
}
/*--------------------------------------------------------------------------*/
/* mos2_gmbr: bulk transconductance calculations, reversed mode
 * returns gmb or 0
 */
void EVAL_MOS_Gmbr::tr_eval(COMPONENT *brh)const
{
  DEV_MOS* p = (DEV_MOS*)brh->parent;
  assert(p);
  trace4(brh->printlabel(), p->bypass, brh->y0.f1, p->gmb, p->reversed);
  assert(!p->bypass || (brh->y0.f1 == (p->reversed) ? p->gmb : 0.));

//  if (p->bypass){
//    brh->y1 = brh->y2;
//  }

  brh->y0.f1 = (p->reversed) ? p->gmb : 0.;
  brh->y0.f0 = 0.;
}
/*--------------------------------------------------------------------------*/
/* gate capacitors.  Meyer model.  
 * Refs: Antognetti, Divekar, Spice 2 & 3 code
 * final ref was Spice 2g6 code.
 * all agree except for typos and smoothing.  (yup!!)
 * (smoothing is different)  Not compatible with Spice 3.
 * Spice 3 ignores substrate voltage
 */
void EVAL_MOS_Cgb::tr_eval(COMPONENT *brh)const
{
  DEV_MOS* p = (DEV_MOS*)brh->parent;
  assert(p);
  const MOS_COMMON* c = (const MOS_COMMON*)p->common;
  assert(c);
  const MODEL_MOS* m = (const MODEL_MOS*)c->model;
  assert(m);

  double cap = brh->val;
  if (p->vgst < - m->phi){ 			/* accumulation */
    cap += c->cgate;
  }else if (p->vgst < 0.){			/* depletion */
    cap += c->cgate * (-p->vgst) / m->phi;
  }						/* active, overlap only */
  brh->y0.f1 = cap;
  brh->y0.f0 = brh->y0.x * brh->y0.f1;
}
/*--------------------------------------------------------------------------*/
void EVAL_MOS_Cgd::tr_eval(COMPONENT *brh)const
{
  DEV_MOS* p = (DEV_MOS*)brh->parent;
  assert(p);
  const MOS_COMMON* c = (const MOS_COMMON*)p->common;
  assert(c);

  double cap = brh->val;			/* start with overlap cap */
  if (p->vgst > 0.  &&  p->vdsat > p->vds){	/* linear */
    double vdbsat = p->vdsat - p->vbs;
    double vdb    = p->vds   - p->vbs;
    double ddif   = 2. * vdbsat - vdb;
    cap += (2./3.) * c->cgate * (1. - (vdbsat*vdbsat)/(ddif*ddif));
  }						/* else overlap only */
  brh->y0.f1 = cap;
  brh->y0.f0 = brh->y0.x * brh->y0.f1;
}
/*--------------------------------------------------------------------------*/
void EVAL_MOS_Cgs::tr_eval(COMPONENT *brh)const
{
  DEV_MOS* p = (DEV_MOS*)brh->parent;
  assert(p);
  const MOS_COMMON* c = (const MOS_COMMON*)p->common;
  assert(c);
  const MODEL_MOS* m = (const MODEL_MOS*)c->model;
  assert(m);

  double cap = brh->val;			/* start with overlap cap */
#if 0
  if (p->vgst > p->vds){			/* linear */
    double numer = p->vgst - p->vds;
    double denom = 2. * p->vgst - p->vds;
    cap += (2./3.) * c->cgate * (1. - (numer*numer)/(denom*denom));
  }else if (p->vgst > 0.){			/* saturation */
    cap += (2./3.) * c->cgate;
  }else if (p->vgst > -m->phi/2.){		/* depletion */
    cap += (2./3.) * c->cgate * ((p->vgst / (m->phi/2.)) + 1.);
  }						/* accum. = overlap only */
#else
  if (p->vgst > 0.){				/* active */
    if (p->vdsat > p->vds){			/* linear */
      double vdbsat = p->vdsat - p->vbs;
      double vdb    = p->vds   - p->vbs;
      double ddif   = 2. * vdbsat - vdb;
      double ndif   = p->vdsat - p->vds;
      cap += (2./3.) * c->cgate * (1. - (ndif*ndif)/(ddif*ddif));
    }else{					/* saturation */
      cap += (2./3.) * c->cgate;
    }
  }else if (p->vgst > -m->phi/2.){		/* depletion */
    cap += (2./3.) * c->cgate * ((p->vgst / (m->phi/2.)) + 1.);
  }						/* accum. = overlap only */
#endif
  brh->y0.f1 = cap;
  brh->y0.f0 = brh->y0.x * brh->y0.f1;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
