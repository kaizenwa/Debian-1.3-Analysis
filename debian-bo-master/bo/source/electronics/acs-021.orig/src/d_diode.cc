/*$Id: d_diode.cc,v 11.35 96/03/22 18:09:50 al Exp $ -*- C++ -*-
 * diode model.
 * netlist syntax:
 * device:  dxxxx n+ n- mname <area> <off> <ic=vd> <model-card-args>
 * model:   .model mname D <args>
 */
#include "ap.h"
#include "d_admit.h"
#include "d_cap.h"
#include "d_diode.h"
#include "e_aux.h"
#include "error.h"
#include "io.h"
#include "s__.h"
#include "u_opt.h"
#include "u_status.h"
#include "io_trace.h"
#include "l_lib.h"
/*--------------------------------------------------------------------------*/
//		MODEL_DIODE::MODEL_DIODE(const char*);
//	void	MODEL_DIODE::parse(CS& cmd);
// 	void	MODEL_DIODE::print(int,int)const;
//		DIODE_COMMON::DIODE_COMMON()
//		DEV_DIODE::DEV_DIODE();
//		DEV_DIODE::DEV_DIODE(CONST DEV_DIODE& p);
//	void	DEV_DIODE::parse(CS& cmd);
// 	void	DEV_DIODE::print(int,int)const;
//	void	DEV_DIODE::expand();
//	double	DEV_DIODE::probe_tr_num(const char *what)const;
//	void	EVAL_DIODE_Yj::tr_eval(COMPONENT *d);
//	void	EVAL_DIODE_Cj::tr_eval(COMPONENT *d);
/*--------------------------------------------------------------------------*/
int DEV_DIODE::Count = 0;
static EVAL_DIODE_Cj Eval_Cj;
static EVAL_DIODE_Yj Eval_Yj;
static DIODE_COMMON Default_DIODE;
/*--------------------------------------------------------------------------*/
MODEL_DIODE::MODEL_DIODE(const char *name):MODEL_CARD(name)
{
  js   = 1e-14;
  rs   = 0.0;
  n    = 1.0;
  tt   = 0.0;
  cjo  = 0.0;
  pb   = 1.0;
  mj   = 0.5;
  eg   = 1.11;
  xti  = 3.0;
  kf   = 0.0;
  af   = 1.0;
  fc   = 0.5;
  bv   = 0.0;	/* infinity */
  ibv  = 1e-3;
  cjsw = 0.0;
  mjsw = 0.33;
  fcpb = fc * pb;
}
/*--------------------------------------------------------------------------*/
void MODEL_DIODE::parse(CS& cmd)
{
  cmd.skiparg();			/* skip known ".model" */
  cmd.ctostr(label, LABELEN, TOKENTERM);
  cmd.skiparg();			/* skip known "d" */
  cmd.skiplparen();
  cmd.stuck();
  do{
    cmd.get("IS",   &js,   mPOSITIVE);
    cmd.get("RS",   &rs,   mPOSITIVE);
    cmd.get("N",    &n,    mPOSITIVE);
    cmd.get("TT",   &tt,   mPOSITIVE);
    cmd.get("CJo",  &cjo,  mPOSITIVE);
    cmd.get("VJ",   &pb,   mPOSITIVE);
    cmd.get("PB",   &pb,   mPOSITIVE);
    cmd.get("Mj",   &mj,   mPOSITIVE);
    cmd.get("EGap", &eg,   mPOSITIVE);
    cmd.get("XTI",  &xti,  mPOSITIVE);
    cmd.get("KF",   &kf,   mPOSITIVE);
    cmd.get("AF",   &af,   mPOSITIVE);
    cmd.get("FC",   &fc,   mPOSITIVE);
    cmd.get("BV",   &bv,   mPOSITIVE);
    cmd.get("IBV",  &ibv,  mPOSITIVE);
    cmd.get("CJSw", &cjsw, mPOSITIVE);
    cmd.get("MJSw", &mjsw, mPOSITIVE);
  }while (cmd.more() && !cmd.stuck());
  cmd.skiprparen();
  cmd.check(bWARNING);
  fcpb = fc * pb;
}
/*--------------------------------------------------------------------------*/
void MODEL_DIODE::print(int where, int)const
{
  mprintf(where, ".model  %s  d  (", label);
  mprintf(where, " is=%s ",  ftos(js,  "", 7, 0));
  mprintf(where, " rs=%s ",  ftos(rs,  "", 7, 0));
  mprintf(where, " n=%s ",   ftos(n,   "", 7, 0));
  mprintf(where, " tt=%s ",  ftos(tt,  "", 7, 0));
  mprintf(where, " cjo=%s ", ftos(cjo, "", 7, 0));
  mprintf(where, " vj=%s ",  ftos(pb,  "", 7, 0));
  mprintf(where, " m=%s ",   ftos(mj,  "", 7, 0));
  mprintf(where, " eg=%s ",  ftos(eg,  "", 7, 0));
  mprintf(where, " xti=%s ", ftos(xti, "", 7, 0));
  mprintf(where, " kf=%s ",  ftos(kf,  "", 7, 0));
  mprintf(where, " af=%s ",  ftos(af,  "", 7, 0));
  mprintf(where, " fc=%s ",  ftos(fc,  "", 7, 0));
  mprintf(where, " bv=%s ",  ftos(bv,  "", 7, 0));
  mprintf(where, " ibv=%s ", ftos(ibv, "", 7, 0));
  if (cjsw != 0.){
    mprintf(where, " cjsw=%s ", ftos(cjsw,"", 7, 0));
    mprintf(where, " mjsw=%s ", ftos(mjsw,"", 7, 0));
  }
  mprintf(where, ")\n");
}
/*--------------------------------------------------------------------------*/
DIODE_COMMON::DIODE_COMMON()
{
  is	= NOT_INPUT;
  rs	= NOT_INPUT;
  cj	= NOT_INPUT;
  cjsw	= NOT_INPUT;
  area	= 1.0;
  perim	= 0.0;
  ic	= NOT_INPUT;
  off	= false;
  calc.is = calc.rs = calc.cj = calc.cjsw = false;
}
/*--------------------------------------------------------------------------*/
DEV_DIODE::DEV_DIODE()
{
  attach_common(&Default_DIODE);
  devclass = OTHERDEVICE;
  Yj = NULL;
  Cj = NULL;
  region = UNKNOWN;
  ++Count;
}
/*--------------------------------------------------------------------------*/
DEV_DIODE::DEV_DIODE(CONST DEV_DIODE& p):BASE_SUBCKT(p)
{
  Yj = NULL;
  Cj = NULL;
  region = UNKNOWN;
  ++Count;
}
/*--------------------------------------------------------------------------*/
void DEV_DIODE::parse(CS& cmd)
{
  assert(common);
  DIODE_COMMON* c = new DIODE_COMMON(*(const DIODE_COMMON*)common);
  assert(c);
  
  parselabel(cmd);
  parsenodes(cmd,NUMNODES);
  cmd.ctostr(c->modelname, LABELEN, TOKENTERM);
  c->model = NULL;
  if (cmd.is_pfloat()){
    c->area = cmd.ctopf();
  }
  cmd.stuck();
  do{
    cmd.get("Area",  &c->area,  mPOSITIVE);
    cmd.get("Perim", &c->perim, mPOSITIVE);
    cmd.get("IC",    &c->ic);
    cmd.set("OFF",   &c->off,	true);
    cmd.get("IS",    &c->is,    mPOSITIVE);
    cmd.get("Rs",    &c->rs,    mPOSITIVE);
    cmd.get("Cjo",   &c->cj,    mPOSITIVE);
    cmd.get("CJSW",  &c->cjsw,  mPOSITIVE);
  }while (cmd.more() && !cmd.stuck());
  cmd.check(bWARNING);
  attach_common(c);
}
/*--------------------------------------------------------------------------*/
void DEV_DIODE::print(int where, int)const
{
  const DIODE_COMMON* c = (const DIODE_COMMON*)common;
  assert(c);

  printlabel(where);
  printnodes(where,NUMNODES);
  mprintf(where, " %s ", c->modelname);

  mprintf(where,    " %s ", ftos(c->area,"", 7, 0));
  if (c->perim != 0.)
    mprintf(where,"perim=%s ",ftos(c->perim,"", 7, 0));
  if (c->off)
    mprintf(where, " off ");
  if (c->ic != NOT_INPUT)
    mprintf(where, " ic=%s ", ftos(c->ic,  "", 7, 0));
  if (!c->calc.is  &&  c->is != NOT_INPUT)
    mprintf(where, " is=%s ", ftos(c->is,  "", 7, 0));
  if (!c->calc.rs  &&  c->rs != NOT_INPUT)
    mprintf(where, " rs=%s ", ftos(c->rs,  "", 7, 0));
  if (!c->calc.cj  &&  c->cj != NOT_INPUT)
    mprintf(where, " cj=%s ", ftos(c->cj,  "", 7, 0));
  if (!c->calc.cjsw  &&  c->cjsw != NOT_INPUT)
    mprintf(where, " cjsw=%s ",ftos(c->cjsw,"", 7, 0));
  if (c->calc.is  &&  c->calc.rs  &&  c->calc.cj  &&  c->calc.cjsw)
    mprintf(where, "\n*+");
  if (c->calc.is  &&  c->is != NOT_INPUT)
    mprintf(where, " is=%s ", ftos(c->is,  "", 7, 0));
  if (c->calc.rs  &&  c->rs != NOT_INPUT)
    mprintf(where, " rs=%s ", ftos(c->rs,  "", 7, 0));
  if (c->calc.cj  &&  c->cj != NOT_INPUT)
    mprintf(where, " cj=%s ", ftos(c->cj,  "", 7, 0));
  if (c->calc.cjsw  &&  c->cjsw != NOT_INPUT)
    mprintf(where, " cjsw=%s ",ftos(c->cjsw,"", 7, 0));
  mprintf(where, "\n");
}
/*--------------------------------------------------------------------------*/
void DEV_DIODE::expand()
{
  DIODE_COMMON* c = (DIODE_COMMON*)common;
  assert(c);
  const MODEL_DIODE* m = (const MODEL_DIODE*)attach_model();
  assert(m);
  
  if (c->calc.is  ||  c->is == NOT_INPUT){
    c->is = m->js * c->area;
    c->calc.is = true;
  }
  if (c->calc.rs  ||  c->rs == NOT_INPUT){
    c->rs = m->rs * c->area;
    c->calc.rs = true;
  }
  if (c->calc.cj  ||  c->cj == NOT_INPUT){
    c->cj = m->cjo * c->area;
    c->calc.cj = true;
  }
  if (c->calc.cjsw  ||  c->cjsw == NOT_INPUT){
    c->cjsw = m->cjsw * c->perim;
    c->calc.cjsw = true;
  }

  subckt = Yj;		/* subckt points at any element in the subckt */
			/* this forces it to be one that will not be deleted */
			/* otherwise it could result in a dangling pointer */

							/* build subckt */
  if (c->cj != 0.  ||  c->cjsw != 0.){
    if (!Cj){
      Cj = new DEV_CAPACITANCE;
      assert(Cj);
      subckt = Cj->insertbefore(subckt);
      assert(subckt);
    }
    Cj->set("Cj", this, &Eval_Cj, 0., n[OUT1], n[OUT2]);
  }else{
    delete Cj;
    Cj = NULL;
  }

  if (!Yj){
    Yj = new DEV_ADMITTANCE;
    assert(Yj);
    subckt = Yj->insertbefore(subckt);
    assert(subckt);
  }
  Yj->set("Yj", this, &Eval_Yj, 0., n[OUT1], n[OUT2]);
  assert(subckt);
  subckt->expand_group();

  assert(!constant); /* because it is nonlinear */
}
/*--------------------------------------------------------------------------*/
double DEV_DIODE::probe_tr_num(const char *what)const
{
  CS cmd(what);
  assert(subckt);
  
  if (cmd.pmatch("Vd")){
    return n[OUT1].v0() - n[OUT2].v0();
  }else if (cmd.pmatch("Id")){
    return CARD::probe(Yj,"I") + CARD::probe(Cj,"I");
  }else if (cmd.pmatch("IJ")){
    return CARD::probe(Yj,"I");
  }else if (cmd.pmatch("IC")){
    return CARD::probe(Cj,"I");
  }else if (cmd.pmatch("P")){
    return CARD::probe(Yj,"P") + CARD::probe(Cj,"P");
  }else if (cmd.pmatch("PD")){
    return CARD::probe(Yj,"PD") + CARD::probe(Cj,"PD");
  }else if (cmd.pmatch("PS")){
    return CARD::probe(Yj,"PS") + CARD::probe(Cj,"PS");
  }else if (cmd.pmatch("PJ")){
    return CARD::probe(Yj,"P");
  }else if (cmd.pmatch("PC")){
    return CARD::probe(Cj,"P");
  }else if (cmd.pmatch("Cap")){
    return CARD::probe(Cj,"EV");
  }else if (cmd.pmatch("Req")){
    return CARD::probe(Yj,"R");
  }else if (cmd.pmatch("Y")){
    return CARD::probe(Yj,"Y") + CARD::probe(Cj,"Y");
  }else if (cmd.pmatch("Z")){
    return port_impedance(n[OUT1], n[OUT2], lu, probe_tr_num("Y"));
  }else if (cmd.pmatch("ZRAW")){
    return port_impedance(n[OUT1], n[OUT2], lu, 0.);
  }else if (cmd.pmatch("REgion")){
    return (double)region;
  }else { /* bad parameter */
    return NOT_VALID;
  }
}
/*--------------------------------------------------------------------------*/
void EVAL_DIODE_Yj::tr_eval(COMPONENT *d)const
{
  DEV_DIODE* p = (DEV_DIODE*)d->parent;
  assert(p);
  const DIODE_COMMON* c = (const DIODE_COMMON*)p->common;
  assert(c);
  const MODEL_DIODE* m = (const MODEL_DIODE*)c->model;
  assert(m);
  fpoly1_t& y = d->y0;

  double volts = y.x;
  double amps  = y.f0;
  trace3(d->printlabel(), d->evaliter, volts, amps);

  double tempratio = SIM::temp / OPT::tnom;
  double vt = (K/Q) * SIM::temp * m->n;
  region_t oldregion = p->region;
  p->isat = c->is * pow(tempratio, m->xti) * Exp((m->eg/vt) * (tempratio-1));
  trace4("", tempratio, vt, oldregion, p->isat);

  if (c->off  &&  STATUS::iter[SIM::mode] <= 1){     /* initially guess off */
    p->region = INITOFF;
    y.f1 = 0.;
    y.f0 = 0.;
    if (OPT::diodeflags & 0040){
      y.f1 = OPT::gmin;
    }
    trace2("initoff", y.f0, y.f1);
  }else if (volts < 0. /* &&  amps < 0.*/){    		  /* reverse biased */
    p->region = REVERSE;	    		  /* x = volts, f(x) = amps */
    double expterm = p->isat * Exp(volts/vt);	
    y.f0 = expterm - p->isat;        /* i = f(x) = isat * (Exp(volts/vt)-1) */
    y.f1 = expterm / vt;	     /* f'(x) = (isat/vt) * Exp(volts/vt)   */
    if (OPT::diodeflags & 0001){
      y.f1 = OPT::gmin;
    }else if (OPT::diodeflags & 0020){
      y.f1 = OPT::gmin;
      y.f0 = OPT::gmin * volts;
    }else if (OPT::diodeflags & 0100){
      y.f1 = p->isat / vt;
    }
    trace2("reverse", y.f0, y.f1);
  }else if (volts > 0.  &&  amps > 0.){			  /* forward biased */
						  /* x = amps, f(x) = volts */
    /* derivation: */		    /* if f(x) = log(u): f'(x)=(1/u)(du/dx) */
    /* poly1 r; */
    /* r.f0 = vt * log(amps/p->isat +1.); */
    /* r.f1 = vt / (isat + amps); */
    /* y.f1 = 1. / r.f1; */
    /* y.f0 = amps - r.f0*y.f1 + volts*y.f1; */
    
    p->region = FORWARD;
    y.f1 = (p->isat + amps) / vt;
    y.f0 = amps - log(amps/p->isat +1.)*(p->isat + amps) + volts*y.f1;
    trace2("forward", y.f0, y.f1);
  }else{			    /* non-converged, inconsistent	    */
    p->region = UNKNOWN;	    /* volts and amps have different signs  */
    y.f1 = p->isat/vt;		    /* guess that the voltage should be 0   */
    y.f0 = 0.;			    /* (it usually is very close)	    */
    if (OPT::diodeflags & 0010){    /* use the correct value there	    */
      y.f0 = y.x * y.f1;
    }
    trace2("unknown", y.f0, y.f1);
  }
  if (oldregion != p->region  &&  OPT::dampstrategy & dsDEVLIMIT){
      SIM::fulldamp = true;
      error(bTRACE,"%s:device limit damp\n", p->printlabel());
  }
  if (OPT::diodeflags & 0004){
    if (y.f1 < OPT::gmin)
      y.f1 = OPT::gmin;
  }
  if (OPT::diodeflags & 0002){
    y.f1 += OPT::gmin;
    y.f0 += OPT::gmin * volts;
  }
  p->gd = y.f1;
}
/*--------------------------------------------------------------------------*/
void EVAL_DIODE_Cj::tr_eval(COMPONENT *d)const
{
  DEV_DIODE* p = (DEV_DIODE*)d->parent;
  assert(p);
  const DIODE_COMMON* c = (const DIODE_COMMON*)p->common;
  assert(c);
  const MODEL_DIODE* m = (const MODEL_DIODE*)c->model;
  assert(m);

  double volts = d->y0.x;
  trace2(d->printlabel(), d->evaliter, volts);

  double cb;
  if (c->cj != 0.){
    if (volts < m->fcpb){
      cb = c->cj / pow(1. - (volts / m->pb),  m->mj);
    }else{
      cb = (c->cj / pow(1. - m->fc, 1. + m->mj))
	* (1. - m->fc*(1.+m->mj) + (volts/m->pb)*m->mj);
    }
  }else{
    cb = 0.;
  }
  
  double csw;
  if (c->cjsw != 0.){
    if (volts < m->fcpb){
      csw = c->cjsw / pow(1. - (volts / m->pb),  m->mjsw);
    }else{
      csw = (c->cjsw / pow(1. - m->fc, 1. + m->mjsw))
	* (1. - m->fc*(1.+m->mjsw) + (volts/m->pb)*m->mjsw);
    }
  }else{
    csw = 0.;
  }
  
  double ctt;
  if (m->tt != 0.){
    ctt = p->gd * m->tt;
  }else{
    ctt = 0.;
  }
  
  trace4("", cb, csw, ctt, cb+csw+ctt);
  d->y0.f1 = cb + csw + ctt;
  d->y0.f0 = d->y0.x * d->y0.f1;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
