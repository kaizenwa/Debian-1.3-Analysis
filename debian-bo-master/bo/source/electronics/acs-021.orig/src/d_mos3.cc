/*$Id: d_mos3.cc,v 11.28 96/03/03 23:07:17 al Exp $ -*- C++ -*-
 * mos model equations: spice level 3 equivalent
 * translated from spice 2g6, then cleaned up
 */
#include "d_mos.h"
#include "l_compar.h"
#include "io_trace.h"
/*--------------------------------------------------------------------------*/
//	void	MODEL_MOS::eval_mos3(CARD*)const;
/*--------------------------------------------------------------------------*/
#define short_channel	(m->xj != NOT_INPUT  &&  m->xj > 0.)
#define do_subthreshold	(m->nfs != 0.)
#define use_vmax	(m->vmax != NOT_INPUT)
/*--------------------------------------------------------------------------*/
void MODEL_MOS::eval_mos3(DEV_MOS *d)const
{
  assert(d);
  const MOS_COMMON* c = (const MOS_COMMON*)d->common;
  assert(c);
  const MODEL_MOS* m = this;
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  trace1(d->printlabel(), d->evaliter);
  trace3("", d->vds, d->vgs, d->vbs);
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* square root term */
  double sarg, v_phi_s, dsarg_dvbs;
  {
    if (d->vbs <= 0.){
      v_phi_s = m->phi - d->vbs;
      sarg = sqrt(v_phi_s);
      dsarg_dvbs = -.5 / sarg;
      d->sbfwd = false;
      trace3("sb-ok", sarg, v_phi_s, dsarg_dvbs);
    }else{
      assert(0);
      sarg = m->sqrt_phi / (d->vbs / (2 * m->phi) + 1.);
      v_phi_s = sarg * sarg;
      dsarg_dvbs = -v_phi_s / (2 * m->phi_sqrt_phi);
      d->sbfwd = true;
      trace3("***sb-reversed***", sarg, v_phi_s, dsarg_dvbs);
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
  /* short channel effect, body effect */
  double fbody, dfbody_dvbs, qbonco, dqbdvb;
  {
    double fshort, dfs_dvbs;
    assert(m->xd != NOT_INPUT);
    assert(m->xd > 0.);
    if (short_channel){
      static const double d[3] = {.0631353, .8013292, -.01110777};
      double wp = m->xd * sarg;
      double wp_xj = wp / m->xj;
      double wc_xj = d[0] + d[1] * wp_xj + d[2] * wp_xj * wp_xj;
      double ld_xj = m->ld / m->xj;
      double xj_le = m->xj / c->le;
      
      double arga = wc_xj + ld_xj;
      double argc = wp_xj / (wp_xj + 1.);
      double argb = sqrt(1. - argc * argc);
      fshort = 1. - xj_le * (arga * argb - ld_xj);

      double dwp_dvbs = m->xd * dsarg_dvbs;
      double darga_dvbs = (d[1] + d[2] * (wp_xj + wp_xj)) * dwp_dvbs / m->xj;
      double dargb_dvbs = -argc * argc * (1. - argc) * dwp_dvbs / (argb * wp);
      dfs_dvbs = -xj_le * (darga_dvbs * argb + arga * dargb_dvbs);
      trace2("short-channel", fshort, dfs_dvbs);
    }else{
      assert(0);
      fshort = 1.;
      dfs_dvbs = 0.;
      trace2("not-short-channel", fshort, dfs_dvbs);
    }

    double gamma_fs = m->gamma * fshort;
    double fbodys = gamma_fs * .5 / (2 * sarg);
    double fnarrw = m->delta3 / c->we;
    trace3("", gamma_fs, fbodys, fnarrw);

    fbody = fbodys + fnarrw;
    dfbody_dvbs = -fbodys * dsarg_dvbs / sarg + fbodys * dfs_dvbs / fshort;
    trace2("", fbody, dfbody_dvbs);

    qbonco = gamma_fs * sarg + fnarrw * v_phi_s;
    dqbdvb = gamma_fs * dsarg_dvbs + m->gamma * dfs_dvbs * sarg - fnarrw;
    trace2("", qbonco, dqbdvb);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* threshold voltage */
  double vth, dvth_dvds, dvth_dvbs;
  {
    double vbix = m->vbi - c->sigma * d->vds;
    vth = vbix + qbonco;
    dvth_dvds = -(c->sigma);
    dvth_dvbs = dqbdvb;
    trace3("", vth, dvth_dvds, dvth_dvbs);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* joint weak inversion and strong inversion */
  /* von */
  double xn, vtxn, dxn_dvbs, dvon_dvds, dvon_dvbs;
  {
    if (do_subthreshold){
      double vt = (K/Q) * SIM::temp;
      xn = 1. + m->cfsox + qbonco / (2 * v_phi_s);
      vtxn = vt * xn;
      dxn_dvbs  = dqbdvb / (2*v_phi_s) - qbonco*dsarg_dvbs / (v_phi_s*sarg);
      trace3("do_sub", xn, vtxn, dxn_dvbs);

      d->von  = vth + vtxn;
      dvon_dvds = dvth_dvds;
      dvon_dvbs = dvth_dvbs + vt * dxn_dvbs;
      d->vgst = d->vgs - d->von;
      trace4("", d->von, dvon_dvds, dvon_dvbs, d->vgst);

      d->subthreshold = (d->vgs < d->von);
      d->cutoff = false;
    }else{
      xn = vtxn = dxn_dvbs = dvon_dvds = dvon_dvbs = 0.;
      d->von = vth;
      d->vgst = d->vgs - d->von;
      trace2("no_sub", vtxn, dxn_dvbs);
      trace4("", d->von, dvon_dvds, dvon_dvbs, d->vgst);
      
      d->subthreshold = false;
      d->cutoff = (d->vgs <= d->von);
      if (d->cutoff){
	trace0("***** cut off *****");
	assert(0);
	d->vdsat = 0.;
	d->ids = 0.;
	d->gm = 0.;
	d->gds = 0.;
	d->gmb = 0.;
	return;
      }
    }
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* device is on */
  /* mobility modulation by gate voltage */
  double vc, onfg, us, dfg_dvgs, dfg_dvds, dfg_dvbs, beta;
  {
    double vgsx = (d->subthreshold) ? d->von : d->vgs;
    vc = vgsx - vth;
    onfg = m->theta * vc + 1.;
    double fgate = 1. / onfg;
    us = m->uo * fgate;
    trace4("", vc, onfg, fgate, us);

    dfg_dvgs = -(m->theta) * fgate * fgate;
    dfg_dvds = -dfg_dvgs * dvth_dvds;
    dfg_dvbs = -dfg_dvgs * dvth_dvbs;
    trace3("", dfg_dvgs, dfg_dvds, dfg_dvbs);

    beta = c->beta * fgate;
    trace2("", c->beta, beta);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* saturation voltage */
  /* vdsat, saturated */
  double dvdsat_dvgs, dvdsat_dvds, dvdsat_dvbs;
  double onvdsc, vdsx;
  {
    double onfbdy = 1. / (fbody + 1.);
    double dvsdga = onfbdy;
    d->vdsat = vc * onfbdy;
    trace2("novm", d->vdsat, dvsdga);
    
    if (use_vmax){
      double vdsc = c->le * m->vmax / us;
      double argb = sqrt(d->vdsat * d->vdsat + vdsc * vdsc);
      d->vdsat += vdsc - argb;
      dvsdga *= (1. - d->vdsat / argb);
      trace2("vmax", d->vdsat, dvsdga);
      dvdsat_dvgs = dvsdga - (1. - vdsc / argb) * vdsc * dfg_dvgs * onfg;
      onvdsc = 1. / vdsc;
    }else{
      dvdsat_dvgs = dvsdga;
      onvdsc = NOT_VALID;
    }
    d->saturated = (d->vds > d->vdsat);
    vdsx =  (d->saturated) ? d->vdsat : d->vds;
    trace3("", d->vdsat, vdsx, onvdsc);

    dvdsat_dvds = -dvdsat_dvgs * dvth_dvds;
    dvdsat_dvbs = -dvdsat_dvgs * dvth_dvbs - d->vdsat * dfbody_dvbs * dvsdga;
    trace3("", dvdsat_dvgs, dvdsat_dvds, dvdsat_dvbs);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* short cut exit if vds == 0 */
  if (vdsx == 0.){ /*900*/
    trace2("***** vdsx == 0 *****", d->vdsat, d->vds);
    d->ids = 0.;
    d->gm = 0.;
    d->gds = beta * vc;
    d->gmb = 0.;
    if (d->subthreshold){
      d->gds *= exp(d->vgst / vtxn);
    }
    return;
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* normalized drain current */
  {
    double cdo = vc - (fbody + 1.) * .5 * vdsx;
    double dcodvb = -dvth_dvbs - dfbody_dvbs * .5 * vdsx;
    trace3("", c->beta, cdo, dcodvb);

    d->gm  = vdsx;
    d->gds = vc - (fbody + 1. + dvth_dvds) * vdsx;
    d->gmb = dcodvb * vdsx;
    d->ids = cdo * vdsx;
    trace4("1", d->ids, d->gm, d->gds, d->gmb);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* scale, but without velocity saturation effect */
  {
    double cd1 = c->beta * d->ids;
    d->gm  *= beta; d->gm  += dfg_dvgs * cd1;
    d->gds *= beta; d->gds += dfg_dvds * cd1;
    d->gmb *= beta;
    d->ids *= beta;
    trace4("2", d->ids, d->gm, d->gds, d->gmb);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* velocity saturation factor */
  double fdrain, dfd_dvgs, dfd_dvds, dfd_dvbs;
  if (use_vmax){
    assert(onvdsc != NOT_VALID);
    fdrain = 1. / (vdsx * onvdsc + 1.);
    double fd2 = fdrain * fdrain;
    double arga = fd2 * vdsx * onvdsc * onfg;
    dfd_dvgs = -dfg_dvgs * arga;
    dfd_dvds = -dfg_dvds * arga - fd2 * onvdsc;
    dfd_dvbs = -dfg_dvbs * arga;
    trace4("", fdrain, dfd_dvgs, dfd_dvds, dfd_dvbs);
    
    d->gm  *= fdrain; d->gm  += dfd_dvgs * d->ids;
    d->gds *= fdrain; d->gds += dfd_dvds * d->ids;
    d->gmb *= fdrain; d->gmb += dfd_dvbs * d->ids;
    d->ids *= fdrain;
    beta   *= fdrain;
    trace4("3", d->ids, d->gm, d->gds, d->gmb);
  }else{
    fdrain = 0.;	/* used only if use_vmax */
    dfd_dvgs = 0.;
    dfd_dvds = 0.;
    dfd_dvbs = 0.;
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* channel length modulation */
  double gds0;
  if (d->saturated){
    double d_l, dl_dvd;
    double ddl_dvgs, ddl_dvds, ddl_dvbs;
    if (m->alpha == 0.){
      assert(0);
      d_l = dl_dvd = ddl_dvgs = ddl_dvds = ddl_dvbs = 0.;
    }else if (use_vmax){	/* use_vmax && m->alpha != 0 */
      double gdsat = d->ids * (1. - fdrain) * onvdsc;
      gdsat = max(1e-12,gdsat);
      double gdoncd = gdsat / d->ids;
      double gdonfd = gdsat / (1. - fdrain);
      double gdonfg = gdsat * onfg;
      double dgdvg = gdoncd * d->gm  - gdonfd * dfd_dvgs + gdonfg * dfg_dvgs;
      double dgdvd = gdoncd * d->gds - gdonfd * dfd_dvds + gdonfg * dfg_dvds;
      double dgdvb = gdoncd * d->gmb - gdonfd * dfd_dvbs + gdonfg * dfg_dvbs;
      
      double emax = d->ids / (c->le * gdsat);
      double emax_o_ids   = emax / d->ids;
      double emax_o_gdsat = emax / gdsat;
      double demax_dvgs = emax_o_ids * d->gm  - emax_o_gdsat * dgdvg;
      double demax_dvds = emax_o_ids * d->gds - emax_o_gdsat * dgdvd;
      double demax_dvbs = emax_o_ids * d->gmb - emax_o_gdsat * dgdvb;
      
      double arga = emax * .5 * m->alpha;
      double argc = m->kappa * m->alpha;
      double argb = sqrt(arga * arga + argc * (d->vds - d->vdsat));
      d_l = argb - arga;
      dl_dvd = argc / (argb + argb);
      double dl_demax = (arga / argb - 1.) * .5 * m->alpha;
      ddl_dvgs = dl_demax * demax_dvgs;
      ddl_dvds = dl_demax * demax_dvds - dl_dvd;
      ddl_dvbs = dl_demax * demax_dvbs;
    }else{
      d_l = sqrt(m->kappa * (d->vds - d->vdsat) * m->alpha);
      dl_dvd = d_l * .5 / (d->vds - d->vdsat);
      ddl_dvgs = 0.;
      ddl_dvds = -dl_dvd;
      ddl_dvbs = 0.;
    }
    
    if (d_l > c->le * .5){		/* punch through approximation */
      d->punchthru = true;
      d_l = c->le - c->le*c->le / (d_l*4.);
      double arga = (c->le-d_l) * (c->le-d_l) * 4. / (c->le*c->le);
      ddl_dvgs *= arga;
      ddl_dvds *= arga;
      ddl_dvbs *= arga;
      dl_dvd *= arga;
    }else{
      d->punchthru = false;
    }

    if (m->alpha != 0){
      double lfact = 1. / (1. - d_l / c->le);
      d->ids *= lfact;
      double diddl = d->ids / (c->le - d_l);
      d->gm   = d->gm  * lfact + diddl * ddl_dvgs;
      gds0    = d->gds * lfact + diddl * ddl_dvds;
      d->gmb  = d->gmb * lfact + diddl * ddl_dvbs;
      d->gm  += gds0 * dvdsat_dvgs;
      d->gmb += gds0 * dvdsat_dvbs;
      d->gds  = gds0 * dvdsat_dvds + diddl * dl_dvd;
    }else{
      gds0 = 0;
    }
    trace2("", d_l, dl_dvd);
    trace3("", ddl_dvgs, ddl_dvds, ddl_dvbs);
    trace3("4", d->ids, gds0, d_l);
  }else{
    d->punchthru = false;
    gds0 = 0;				/* not saturated */
  }
  trace4("4", d->ids, d->gm, d->gds, d->gmb);
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* weak inversion -- subthreshold region */
  if (d->subthreshold){
    double wfact = exp(d->vgst / vtxn);
    d->ids *= wfact;
    double gms = d->gm * wfact;
    double gmw = d->ids / vtxn;
    trace2("subth", gmw, gms);

    d->gm   = gmw;
    d->gm  += gds0 * dvdsat_dvgs * wfact;
    d->gds *= wfact;
    d->gds += (gms - gmw) * dvon_dvds;
    d->gmb *= wfact;
    d->gmb += (gms - gmw) * dvon_dvbs - gmw * d->vgst * dxn_dvbs / xn;
    trace4("5", d->ids, d->gm, d->gds, d->gmb);
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
