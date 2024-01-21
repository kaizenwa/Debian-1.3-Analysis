/*$Id: d_mos2.cc,v 11.28 96/03/03 23:07:15 al Exp $ -*- C++ -*-
 * mos model equations: spice level 2 equivalent
 */
#include "d_mos.h"
#include "error.h"
#include "u_opt.h"
#include "l_compar.h"
#include "io.h"
#include "io_trace.h"
/*--------------------------------------------------------------------------*/
//	void	MODEL_MOS::eval_mos2(DEV_MOS*)const;
/*--------------------------------------------------------------------------*/
#define short_channel	(m->xj != NOT_INPUT  &&  m->xj > 0.)
#define do_subthreshold	(m->nfs != 0.)
/*--------------------------------------------------------------------------*/
void MODEL_MOS::eval_mos2(DEV_MOS *d)const
{
  assert(d);
  const MOS_COMMON* cc = (const MOS_COMMON*)d->common;
  assert(cc);
  const MODEL_MOS* m = this;
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  trace1(d->printlabel(), d->evaliter);
  trace3("", d->vds, d->vgs, d->vbs);
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  double v_phi_s = m->phi - d->vbs;
  double sarg, dsarg_dvbs, d2sdb2, sarg3;
  {
    if (d->vbs <= 0.){
      sarg = sqrt(v_phi_s);
      dsarg_dvbs = -.5 / sarg;
      d2sdb2 = .5 * dsarg_dvbs / v_phi_s;
      d->sbfwd = false;
      trace3("sb-ok", sarg, v_phi_s, dsarg_dvbs);
    }else{
      if (OPT::mosflags & 01000){
	sarg = m->sqrt_phi / (1. + .5 * d->vbs / m->phi);
	dsarg_dvbs = -.5 * sarg * sarg / m->phi_sqrt_phi;
	d2sdb2 = -dsarg_dvbs * sarg / m->phi_sqrt_phi;
      }else{
	sarg = m->sqrt_phi 
	    / (1. + .5 * d->vbs / m->phi 
	    	  + .375 * d->vbs * d->vbs / (m->phi * m->phi));
	dsarg_dvbs = (-.5 * sarg * sarg / m->phi_sqrt_phi) 
		* (1. + 1.5 * d->vbs / m->phi);
	d2sdb2 = (-dsarg_dvbs * sarg / m->phi_sqrt_phi)
		- (.75 * sarg / (m->phi_sqrt_phi * m->phi)) 
			* (2. * d->vbs * dsarg_dvbs + sarg);
      }
      d->sbfwd = true;
      trace3("***sb-reversed***", sarg, v_phi_s, dsarg_dvbs);
    }
    sarg3 = sarg*sarg*sarg;
    assert(sarg > 0.);
    assert(dsarg_dvbs < 0.);
    assert(uporder(-1/m->phi, d2sdb2, 1/m->phi));
    trace2("", d2sdb2, sarg3);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  double barg, dbarg_dvbs, d2bdb2;
  {
    double vbd = d->vbs - d->vds;
    double v_phi_d = m->phi - vbd;
    if (vbd <= 0.){
      barg = sqrt(v_phi_d);
      dbarg_dvbs = -.5 / barg;
      d2bdb2 = .5 * dbarg_dvbs / v_phi_d;
      d->dbfwd = false;
    }else{
      if (OPT::mosflags & 01000){
	barg = m->sqrt_phi / (1. + .5 * vbd / m->phi);
	dbarg_dvbs = -.5 * barg * barg / m->phi_sqrt_phi;
	d2bdb2 = -dbarg_dvbs * barg / m->phi_sqrt_phi;
        trace4("db-ok", barg, v_phi_d, dbarg_dvbs, d2bdb2);
      }else{
	barg = m->sqrt_phi 
	    / (1. + .5 * vbd / m->phi 
	    	  + .375 * vbd * vbd / (m->phi * m->phi));
	dbarg_dvbs = (-.5 * barg * barg / m->phi_sqrt_phi) 
		* (1. + 1.5 * vbd / m->phi);
	d2bdb2 = (-dbarg_dvbs * barg / m->phi_sqrt_phi)
		- (.75 * barg / (m->phi_sqrt_phi * m->phi)) 
			* (2. * vbd * dbarg_dvbs + barg);
      }
      d->dbfwd = true;
        trace4("***db-reversed***", barg, v_phi_d, dbarg_dvbs, d2bdb2);
    }
    assert(barg > 0.);
    assert(dbarg_dvbs < 0.);
    assert(uporder(-1/m->phi, d2bdb2, 1/m->phi));
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  double gamma_s, dgamma_s_dvds, dgamma_s_dvbs, dgddb2;
  {
    if (short_channel){
      double argxd = 1. + 2. * barg * m->xd / m->xj;
        assert(argxd > 0);
      double argd = sqrt(argxd);
      trace2("", argxd, argd);
      
      double alpha_d = cc->relxj * (argd - 1.);
      double dalpha_d_dvds = m->xd / (4. * cc->le * argd * barg);
      double dalpha_d_dvbs = -dalpha_d_dvds;
      trace3("", alpha_d, dalpha_d_dvds, dalpha_d_dvbs);
      
      double argxs = 1. + 2. * sarg * m->xd / m->xj;
        assert(argxs > 0);
      double args = sqrt(argxs);
      trace2("", argxs, args);
      
      double alpha_s = cc->relxj * (args - 1.);
      double dalpha_s_dvbs = -m->xd / (4. * cc->le * args * sarg);
      trace2("", alpha_s, dalpha_s_dvbs);
      
      gamma_s = m->gamma * (1. - alpha_s - alpha_d);
      dgamma_s_dvds = -m->gamma *  dalpha_d_dvds;
      dgamma_s_dvbs = -m->gamma * (dalpha_d_dvbs + dalpha_s_dvbs);
  
      double dasdb2 = -m->xd*(d2sdb2+dsarg_dvbs*dsarg_dvbs*m->xd/(m->xj*argxs))
	  / (cc->le*args);
      double daddb2 = -m->xd*(d2bdb2+dbarg_dvbs*dbarg_dvbs*m->xd/(m->xj*argxd))
	  / (cc->le*argd);
      dgddb2 = -.5 * m->gamma * (dasdb2 + daddb2);
  
      if (gamma_s <= 0. && m->gamma > 0. /* && !IO::suppresserrors */){
	error(bTRACE, "%s: gamma is negative\n", d->printlabel());
	error(bTRACE, "+   gamma_s=%g, alpha_s=%g, alpha_d=%g\n",
	      gamma_s,    alpha_s,    alpha_d);
      }
    }else{
      gamma_s = m->gamma;
      dgamma_s_dvds = dgamma_s_dvbs = 0.;
      dgddb2 = 0.;
    }
    trace4("", gamma_s, dgamma_s_dvds, dgamma_s_dvds, dgddb2);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* von, subthreshold, cutoff, vgst */
  double vt = (K/Q) * SIM::temp;
  double vc, vc_eta, dvon_dvbs;
  double xn, vtxn, dxn_dvbs;	/* subthreshold only */
  {
    double vbin = m->vbi + cc->eta_1 * v_phi_s;
    d->von = vbin + gamma_s * sarg;
    dvon_dvbs = -cc->eta_1 + dgamma_s_dvbs * sarg + gamma_s * dsarg_dvbs;
    trace3("guess", vbin, d->von, dvon_dvbs);
    
    if (do_subthreshold){
      double cdonco = -(gamma_s*dsarg_dvbs + dgamma_s_dvbs*sarg) + cc->eta_1;
      xn = 1. + m->cfsox + cdonco;
      vtxn = vt * xn;
      dxn_dvbs = 2. * dgamma_s_dvbs * dsarg_dvbs
			+ gamma_s * d2sdb2 + dgddb2 * sarg;
      trace3("do_sub", xn, vtxn, dxn_dvbs);

      d->von += vtxn;
      dvon_dvbs += vt * dxn_dvbs;
      d->vgst = d->vgs - d->von;
      trace3("", d->von, dvon_dvbs, d->vgst);

      d->subthreshold = (d->vgs < d->von);
      d->cutoff = false;
    }else{
      xn = vtxn = dxn_dvbs = 0.;
      d->vgst = d->vgs - d->von;
      trace3("no_sub", xn, vtxn, dxn_dvbs);
      trace3("", d->von, dvon_dvbs, d->vgst);

      d->subthreshold = false;
      d->cutoff = (d->vgs < d->von);
      if (d->cutoff){
	trace0("***** cut off *****");
	d->ids = 0.;
	d->gm = 0.;
	d->gds = 0.;
	d->gmb = 0.;
	return;
      }
    }
    double vgsx = (d->subthreshold) ? d->von : d->vgs;
    vc = vgsx - vbin;
    vc_eta = vc / cc->eta;
    trace3("", vgsx, vc, vc_eta);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  double ufact, duf_dvgs, duf_dvds, duf_dvbs, ueff;
  {
    if (m->uexp != NOT_INPUT  &&  d->vgst > m->vbp){
      ufact = pow(m->vbp/d->vgst, m->uexp);
      duf_dvgs = -ufact * m->uexp / d->vgst;
      duf_dvds = 0.;	/* wrong, but as per spice2 */
      duf_dvbs = dvon_dvbs * ufact * m->uexp / d->vgst;
    }else{
      ufact = 1.;
      duf_dvgs = duf_dvds = duf_dvbs = 0.;
    }
    ueff = m->uo * ufact;	/* ???? */
    trace2("", ufact, ueff);
    trace3("", duf_dvgs, duf_dvds, duf_dvbs);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* vdsat  according to Baum's Theory of scattering velocity saturation  */
  int use_vmax = m->vmax != NOT_INPUT;
  if (use_vmax){
    double y3;
    double xvalid = 0.;
    double x4[8];
    double gammad = gamma_s / cc->eta;
    double v1 = vc_eta + v_phi_s;
    double v2 = v_phi_s;
    double xv = m->vmax * cc->le / ueff;
    double a1 = gammad * (4./3.);
    double b1 = -2. * (v1+xv);
    double c1 = -2. * gammad * xv;			/* end of scope */
    double d1 = 2.*v1*(v2+xv) - v2*v2 - (4./3.)*gammad*sarg3;
    double a = -b1;					/* xv, v1, v2, sarg3 */
    double b = a1 * c1 - 4. * d1;
    double c = -d1 * (a1*a1 - 4.*b1) - c1*c1;
    double r = -a*a / 3. + b;
    double r3 = r*r*r;					/* r */
    double s = 2. * a*a*a / 27. - a*b / 3. + c;		/* b, c */
    double s2 = s*s;
    double p = s2 / 4. + r3 / 27.;			/* r */
    if (p < 0.){					/* p */
      double ro = pow((-r3 / 27), (1./6.));		/* s2, r3 */
      double fi = atan(-2. * sqrt(-p) / s);
      y3 = 2. * ro * cos(fi/3.) - a / 3.;
    }else{
      double p2 = sqrt(p);
      double p3 = pow((fabs(-s/2.+p2)), (1./3.));
      double p4 = pow((fabs(-s/2.-p2)), (1./3.));	/* s */
      y3 = p3 + p4 - a / 3.;				/* a */
    }

    int iknt = 0;
    if (a1*a1 / 4. - b1 + y3  < 0.  &&  y3*y3 / 4. - d1  < 0.){
      error(bWARNING,
	    "%s: internal error: a3,b4, a1=%g, b1=%g, y3=%g, d1=%g\n",
	    d->printlabel(),	      a1,    b1,    y3,    d1);
    }else{
      double a3 = sqrt(a1*a1 / 4. - b1 + y3);
      double b3 = sqrt(y3*y3 / 4. - d1);
      for (int i = 0;   i < 4;   i++){
	static const double sig1[4] = {1., -1., 1., -1.};
	static const double sig2[4] = {1., 1., -1., -1.};
	double a4 = a1 / 2. + sig1[i] * a3;
	double b4 = y3 / 2. + sig2[i] * b3;		/* y3 */
	double delta4 = a4*a4 / 4. - b4;
	if (delta4 >= 0.){
	  double sd4 = sqrt(delta4);
	  x4[iknt++] = - a4 / 2. + sd4;
	  x4[iknt++] = - a4 / 2. - sd4;			/* i */
	}
      }
    }

    int root_count = 0;
    for (int j = 0;   j < iknt;   j++){			/* iknt */
      if (x4[j] > 0.){
	double poly4 = x4[j]*x4[j]*x4[j]*x4[j]	/* ~= 0, used as check	*/
	  	+ a1 * x4[j]*x4[j]*x4[j]	/* roundoff error not	*/
	 	+ b1 * x4[j]*x4[j]		/* propagated, so ok	*/
		+ c1 * x4[j]
		+ d1;				/* a1, b1, c1, d1 */
	if (fabs(poly4) <= 1e-6){
	  root_count++;
	  if (root_count <= 1)			/* xvalid = min(x4[j]) */
	    xvalid=x4[j];
	  if (x4[j] <= xvalid)
	    xvalid=x4[j];				/* x4[], j */
	}
      }
    }
    if (root_count <= 0){				/* root_count */
      if (OPT::picky <= bTRACE || !IO::suppresserrors)
	error(bWARNING, "%s: Baum's theory rejected\n", d->printlabel());
      use_vmax = false;
      d->vdsat = 0.;
      trace1("use_vmax rejected", d->vdsat);
    }else{
      d->vdsat = xvalid*xvalid - v_phi_s;
      trace1("use_vmax", d->vdsat);
    }
  }else{
    d->vdsat = 0.;
    trace1("!use_vmax", d->vdsat);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* vdsat     according to Grove-Frohman equation  */
  double dvdsat_dvgs = NOT_VALID;
  double dvdsat_dvbs = NOT_VALID;
  if (!use_vmax){
    if (gamma_s > 0.){
      double argv = vc_eta + v_phi_s;
      if (argv > 0.){
	double gammad = gamma_s / cc->eta;
	double gammd2 = gammad * gammad;
	double arg1 = sqrt(1. + 4. * argv / gammd2);
	d->vdsat = vc_eta  +  gammd2 * (1.-arg1) / 2.;
	dvdsat_dvgs = (1. - 1./arg1) / cc->eta;
	dvdsat_dvbs = (gammad * (1.-arg1) + 2.*argv / (gammad*arg1))
	  / cc->eta * dgamma_s_dvbs
	    + 1./arg1 + cc->eta_1 * dvdsat_dvgs;
      }else{
	d->vdsat = 0.;
	dvdsat_dvgs = dvdsat_dvbs = 0.;
	if (!IO::suppresserrors){
	  error(bWARNING, "%s: argv is negative\n", d->printlabel());
	}
	trace2("argv<0", argv, vc);
      }
      trace3("!use_vmax, gamma>0", d->vdsat, dvdsat_dvgs, dvdsat_dvbs);
    }else{
      d->vdsat = vc_eta;
      dvdsat_dvgs = 1.;
      dvdsat_dvbs = 0.;
      trace3("!use_vmax, gamma<=0", d->vdsat, dvdsat_dvgs, dvdsat_dvbs);
    }
  }else{
    /* dvdsat_dvgs, dvdsat_dvbs   deferred */
    trace3("use_vmax", d->vdsat, dvdsat_dvgs, dvdsat_dvbs);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  if (d->vdsat < 0.){
    error(bWARNING, "%s: calculated vdsat (%g) < 0.  using vdsat = 0.\n",
	  d->printlabel(), d->vdsat);
    d->vdsat = 0.;
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  double bsarg, dbsarg_dvbs;
  {
    double vbdsat = d->vbs - d->vdsat;
    if (vbdsat <= 0.){
      double v_phi_ds = m->phi - vbdsat;
      bsarg = sqrt(v_phi_ds);
      dbsarg_dvbs = -.5 / bsarg;
    }else{
      bsarg = m->sqrt_phi / (1. + .5 * vbdsat / m->phi);
      dbsarg_dvbs = -.5 * bsarg * bsarg / m->phi_sqrt_phi;
    }
    trace3("", vbdsat, bsarg, dbsarg_dvbs);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* local   dvdsat_dvgs, dvdsat_dvbs   maybe */
  {
    if (use_vmax){
      double bodys = bsarg*bsarg*bsarg - sarg3;
      double gdbdvs =
	2. * gamma_s * (bsarg*bsarg*dbsarg_dvbs - sarg*sarg*dsarg_dvbs);
      double argv = vc_eta - d->vdsat;
      double vqchan = argv - gamma_s * bsarg;
      double dqdsat = -1. + gamma_s * dbsarg_dvbs;
      double vl = m->vmax * cc->le;
      double dfunds = vl * dqdsat - ueff * vqchan;
      double dfundg = (vl - ueff * d->vdsat) / cc->eta;
      double dfundb = -vl * (1. + dqdsat - cc->eta_1 / cc->eta)
	+ ueff * (gdbdvs - dgamma_s_dvbs * bodys / 1.5) / cc->eta;
      dvdsat_dvgs = -dfundg / dfunds;
      dvdsat_dvbs = -dfundb / dfunds;
      trace2("use_vmax", dvdsat_dvgs, dvdsat_dvbs);
    }else{
      /* dvdsat_dvgs, dvdsat_dvbs   already set */
      trace2("!use_vmax", dvdsat_dvgs, dvdsat_dvbs);
    }
    assert(dvdsat_dvgs != NOT_VALID);
    assert(dvdsat_dvbs != NOT_VALID);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  double  dl_dvgs, dl_dvds, dl_dvbs, clfact;
  {
    if (d->vds > 0.){
      if (m->lambda == NOT_INPUT){
	double dldsat;
	if (use_vmax){
	  double xdv = m->xd / sqrt(m->neff);
	  double xlv = m->vmax * xdv / (2. * ueff);
	  double argv = d->vds - d->vdsat;
	  if (argv < 0.)
	    argv = 0.;
	  double xls = sqrt(xlv*xlv + argv);
	  double dl = (xls-xlv) * xdv;
	  /* lambda = dl / (cc->le * d->vds); */
	  clfact = (1. - dl / cc->le);
	  dldsat = xdv / (2. * xls * cc->le);
	}else{
	  double argv = (d->vds - d->vdsat) / 4.;
	  double sargv = sqrt(1. + argv*argv);
	  if (argv + sargv >= 0.){
	    double dl = m->xd * sqrt(argv + sargv);
	    /* lambda = dl / (cc->le * d->vds); */
	    clfact = (1. - dl / cc->le);
	    /* dldsat = lambda * d->vds / (8. * sargv); */
	    dldsat = dl / (cc->le * 8. * sargv);
	  }else{
	    /* lambda = 0.; */
	    clfact = 1.;
	    dldsat = 0.;
	    error(bWARNING, "%s: internal error: vds(%g) < vdsat(%g)\n",
		  d->printlabel(), d->vds,   d->vdsat);
	  }
	}
	dl_dvgs =  dvdsat_dvgs * dldsat;
	dl_dvds =              - dldsat;
	dl_dvbs =  dvdsat_dvbs * dldsat;
      }else{
	/* lambda = m->lambda; */
	clfact = (1. - m->lambda * d->vds);
	dl_dvgs = dl_dvbs = 0.;
	dl_dvds = -m->lambda;
      }
    
      /* clfact = (1. - lambda * d->vds); */
      if (clfact < m->xwb/cc->le){
	double leff = m->xwb / (2. - (clfact * cc->le / m->xwb));
	double dfact = (leff * leff) / (m->xwb * m->xwb);
	clfact = leff / cc->le;
	dl_dvgs *= dfact;
	dl_dvds *= dfact;
	dl_dvbs *= dfact;
      }
    }else{  /* vds <= 0. */
      /* lambda = 0.; */
      clfact = 1.;
      dl_dvgs = dl_dvds = dl_dvbs = 0.;
      trace1("*** vds < 0 ***", d->vds);
    }
    trace4("", dl_dvgs, dl_dvds, dl_dvbs, clfact);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  /* ids, gm, gds, gmb */
  {
    d->saturated = (d->vds > d->vdsat);
    double vdsx =  (d->saturated) ? d->vdsat : d->vds;
    double bargx = (d->saturated) ? bsarg : barg;
    double body = bargx*bargx*bargx - sarg3;
    double expg = (d->subthreshold) ? Exp(d->vgst / vtxn) : 1.;
    trace4("", vdsx, bargx, body, expg);
        
    double beta = cc->beta * ufact / clfact;
    double ids_on = 
	  beta * ((vc - cc->eta_2 * vdsx) * vdsx  - (2./3.) * gamma_s * body);
    double didvds = beta * (vc  -  cc->eta * vdsx  -  gamma_s * bargx);
    trace4("", beta, ids_on, didvds, d->saturated);

    d->ids = ids_on * expg;
    
    d->gm = beta * vdsx;
    d->gm += ids_on * (duf_dvgs/ufact - dl_dvgs/clfact);
    if (d->saturated)
      d->gm += didvds * dvdsat_dvgs;
    if (d->subthreshold){
      d->gm = ids_on / vtxn;
      if (d->saturated)
	d->gm += didvds * dvdsat_dvgs;
      d->gm *= expg;
    }
    
    d->gds = (d->saturated) ? 0.: didvds;
    d->gds += ids_on * (duf_dvds/ufact - dl_dvds/clfact);
    if (short_channel)
      d->gds -= beta * (2./3.) * body * dgamma_s_dvds;
    if (d->subthreshold){
      double dxndvd = dgamma_s_dvds * dsarg_dvbs;
      double dodvds = dgamma_s_dvds * sarg + vt * dxndvd;
      double gmw = d->ids * d->vgst / (vtxn * xn);
      d->gds *= expg;
      d->gds -= d->gm * dodvds + gmw * dxndvd;
    }

    d->gmb = beta * (cc->eta_1 * vdsx - gamma_s * (sarg - bargx));
    d->gmb += ids_on * (duf_dvbs/ufact - dl_dvbs/clfact);
    if (short_channel)
      d->gmb -= beta * (2./3.) * body * dgamma_s_dvbs;
    if (d->saturated)
      d->gmb += didvds * dvdsat_dvbs;
    if (d->subthreshold){
      double gmw = d->ids * d->vgst / (vtxn * xn);
      d->gmb += beta * dvon_dvbs * vdsx;
      d->gmb *= expg;
      d->gmb -= d->gm * dvon_dvbs + gmw * dxn_dvbs;
    }
    trace4("", d->ids, d->gm, d->gds, d->gmb);
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
