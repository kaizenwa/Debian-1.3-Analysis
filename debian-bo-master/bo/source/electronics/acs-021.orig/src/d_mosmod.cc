/*$Id: d_mosmod.cc,v 11.30 96/03/17 19:20:49 al Exp $ -*- C++ -*-
 * mos model basics
 * netlist syntax:
 * device:  mxxxx d g s b mname <device args> <model card args>
 * model:   .model mname NMOS <args>
 *	or  .model mname PMOS <args>
 */
#include "ap.h"
#include "d_diode.h"
#include "d_mos.h"
#include "error.h"
#include "io.h"
#include "l_lib.h"
#include "u_opt.h"
/*--------------------------------------------------------------------------*/
//		MOS_COMMON::MOS_COMMON();
//		MODEL_MOS::MODEL_MOS(const char*);
//	void	MODEL_MOS::parse(CS& cmd);
// 	void	MODEL_MOS::print(int,int)const;
//	void	MODEL_MOS::tr_eval(CARD*)const;
/*--------------------------------------------------------------------------*/
const double DEFAULT_vto   = 0.0;		/* last resort defaults */
const double DEFAULT_gamma = 0.0;		/* in case can't calculate */
const double DEFAULT_phi   = 0.6;
const double DEFAULT_cj    = 0.0;
const double DEFAULT_tox   = 1e-7;
const double DEFAULT_nss   = 0.0;
const double DEFAULT_ucrit = (1e4*ICM2M);
const double DEFAULT_neff  = 1.0;
/*--------------------------------------------------------------------------*/
MOS_COMMON::MOS_COMMON()
{
  lo	= NOT_INPUT;
  wo	= NOT_INPUT;
  ad_in	= NOT_INPUT;
  as_in	= NOT_INPUT;
  pd	= 0.0;
  ps	= 0.0;
  nrd	= 1.0;
  nrs	= 1.0;
  off = icset = false;
  for (int i = 0;  i < mNUM_INIT_COND;  i++)
    ic[i] = 0.;
}
/*--------------------------------------------------------------------------*/
MODEL_MOS::MODEL_MOS(const char *name):MODEL_DIODE(name)
{
  			/* inherit, then override diode defaults */
  cjo    = NOT_INPUT;	/* override 0.0 */
  pb     = 0.8;		/* override 1.0 */
  fcpb   = fc * pb;
			/* mos (not diode) defaults */
  vto    = NOT_INPUT;	/*calc*/
  kp     = NOT_INPUT;	/*calc*/
  gamma  = NOT_INPUT;	/*calc*/
  phi    = NOT_INPUT;	/*calc*/
  lambda = NOT_INPUT;
  rd     = NOT_INPUT;
  rs     = NOT_INPUT;
  cbd    = NOT_INPUT;
  cbs    = NOT_INPUT;
  is     = NOT_INPUT;
  cgso   = 0.0;
  cgdo   = 0.0;
  cgbo   = 0.0;
  rsh    = NOT_INPUT;
  js     = NOT_INPUT;
  tox    = DEFAULT_tox;
  nsub   = NOT_INPUT;	/*flag*/
  nss    = DEFAULT_nss;
  nfs    = 0.0;
  xj     = NOT_INPUT;
  ld     = 0.0;
  uo     = (600.*CM2M2);
  ucrit  = DEFAULT_ucrit;
  uexp   = NOT_INPUT;
  utra   = NOT_INPUT;
  vmax   = NOT_INPUT;
  neff   = DEFAULT_neff;
  kf     = NOT_INPUT;
  af     = NOT_INPUT;
  delta  = 0.0;
  theta  = 0.0;
  eta    = 0.0;
  kappa  = 0.2;
  level  = 2;
  tpg    = gSAME;
  polarity = pN;
  alpha	 = NOT_INPUT;
  xd     = NOT_INPUT;	/*calc if nsub spec, else not*/
  cox    = (E_OX / tox);
  vfb    = NOT_INPUT;
  vbi    = NOT_INPUT;
  xwb    = .25e-6;
  vbp    = NOT_INPUT;
  cfsox  = NOT_INPUT;
  delta3 = 0.0;
  sqrt_phi = NOT_INPUT;
  phi_sqrt_phi = NOT_INPUT;
  calc.vto = calc.kp = calc.gamma = calc.phi = calc.cj = false;
}
/*--------------------------------------------------------------------------*/
void MODEL_MOS::parse(CS& cmd)
{
  cmd.skiparg();	/* skip known ".model" */
  cmd.ctostr(label, LABELEN, TOKENTERM);
  cmd.stuck();
  /**/ set(cmd, "NMos", &polarity, pN)
    || set(cmd, "PMos", &polarity, pP);
  if (cmd.stuck()){
    cmd.check(bWARNING);
  }
  cmd.skiplparen();
  cmd.stuck();
  do{
    cmd.get("LEvel",  &level);
    cmd.get("VTO",    &vto,	mSCALE, (double)polarity);
    cmd.get("KP",     &kp);
    cmd.get("GAmma",  &gamma);
    cmd.get("PHI",    &phi,	mPOSITIVE);
    cmd.get("LAmbda", &lambda);
    cmd.get("RD",     &rd);
    cmd.get("RS",     &rs);
    cmd.get("CBD",    &cbd);
    cmd.get("CBS",    &cbs);
    cmd.get("IS",     &is);
    cmd.get("PB",     &pb,	mPOSITIVE);
    cmd.get("CGSo",   &cgso);
    cmd.get("CGDo",   &cgdo);
    cmd.get("CGBo",   &cgbo);
    cmd.get("RSH",    &rsh);
    cmd.get("CJ",     &cjo);
    cmd.get("MJ",     &mj);
    cmd.get("CJSw",   &cjsw);
    cmd.get("MJSw",   &mjsw);
    cmd.get("JS",     &js);
    cmd.get("TOX",    &tox,	mPOSITIVE);
    cmd.get("NSUb",   &nsub,	mSCALE, ICM2M3);
    cmd.get("NSS",    &nss,	mSCALE, ICM2M2);
    cmd.get("NFS",    &nfs,	mSCALE, ICM2M2);
    cmd.get("TPG",    (int*)&tpg);
    cmd.get("XJ",     &xj,	mPOSITIVE);
    cmd.get("LD",     &ld);
    cmd.get("UO",     &uo,	mSCALE, CM2M2);
    cmd.get("UCRit",  &ucrit,	mSCALE, ICM2M);
    cmd.get("UEXp",   &uexp);
    cmd.get("UTRa",   &utra);
    cmd.get("VMAx",   &vmax);
    cmd.get("NEFf",   &neff,	mPOSITIVE);
    cmd.get("KF",     &kf);
    cmd.get("AF",     &af);
    cmd.get("FC",     &fc);
    cmd.get("DELta",  &delta);
    cmd.get("THEta",  &theta);
    cmd.get("ETA",    &eta);
    cmd.get("KAPpa",  &kappa);
  }while (cmd.more() && !cmd.stuck());
  cmd.skiprparen();
  cmd.check(bWARNING);
  
  if ((rs == NOT_INPUT)  &&  (rd != NOT_INPUT)){
    error(bWARNING, "%s: rd input, but not rs. setting rs = 0.\n",
	  printlabel());
    rs = 0.;
  }else if ((rd == NOT_INPUT)  &&  (rs != NOT_INPUT)){
    error(bWARNING, "%s: rs input, but not rd. setting rd = 0.\n",
	  printlabel());
    rd = 0.;
  }
  
  if ((rsh != NOT_INPUT)  &&  (rd != NOT_INPUT)){
    error(bWARNING, "%s: rsh - rs - rd conflict: using %s\n",
	  printlabel(),
	  ((rd <= 0.)  &&  (rs <= 0.)) ? "rsh" : "rs,rd" );
  }else if ((rsh == NOT_INPUT)  &&  (rd == NOT_INPUT)){
    rsh = 0.;
  }
  
  if (tox == 0.){
    error(bWARNING, "%s: tox == 0: using %s\n",
	  printlabel(),
	  ftos(DEFAULT_tox,        "", 7, 0));
    tox = DEFAULT_tox;
  }
  cox = E_OX / tox;
  
  if (kp == NOT_INPUT){
    kp = uo * cox;
    calc.kp = true;
  }
  
  if (is == NOT_INPUT  &&  js == NOT_INPUT){
    is = mDEFAULT_is;
  }else if (is != NOT_INPUT  &&  js != NOT_INPUT){
    error(bWARNING, "%s: is - js conflict\n", printlabel());
  }
  
  if (nsub != NOT_INPUT){
    nsub = fabs(nsub);
    if (nsub < NI){
      error(bWARNING, "%s: nsub < ni\n", printlabel());
      nsub = NI;
    }
    if (cjo == NOT_INPUT){
      cjo = sqrt(E_SI * Q * nsub / (2. * pb ));
      calc.cj = true;
    }
    if (phi == NOT_INPUT){
      phi = (2.*K/Q)*OPT::tempamb*log(nsub/NI);
      if (phi < .1){
	error(bWARNING, "%s: calculated phi too small, using .1\n",
	      printlabel());
	phi = .1;
      }
      calc.phi = true;
    }
    if (gamma == NOT_INPUT){
      gamma = sqrt(2. * E_SI * Q * nsub) / cox;
      calc.gamma = true;
    }
    if (vto == NOT_INPUT){
      double egap = 
	1.16 - (7.02e-4*OPT::tempamb*OPT::tempamb) / (OPT::tempamb+1108.);
      double phi_ms = (tpg == gMETAL)
	? -.05 - (egap + polarity * phi) / 2.
	: -polarity * (tpg * egap + phi) / 2.;
      
      vfb = phi_ms - Q * nss / cox;
      vto = vfb + polarity * (phi + gamma * sqrt(phi));
      calc.vto = true;
    }
    alpha = (2. * E_SI / Q) / nsub;
    xd = sqrt(alpha);
    xwb = xd * sqrt(pb);
  }else{
    if (vto == NOT_INPUT)
      vto = DEFAULT_vto;
    if (gamma == NOT_INPUT)
      gamma = DEFAULT_gamma;
    if (phi == NOT_INPUT)
      phi = DEFAULT_phi;
    if (cjo == NOT_INPUT)
      cjo = DEFAULT_cj;
    alpha = 0.;
    xd = xwb / sqrt(pb);
  }
  sqrt_phi = sqrt(phi);
  phi_sqrt_phi = phi * sqrt_phi;
  if (lambda != NOT_INPUT  &&  lambda > .2)
    error(bWARNING, "%s: lambda too large (> .2)\n", printlabel());
  if (vfb == NOT_INPUT)
    vfb = vto - (phi + gamma * sqrt_phi);
  vbi = vfb + phi;
  vbp = ucrit * E_SI / cox;
  cfsox = Q * nfs / cox;
  fcpb = fc * pb;

  delta3 = delta * kPId2 * E_SI / cox;	/* level 3 stuff */
}
/*--------------------------------------------------------------------------*/
void MODEL_MOS::print(int where, int)const
{
  mprintf(where, ".model %s ", label);
  mprintf(where, " %s (",     (polarity < 0) ? "pmos" : "nmos");
  mprintf(where, " level=%d ", level);
  if (!calc.vto)
    mprintf(where, " vto=%s ",   ftos(vto*polarity,"", 7, 0));
  if (!calc.kp)
    mprintf(where, " kp=%s ",    ftos(kp,         "", 7, 0));
  if (!calc.gamma)
    mprintf(where, " gamma=%s ", ftos(gamma,      "", 7, 0));
  if (!calc.phi)
    mprintf(where, " phi=%s ",   ftos(phi,        "", 7, 0));
  if (lambda != NOT_INPUT)
    mprintf(where, " lambda=%s ",ftos(lambda,     "", 7, 0));
  if (rd != NOT_INPUT)
    mprintf(where, " rd=%s ",    ftos(rd,         "", 7, 0));
  if (rs != NOT_INPUT)
    mprintf(where, " rs=%s ",    ftos(rs,         "", 7, 0));
  if (cbd != NOT_INPUT)
    mprintf(where, " cbd=%s ",   ftos(cbd,        "", 7, 0));
  if (cbs != NOT_INPUT)
    mprintf(where, " cbs=%s ",   ftos(cbs,        "", 7, 0));
  if (is != NOT_INPUT)
    mprintf(where, " is=%s ",    ftos(is,         "", 7, 0));
  mprintf(where, " pb=%s ",    ftos(pb ,         "", 7, 0));
  mprintf(where, " cgso=%s ",  ftos(cgso,       "", 7, 0));
  mprintf(where, " cgdo=%s ",  ftos(cgdo,       "", 7, 0));
  mprintf(where, " cgbo=%s ",  ftos(cgbo,       "", 7, 0));
  if (rsh != NOT_INPUT)
    mprintf(where, " rsh=%s ",   ftos(rsh,        "", 7, 0));
  if (!calc.cj)
    mprintf(where, " cj=%s ",    ftos(cjo,      "", 7, 0));
  mprintf(where, " mj=%s ",    ftos(mj,      "", 7, 0));
  mprintf(where, " cjsw=%s ",  ftos(cjsw,    "", 7, 0));
  mprintf(where, " mjsw=%s ",  ftos(mjsw,    "", 7, 0));
  if (js != NOT_INPUT)
    mprintf(where, " js=%s ",    ftos(js,         "", 7, 0));
  mprintf(where, " tox=%s ",   ftos(tox,	     "", 7, 0));
  if (nsub != NOT_INPUT)
    mprintf(where, " nsub=%s ",  ftos(nsub/ICM2M3,"", 7, 0));
  if (nss != DEFAULT_nss  ||  nsub != NOT_INPUT)
    mprintf(where, " nss=%s ",   ftos(nss /ICM2M2,"", 7, 0));
  mprintf(where, " nfs=%s ",   ftos(nfs /ICM2M2,"", 7, 0));
  mprintf(where, " tpg=%d ",   tpg);
  if (xj != NOT_INPUT)
    mprintf(where, " xj=%s ",    ftos(xj,         "", 7, 0));
  mprintf(where, " ld=%s ",    ftos(ld,         "", 7, 0));
  mprintf(where, " uo=%s ",    ftos(uo   /CM2M2,"", 7, 0));
  if (ucrit != DEFAULT_ucrit  ||  uexp != NOT_INPUT)
    mprintf(where, " ucrit=%s ", ftos(ucrit/ICM2M,"", 7, 0));
  if (uexp != NOT_INPUT)
    mprintf(where, " uexp=%s ",  ftos(uexp,       "", 7, 0));
  if (utra != NOT_INPUT)
    mprintf(where, " utra=%s ",  ftos(utra,       "", 7, 0));
  if (vmax != NOT_INPUT)
    mprintf(where, " vmax=%s ",  ftos(vmax,       "", 7, 0));
  if (neff != DEFAULT_neff  ||  lambda == NOT_INPUT)
    mprintf(where, " neff=%s ",  ftos(neff,       "", 7, 0));
  if (kf != NOT_INPUT)
    mprintf(where, " kf=%s ",    ftos(kf,         "", 7, 0));
  if (af != NOT_INPUT)
    mprintf(where, " af=%s ",    ftos(af,         "", 7, 0));
  mprintf(where, " fc=%s ",    ftos(fc,      "", 7, 0));
  mprintf(where, " delta=%s ", ftos(delta,      "", 7, 0));
  if (level == 3){
    mprintf(where, " theta=%s ", ftos(theta,      "", 7, 0));
    mprintf(where, " eta=%s ",   ftos(eta,        "", 7, 0));
    mprintf(where, " kappa=%s ", ftos(kappa,      "", 7, 0));
  }
  mprintf(where, ")\n*+(");
  mprintf(where, "* vfb=%s ",  ftos(vfb,        "", 7, 0));
  if (calc.vto)
    mprintf(where, "* vto=%s ",   ftos(vto*polarity,        "", 7, 0));
  if (calc.kp)
    mprintf(where, "* kp=%s ",    ftos(kp,         "", 7, 0));
  if (calc.gamma)
    mprintf(where, "* gamma=%s ", ftos(gamma,      "", 7, 0));
  if (calc.phi)
    mprintf(where, "* phi=%s ",   ftos(phi,        "", 7, 0));
  if (calc.cj)
    mprintf(where, "* cj=%s ",    ftos(cjo,         "", 7, 0));
  mprintf(where, ")\n");
}
/*--------------------------------------------------------------------------*/
void MODEL_MOS::tr_eval(COMPONENT* brh)const
{
  DEV_MOS* d = (DEV_MOS*)brh;
  assert(d);
  switch (level){
    default:
      error(bDANGER, "%s: level %s not implemented, using 1\n",
	    printlabel(), level);
    case 1:	eval_mos1(d);	break;
    case 2:	eval_mos2(d);	break;
    case 3:	eval_mos3(d);	break;
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
