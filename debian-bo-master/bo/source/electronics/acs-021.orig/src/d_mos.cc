/*$Id: d_mos.cc,v 11.35 96/03/22 18:09:53 al Exp $ -*- C++ -*-
 * mos device basics
 * netlist syntax:
 * device:  mxxxx d g s b mname <device args> <model card args>
 * model:   .model mname NMOS <args>
 *	or  .model mname PMOS <args>
 */
#include "ap.h"
#include "d_admit.h"
#include "d_cap.h"
#include "d_cs.h"
#include "d_res.h"
#include "d_diode.h"
#include "d_mos.h"
#include "d_subckt.h"
#include "d_vccs.h"
#include "error.h"
#include "io.h"
#include "l_lib.h"
#include "u_opt.h"
#include "u_status.h"
#include "s__.h"
#include "l_compar.h"
#include "declare.h"	/* newnode_model ... */
#include "io_trace.h"
/*--------------------------------------------------------------------------*/
//		DEV_MOS::DEV_MOS();
//		DEV_MOS::DEV_MOS(CONST DEV_MOS& p);
//	void	DEV_MOS::parse(CS& cmd);
// 	void	DEV_MOS::print(int,int)const;
//	double	DEV_MOS::probe_tr_num(const char *what)const;
//	void	DEV_MOS::expand();
// 	bool	DEV_MOS::dotr();
//	void	DEV_MOS::limit_mos(double,double,double);
/*--------------------------------------------------------------------------*/
int DEV_MOS::Count = 0;
static EVAL_MOS_Ids Eval_Ids;
static EVAL_MOS_Gmf Eval_Gmf;
static EVAL_MOS_Gmr Eval_Gmr;
static EVAL_MOS_Gds Eval_Gds;
static EVAL_MOS_Gmbf Eval_Gmbf;
static EVAL_MOS_Gmbr Eval_Gmbr;
static EVAL_MOS_Cgb Eval_Cgb;
static EVAL_MOS_Cgd Eval_Cgd;
static EVAL_MOS_Cgs Eval_Cgs;
static MOS_COMMON Default_MOS;
/*--------------------------------------------------------------------------*/
DEV_MOS::DEV_MOS()
{
  attach_common(&Default_MOS);
  devclass = OTHERDEVICE;
  ids = gm = gds = gmb = vgs = vds = vbs = vdsat = vgst = von = 0.;
  cutoff = subthreshold = saturated = reversed = dbfwd = sbfwd = 
  	punchthru = false;
  Rs = Rd = NULL;
  Ddb = Dsb = NULL;
  Cgs = Cgd = Cgb = NULL;
  Gmbf = Gmbr = Gmf = Gmr = NULL;
  Yds = NULL;
  Ids = NULL;
  ++Count;
}
/*--------------------------------------------------------------------------*/
DEV_MOS::DEV_MOS(CONST DEV_MOS& p):BASE_SUBCKT(p)
{
  ids = gm = gds = gmb = vgs = vds = vbs = vdsat = vgst = von = 0.;
  cutoff = subthreshold = saturated = reversed = dbfwd = sbfwd = 
  	punchthru = false;
  Rs = Rd = NULL;
  Ddb = Dsb = NULL;
  Cgs = Cgd = Cgb = NULL;
  Gmbf = Gmbr = Gmf = Gmr = NULL;
  Yds = NULL;
  Ids = NULL;
  ++Count;
}
/*--------------------------------------------------------------------------*/
void DEV_MOS::parse(CS& cmd)
{
  assert(common);
  MOS_COMMON* c = new MOS_COMMON(*(const MOS_COMMON*)common);
  assert(c);
  
  parselabel(cmd);
  parsenodes(cmd,NUMNODES);
  cmd.ctostr(c->modelname, LABELEN, TOKENTERM);
  c->model = NULL;
  if (cmd.is_pfloat()){				/* accept W/L notation for */
    c->wo = cmd.ctopf() * MICRON2METER;		/* dimensions (microns)    */
    if (cmd.match('/')){
      cmd.skip();
      c->lo = cmd.ctopf() * MICRON2METER;
    }
  }
  cmd.stuck();
  do{
    cmd.get("L",   &c->lo,    mPOSITIVE);
    cmd.get("W",   &c->wo,    mPOSITIVE);
    cmd.get("AD",  &c->ad_in, mPOSITIVE);
    cmd.get("AS",  &c->as_in, mPOSITIVE);
    cmd.get("PD",  &c->pd,    mPOSITIVE);
    cmd.get("PS",  &c->ps,    mPOSITIVE);
    cmd.get("NRD", &c->nrd,   mPOSITIVE);
    cmd.get("NRS", &c->nrs,   mPOSITIVE);
  }while (cmd.more() && !cmd.stuck());
  cmd.check(bWARNING);
  attach_common(c);
}
/*--------------------------------------------------------------------------*/
void DEV_MOS::print(int where, int)const
{
  const MOS_COMMON* c = (const MOS_COMMON*)common;
  assert(c);
  
  printlabel(where);
  printnodes(where,NUMNODES);
  mprintf(where, " %s ",    c->modelname);
  if (c->lo != NOT_INPUT)
    mprintf(where, " l=%s ",  ftos(c->lo, "", 7, 0));
  if (c->wo != NOT_INPUT)
    mprintf(where, " w=%s ",  ftos(c->wo,  "", 7, 0));
  if (c->ad_in != NOT_INPUT)
    mprintf(where, " ad=%s ", ftos(c->ad_in, "", 7, 0));
  if (c->as_in != NOT_INPUT)
    mprintf(where, " as=%s ", ftos(c->as_in, "", 7, 0));
  if (c->pd != 0.)
    mprintf(where, " pd=%s ", ftos(c->pd, "", 7, 0));
  if (c->ps != 0.)
    mprintf(where, " ps=%s ", ftos(c->ps, "", 7, 0));
  mprintf(where, " nrd=%s ",ftos(c->nrd,"", 7, 0));
  mprintf(where, " nrs=%s ",ftos(c->nrs,"", 7, 0));
  
  if (c->icset){
    mprintf(where, "  IC=");
    for (int i = 0;  i < mNUM_INIT_COND;  i++)
      mprintf(where, "%s ",ftos(c->ic[i],"", 7, 0));
  }
  mprintf(where, "\n");
}
/*--------------------------------------------------------------------------*/
double DEV_MOS::probe_tr_num(const char *what)const
{
  CS cmd(what);
  const MOS_COMMON* c = (const MOS_COMMON*)common;
  assert(c);
  const MODEL_MOS* m = (const MODEL_MOS*)c->model;
  assert(m);
  assert(subckt);

  if (cmd.pmatch("VDS")){
    return nDRAIN.v0() - nSOURCE.v0();
  }else if (cmd.pmatch("VGS")){
    return nGATE.v0() - nSOURCE.v0();
  }else if (cmd.pmatch("VBS")){
    return nBULK.v0() - nSOURCE.v0();
  }else if (cmd.pmatch("VGD")){
    return nGATE.v0() - nDRAIN.v0();
  }else if (cmd.pmatch("VBD")){
    return nBULK.v0() - nDRAIN.v0();
  }else if (cmd.pmatch("VSD")){
    return nSOURCE.v0() - nDRAIN.v0();
  }else if (cmd.pmatch("VDM")){
    return (nDRAIN.v0() - nSOURCE.v0()
	    + nDRAIN.v0() - nDRAIN.v0()) / 2.;
  }else if (cmd.pmatch("VGM")){
    return (nGATE.v0() - nSOURCE.v0()
	    + nGATE.v0() - nDRAIN.v0()) / 2.;
  }else if (cmd.pmatch("VBM")){
    return (nBULK.v0() - nSOURCE.v0()
	    + nBULK.v0() - nDRAIN.v0()) / 2.;
  }else if (cmd.pmatch("VSM")){
    return (nSOURCE.v0() - nSOURCE.v0()
	    + nSOURCE.v0() - nDRAIN.v0()) / 2.;
  }else if (cmd.pmatch("VDG")){
    return nDRAIN.v0() - nGATE.v0();
  }else if (cmd.pmatch("VBG")){
    return nBULK.v0() - nGATE.v0();
  }else if (cmd.pmatch("VSG")){
    return nSOURCE.v0() - nGATE.v0();
  }else if (cmd.pmatch("VDB")){
    return nDRAIN.v0() - nBULK.v0();
  }else if (cmd.pmatch("VGB")){
    return nGATE.v0() - nBULK.v0();
  }else if (cmd.pmatch("VSB")){
    return nSOURCE.v0() - nBULK.v0();
  }else if (cmd.pmatch("VD")){
    return nDRAIN.v0();
  }else if (cmd.pmatch("VG")){
    return nGATE.v0();
  }else if (cmd.pmatch("VB")){
    return nBULK.v0();
  }else if (cmd.pmatch("VS")){
    return nSOURCE.v0();
  }else if (cmd.pmatch("Id")){
    return (Rd)
      ?   CARD::probe(Rd,"I")
      :   CARD::probe(Ids,"I")
	+ CARD::probe(Gmf,"I")
	+ CARD::probe(Gmr,"I")
	+ CARD::probe(Yds,"I")
	+ CARD::probe(Gmbf,"I")
	+ CARD::probe(Gmbr,"I")
	- CARD::probe(Cgd,"I")
	+ CARD::probe(Ddb,"I") * m->polarity;
  }else if (cmd.pmatch("IS")){
    return (Rs)
      ?   CARD::probe(Rs,"I")
      : - CARD::probe(Ids,"I")
	- CARD::probe(Gmf,"I")
	- CARD::probe(Gmr,"I")
	- CARD::probe(Yds,"I")
	- CARD::probe(Gmbf,"I")
	- CARD::probe(Gmbr,"I")
	- CARD::probe(Cgs,"I")
	+ CARD::probe(Dsb,"I") * m->polarity;
  }else if (cmd.pmatch("IG")){
    return CARD::probe(Cgs,"I")
         + CARD::probe(Cgd,"I")
	 + CARD::probe(Cgb,"I");
  }else if (cmd.pmatch("IB")){
    return - CARD::probe(Ddb,"I") * m->polarity
	   - CARD::probe(Dsb,"I") * m->polarity
	   - CARD::probe(Cgb,"I");
  }else if (cmd.pmatch("CGSOvl")){
    return CARD::probe(Cgs,"NV");
  }else if (cmd.pmatch("CGDOvl")){
    return CARD::probe(Cgd,"NV");
  }else if (cmd.pmatch("CGBOvl")){
    return CARD::probe(Cgb,"NV");
  }else if (cmd.pmatch("CGST")){
    return CARD::probe(Cgs,"EV");
  }else if (cmd.pmatch("CGDT")){
    return CARD::probe(Cgd,"EV");
  }else if (cmd.pmatch("CGBT")){
    return CARD::probe(Cgb,"EV");
  }else if (cmd.pmatch("CGSm")){
    return CARD::probe(Cgs,"EV") - CARD::probe(Cgs,"NV");
  }else if (cmd.pmatch("CGDm")){
    return CARD::probe(Cgd,"EV") - CARD::probe(Cgd,"NV");
  }else if (cmd.pmatch("CGBm")){
    return CARD::probe(Cgb,"EV") - CARD::probe(Cgb,"NV");
  }else if (cmd.pmatch("CBD")){
    return CARD::probe(Ddb,"Cap");
  }else if (cmd.pmatch("CBS")){
    return CARD::probe(Dsb,"Cap");
  }else if (cmd.pmatch("CGATE")){
    return c->cgate;
  }else if (cmd.pmatch("GM")){
    return gm;
  }else if (cmd.pmatch("GDS")){
    return gds;
  }else if (cmd.pmatch("GMB")){
    return gmb;
  }else if (cmd.pmatch("VGST")){
    return vgst;
  }else if (cmd.pmatch("VON")){
    return von;
  }else if (cmd.pmatch("VDSAT")){
    return vdsat * m->polarity;
  }else if (cmd.pmatch("VTH")){
    return von * m->polarity;
  }else if (cmd.pmatch("VDSInt")){
    return vds;
  }else if (cmd.pmatch("VGSInt")){
    return vgs;
  }else if (cmd.pmatch("VBSInt")){
    return vbs;
  }else if (cmd.pmatch("IDS")){
    return ids;
  }else if (cmd.pmatch("IDSTray")){
    return - CARD::probe(Cgd,"I")
           + CARD::probe(Ddb,"I") * m->polarity;
  }else if (cmd.pmatch("IDERror")){
    return  CARD::probe(Gmf,"I")
	  + CARD::probe(Gmr,"I")
	  + CARD::probe(Yds,"I")
	  + CARD::probe(Gmbf,"I")
	  + CARD::probe(Gmbr,"I");
  }else if (cmd.pmatch("P")){
    CARD *pb, *stop;
    double power = 0.;
    stop = pb = subckt;
    do{
      power += CARD::probe(pb,"P");
    }while (pb=pb->next(), pb != stop);
    return power;
  }else if (cmd.pmatch("PD")){
    CARD *pb, *stop;
    double power = 0.;
    stop = pb = subckt;
    do{
      power += CARD::probe(pb,"PD");
    }while (pb=pb->next(), pb != stop);
    return power;
  }else if (cmd.pmatch("PS")){
    CARD *pb, *stop;
    double power = 0.;
    stop = pb = subckt;
    do{
      power += CARD::probe(pb,"PS");
    }while (pb=pb->next(), pb != stop);
    return power;
  }else if (cmd.pmatch("REgion")){
    return (double)(
	  (!cutoff)
	+ (!subthreshold * 2)
	+ (saturated * 4)
	+ (sbfwd * 10)
	+ (dbfwd * 20)
	+ (punchthru * 40)
    ) * ((reversed)? -1 : 1);
  }else if (cmd.pmatch("Status")){
    return (double)(bypass + converged * 2);
//  }else if (cmd.pmatch("DTNew")){
//    return timef - time0;
//  }else if (cmd.pmatch("DTOld")){
//    return time0 - time1;
//  }else if (cmd.pmatch("TIMEF")){
//    return timef;
//  }else if (cmd.pmatch("TIME")){
//    return time0;
//  }else if (cmd.pmatch("TIMEO")){
//    return time1;
  }else{ /* bad parameter */
    return NOT_VALID;
  }
  /*NOTREACHED*/
}
/*--------------------------------------------------------------------------*/
bool DEV_MOS::dotr()
{
  assert(!isbypassed());
  const MOS_COMMON* c = (const MOS_COMMON*)common;
  assert(c);
  const MODEL_MOS* m = (const MODEL_MOS*)c->model;
  assert(m);
  assert(subckt);

  double Vds, Vgs, Vbs;
  
  sourcenode.m = NODE::to_internal(sourcenode.t);
  drainnode.m  = NODE::to_internal(drainnode.t);

  bool was_cutoff = cutoff;
  bool was_subthreshold = subthreshold;
  bool was_saturated = saturated;
  bool was_reversed = reversed;
  bool was_dbfwd = dbfwd;
  bool was_sbfwd = sbfwd;

  if (STATUS::iter[SIM::mode] <= 1){
    reversed = false;
    Vds = Vgs = Vbs = 0.;
    if (OPT::mosflags & 0100){
      Vds = m->vto;
    }
    if (OPT::mosflags & 0200){
      Vgs = m->vto;
    }
    if (OPT::mosflags & 0400  &&  nSOURCE.t != nBULK.t){
      Vbs = -1;
    }
    if (nDRAIN.t == nGATE.t){
      Vds = Vgs;
    }
  }else if (reversed){
    Vds = m->polarity * volts_limited(sourcenode,drainnode);
    Vgs = m->polarity * volts_limited(nGATE,drainnode);
    Vbs = m->polarity * volts_limited(nBULK,drainnode);
  }else{
    Vds = m->polarity * volts_limited(drainnode,sourcenode);
    Vgs = m->polarity * volts_limited(nGATE,sourcenode);
    Vbs = m->polarity * volts_limited(nBULK,sourcenode);
  }
  
  if (SIM::bypass_ok  &&  (converged  ||  OPT::bypass == bVOLT)
      && conchk(vds, Vds, OPT::vntol)
      && conchk(vgs, Vgs, OPT::vntol)
      && conchk(vbs, Vbs, OPT::vntol)){
    assert(converged);
    bypass = true;
  }else{
    bypass = false;
    limit_mos(Vds, Vgs, Vbs);
    if (vds < 0){
      if (nSOURCE.t == nBULK.t  ||  nDRAIN.t == nGATE.t){
        error(bTRACE,"%s: cannot reverse\n", printlabel());
	error(bTRACE,"vds=%g vgs=%g vbs=%g\n", vds, vgs, vbs);
      }else if (!(OPT::mosflags & 0001)){
	error(bTRACE,"%s: reversing\n", printlabel());
	error(bTRACE,"before: vds=%g vgs=%g vbs=%g\n", vds, vgs, vbs);
	reversed = !reversed;
	vgs -= vds;
	vbs -= vds;
	vds = -vds;
	error(bTRACE,"after: vds=%g vgs=%g vbs=%g\n", vds, vgs, vbs);
	if (OPT::dampstrategy & dsREVERSE){
	    SIM::fulldamp = true;
	    error(bTRACE,"%s:reverse damp\n", printlabel());
	}
	if (!(OPT::mosflags & 0040)){
	  vbs = min(vbs,0.);
	}
      }
    }
    m->tr_eval(this);
    converged = subckt->dotr_group();
  }

  if (vds > 100){
    error(bDEBUG,"%s:%d: ", printlabel(), evaliter);
    error(bDEBUG,"vds=%e vgs=%e vbs=%e\n",
		  vds,   vgs,   vbs);
    error(bDEBUG,"+      ids=%e gm=%e gds=%e gmb=%e\n",
    			 ids,   gm,   gds,   gmb);
  }
  trace3(printlabel(), vds, vgs, vbs);
  trace4("", ids, gm, gds, gmb);
  if (was_cutoff != cutoff  ||  was_subthreshold != subthreshold  
  	||  was_saturated != saturated  ||  was_reversed != reversed  
	||  was_dbfwd != dbfwd  ||  was_sbfwd != sbfwd){
    if (OPT::dampstrategy & dsDEVREGION){
      SIM::fulldamp = true;
    }
    #if defined(DO_TRACE)
      error(bTRACE,"%s:%d: region change\n", printlabel(), evaliter);
    #endif
  }
  return converged;
}
/*--------------------------------------------------------------------------*/
/* limit_mos: do Spice style voltage limiting
 */
void DEV_MOS::limit_mos(double Vds, double Vgs, double Vbs)
{
					/* Spice style vgs limiting */
  if (!(OPT::mosflags & 0010) && STATUS::iter[SIM::mode] > 1){
    //assert(vgst == vgs - von);
    if (Vgs > vgs){			/* increasing */
      if (vgst < 0){			/* was off */
	vgs = min(Vgs, von + .5);
      }else if (vgst < 3.5){		/* ??? */
	vgs = min(Vgs, von + 4.);
      }else{
	vgs = min(Vgs, 3.*vgs - 2.*von + 2.);
      }
    }else{				/* decreasing */
      if (vgst < 0){			/* off */
	vgs = max(Vgs, 3. * vgs - 2. * von - 2.);
      }else if (vgst < 3.5){
	vgs = max(Vgs, von - .5);
      }else{
	vgs = max(Vgs, von + 2.);
      }
    }
    //Vds += vgs - Vgs;			/* vds patch (per Spice) not done */
  }else{				/* because it usually makes it worse */
    vgs = Vgs;
  }
  if (nDRAIN.t == nGATE.t){		/* gd tied, limiting done */
    vds = Vds + (vgs - Vgs);		/* it seems that vds = vgs should */
    					/* work, but it will be a little off */
					/* if there is resistance */

					/* Spice style vds limiting */
  }else if (!(OPT::mosflags & 0020) && STATUS::iter[SIM::mode] > 1){
    if (Vds <= vds){			/* decreasing */
      if (vds < 3.5){
	vds = max(Vds,-.5);
      }else{
	vds = max(Vds,2.);
      }
    }else{				/* increasing */
      if (vds < 3.5){
	vds = min(Vds,4.);
      }else{
	vds = min(Vds, 3.*vds + 2.);
      }
    }
    //vgs += vds - Vds;
  }else{
    vds = Vds;
  }

  if (!(OPT::mosflags & 0040) && STATUS::iter[SIM::mode] > 1){
//    if (Vbs > 0.){
//      if (vbs >= 0.){
//        vbs = min(Vbs,vbs+.1);
//      }else{
//        vbs = 0.;
//      }
//    }else{
//      vbs = Vbs;
//    }
    vbs = min(Vbs,0.);
  }else{
    vbs = Vbs;
  }
  if (OPT::dampstrategy & dsDEVLIMIT
      && (vgs != Vgs || vds != Vds || vbs != Vbs)){
    SIM::fulldamp = true;
    error(bTRACE,"%s:device limit damp\n", printlabel());
  }
  #if defined(DO_TRACE)
    if ((vgs != Vgs || vds != Vds || vbs != Vbs)){
      trace1(printlabel(), evaliter);
      trace3("prop", Vds, Vgs, Vbs);
      trace3("using", vds, vgs, vbs);
    }
  #endif
}
/*--------------------------------------------------------------------------*/
void DEV_MOS::expand()
{
  MOS_COMMON* c = (MOS_COMMON*)common;
  assert(c);
  const MODEL_MOS* m = (const MODEL_MOS*)attach_model();
  assert(m);
 
  if (c->lo != NOT_INPUT){
    c->le = c->lo - 2. * m->ld;
  }else{
    c->le = OPT::defl - 2. * m->ld;
  }
  c->we = (c->wo != NOT_INPUT) ? c->wo : OPT::defw;
  
  c->cgate = m->cox * c->we * c->le;
  if (c->cgate <= 0.)
    error(bERROR, "%s: cgate is negative?\n", printlabel());
  c->beta = m->kp * c->we / c->le;
  c->relxj = (m->xj != NOT_INPUT  &&  m->xj > 0.)
    ? .5 * m->xj / c->le
    : NOT_INPUT;
  c->eta_1 = (kPI/4.) * E_SI * m->delta / c->cgate * c->le;
  c->eta = c->eta_1 + 1.;
  c->eta_2 = c->eta / 2.;
  c->sigma = m->eta * 8.15e-22 / (m->cox * c->le * c->le * c->le);

  if (m->rsh != NOT_INPUT  &&  m->rd <= 0.  &&  m->rs <= 0.){
    c->rd = m->rsh * c->nrd;
    c->rs = m->rsh * c->nrs;
  }else{
    c->rd = (m->rd != NOT_INPUT) ? m->rd : 0.;
    c->rs = (m->rs != NOT_INPUT) ? m->rs : 0.;
  }

  c->ad = (c->ad_in != NOT_INPUT) ? c->ad_in : OPT::defad;
  c->as = (c->as_in != NOT_INPUT) ? c->as_in : OPT::defas;
  if (m->js == NOT_INPUT  ||  c->ad == 0.  ||  c->as == 0.){
    if (m->is == NOT_INPUT){
      c->idsat = c->issat = mDEFAULT_is;
      error(bWARNING,"%s: ignoring js, using default is\n",printlabel());
    }else{
      c->idsat = c->issat = m->is;	/* this convoluted logic */
    }					/* is for Spice compatibility */
  }else{
    c->idsat = m->js * c->ad;
    c->issat = m->js * c->as;
  }

  strcpy(c->db.modelname, c->modelname);
  c->db.area = c->ad;
  c->db.perim = c->pd;
  c->db.is = c->idsat;
  c->db.cj = m->cbd;	/* if NOT_INPUT diode will calculate it */
  c->db.cjsw = NOT_INPUT;
  c->db.model = c->model;
  c->db.off = true;

  strcpy(c->sb.modelname, c->modelname);
  c->sb.area = c->as;
  c->sb.perim = c->ps;
  c->sb.is = c->issat;
  c->sb.cj = m->cbs;	/* if NOT_INPUT diode will calculate it */
  c->sb.cjsw = NOT_INPUT;
  c->sb.model = c->model;
  c->sb.off = true;

  subckt = Ids;		/* subckt points at any element in the subckt */
 			/* this forces it to be one that will not be deleted */
			/* otherwise it could result in a dangling pointer */
			
							    /* build subckt */
							       /* resistors */
  if (OPT::rstray && c->rs != 0.){
    sourcenode.t = newnode_model();
    if (!Rs){
      Rs = new DEV_RESISTANCE;
      assert(Rs);
      subckt = Rs->insertbefore(subckt);
      assert(subckt == Rs);
    }
    Rs->set("Rs", this, NULL, c->rs, nSOURCE, sourcenode);
  }else{
    sourcenode = nSOURCE;
    delete Rs;
    Rs = NULL;
  }
  if (OPT::rstray && c->rd != 0.){
    drainnode.t = newnode_model();
    if (!Rd){
      Rd = new DEV_RESISTANCE;
      assert(Rd);
      subckt = Rd->insertbefore(subckt);
      assert(subckt == Rd);
    }
    Rd->set("Rd", this, NULL, c->rd, nDRAIN, drainnode);
  }else{
    drainnode = nDRAIN;
    delete Rd;
    Rd = NULL;
  }
								/* diodes */
  if (nBULK.t != drainnode.t   &&   c->idsat != 0.){
    if (!Ddb){
      Ddb = new DEV_DIODE;
      assert(Ddb);
      subckt = Ddb->insertbefore(subckt);
      assert(subckt == Ddb);
    }
    switch (m->polarity){
    case MODEL_MOS::pN:
      Ddb->set("Ddb", this, &(c->db), 0., nBULK, drainnode);
      break;
    case MODEL_MOS::pP:
      Ddb->set("Ddb", this, &(c->db), 0., drainnode, nBULK);
      break;
    }
  }else{
    delete Ddb;
    Ddb = NULL;
  }
  
  if (nBULK.t != sourcenode.t   &&   c->issat != 0.){
    if (!Dsb){
      Dsb = new DEV_DIODE;
      assert(Dsb);
      subckt = Dsb->insertbefore(subckt);
      assert(subckt == Dsb);
    }
    switch (m->polarity){
    case MODEL_MOS::pN:
      Dsb->set("Dsb", this, &(c->sb), 0., nBULK, sourcenode);
      break;
    case MODEL_MOS::pP:
      Dsb->set("Dsb", this, &(c->sb), 0., sourcenode, nBULK);
      break;
    }
  }else{
    delete Dsb;
    Dsb = NULL;
  }
							    /* capacitors */
  if (OPT::cstray  &&  nGATE.t != sourcenode.t){
    if (!Cgs){
      Cgs = new DEV_CAPACITANCE;
      assert(Cgs);
      subckt = Cgs->insertbefore(subckt);
      assert(subckt == Cgs);
    }
    Cgs->set("Cgs", this, &Eval_Cgs, (m->cgso*c->we), nGATE, sourcenode);
  }else{
    delete Cgs;
    Cgs = NULL;
  }
  
  if (OPT::cstray  &&  nGATE.t != drainnode.t){
    if (!Cgd){
      Cgd = new DEV_CAPACITANCE;
      assert(Cgd);
      subckt = Cgd->insertbefore(subckt);
      assert(subckt == Cgd);
    }
    Cgd->set("Cgd", this, &Eval_Cgd, (m->cgdo*c->we), nGATE, drainnode);
  }else{
    delete Cgd;
    Cgd = NULL;
  }
  
  if (OPT::cstray  &&  nBULK.t != nGATE.t){
    if (!Cgb){
      Cgb = new DEV_CAPACITANCE;
      assert(Cgb);
      subckt = Cgb->insertbefore(subckt);
      assert(subckt == Cgb);
    }
    Cgb->set("Cgb", this, &Eval_Cgb, (m->cgbo*c->le), nGATE, nBULK);
  }else{
    delete Cgb;
    Cgb = NULL;
  }
							   /* model sources */
  if (nBULK.t != sourcenode.t){
    if (!Gmbf){
      Gmbf = new DEV_VCCS;
      assert(Gmbf);
      subckt = Gmbf->insertbefore(subckt);
      assert(subckt == Gmbf);
    }
    Gmbf->set("Gmbf",this,&Eval_Gmbf,0.,drainnode,sourcenode,nBULK,sourcenode);
  }else{
    delete Gmbf;
    Gmbf = NULL;
  }

  if (nBULK.t != drainnode.t){
    if (!Gmbr){
      Gmbr = new DEV_VCCS;
      assert(Gmbr);
      subckt = Gmbr->insertbefore(subckt);
      assert(subckt == Gmbr);
    }
    Gmbr->set("Gmbr",this,&Eval_Gmbr,0.,sourcenode,drainnode,nBULK,drainnode);
  }else{
    delete Gmbr;
    Gmbr = NULL;
  }

  if (!Yds){
    Yds = new DEV_ADMITTANCE;
    assert(Yds);
    subckt = Yds->insertbefore(subckt);
    assert(subckt == Yds);
  }
  Yds->set("Yds", this, &Eval_Gds, 0., drainnode, sourcenode);

  if (nGATE.t != sourcenode.t){
    if (!Gmf){
      Gmf = new DEV_VCCS;
      assert(Gmf);
      subckt = Gmf->insertbefore(subckt);
      assert(subckt == Gmf);
    }
    Gmf->set("Gmf",this,&Eval_Gmf,0.,drainnode,sourcenode,nGATE,sourcenode);
  }else{
    delete Gmf;
    Gmf = NULL;
  }

  if (nGATE.t != drainnode.t){
    if (!Gmr){
      Gmr = new DEV_VCCS;
      assert(Gmr);
      subckt = Gmr->insertbefore(subckt);
      assert(subckt == Gmr);
    }
    Gmr->set("Gmr",this,&Eval_Gmr,0.,sourcenode,drainnode,nGATE,drainnode);
  }else{
    delete Gmr;
    Gmr = NULL;
  }

  if (!Ids){
    Ids = new DEV_CS;
    assert(Ids);
    subckt = Ids->insertbefore(subckt);
    assert(subckt == Ids);
  }
  Ids->set("Ids", this, &Eval_Ids, 0., drainnode, sourcenode);
  assert(subckt);
  subckt->expand_group();

  assert(!constant); /* because it is nonlinear */
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
