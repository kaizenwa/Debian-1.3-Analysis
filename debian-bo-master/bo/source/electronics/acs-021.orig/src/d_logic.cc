/*$Id: d_logic.cc,v 11.39 96/03/30 15:56:35 al Exp $ -*- C++ -*-
 * logic model and device.
 * netlist syntax:
 * device:  mxxxx vdd out in1 in2 ... family gatetype
 * model:   .model mname LOGIC <args>
 */
#include "ap.h"
#include "error.h"
#include "u_opt.h"
#include "io.h"
#include "l_lib.h"
#include "u_status.h"
#include "d_logic.h"
#include "s__.h"
#include "declare.h"	/*  new_event ... */
/*--------------------------------------------------------------------------*/
//		MODEL_LOGIC::MODEL_LOGIC(const char*);
//	void	MODEL_LOGIC::parse(CS& cmd);
// 	void	MODEL_LOGIC::print(int,int)const;

//		DEV_LOGIC::DEV_LOGIC();
//		DEV_LOGIC::DEV_LOGIC(CONST DEV_LOGIC& p);
//	void	DEV_LOGIC::parse(CS& cmd);
// 	void	DEV_LOGIC::print(int,int)const;
//	void	DEV_LOGIC::expand();
//	double	DEV_LOGIC::probe_tr_num(const char *what)const;
//	double	DEV_LOGIC::probe_ac_num(const char *what)const;
// 	bool	DEV_LOGIC::dotr();
// 	void	DEV_LOGIC::trload();
// 	void	DEV_LOGIC::trunload();
// 	void	DEV_LOGIC::doac();
	NODE* 	tologic(const MODEL_LOGIC*,const node_t*);
	double	toanalog(const MODEL_LOGIC*,const node_t*);
//	double	DEV_LOGIC::tr_review();
/*--------------------------------------------------------------------------*/
extern NODE* nstat;
static const char *(type_name[]) = {"error", "and", "nand", "or", "nor", "xor",
    "inv", ""};
static struct logic defalt = {NULL, sizeof(struct logic),
    NULL, "-error-", /*more*/};
int DEV_LOGIC::Count = 0;
/*--------------------------------------------------------------------------*/
MODEL_LOGIC::MODEL_LOGIC(const char *name):MODEL_CARD(name)
{
  delay = 1e-9;
  vmax  = 5.;
  vmin  = 0.;
  range = vmax - vmin;
  rise  = delay / 2;
  fall  = delay / 2;
  rs    = 100.;
  rw    = 1e9;
  th1   = .75;
  th0   = .25;
  mr    = 5.;
  mf    = 5.;
  over  = .1;
}
/*--------------------------------------------------------------------------*/
void MODEL_LOGIC::parse(CS& cmd)
{
  cmd.skiparg();		/* skip known ".model" */
  cmd.ctostr(label, LABELEN, TOKENTERM);
  cmd.skiparg();		/* skip known "logic" */
  cmd.skiplparen();
  cmd.stuck();
  do{
    cmd.get("DElay",	&delay,	mPOSITIVE);
    cmd.get("RIse",	&rise,	mPOSITIVE);
    cmd.get("FAll",	&fall,	mPOSITIVE);
    cmd.get("RS",	&rs,	mPOSITIVE);
    cmd.get("RW",	&rw,	mPOSITIVE);
    cmd.get("THH",	&th1);
    cmd.get("THL",	&th0);
    cmd.get("MR",	&mr,	mPOSITIVE);
    cmd.get("MF",	&mf,	mPOSITIVE);
    cmd.get("OVer",	&over,	mPOSITIVE);
    cmd.get("VMAx",	&vmax);
    cmd.get("VMIn",	&vmin);
  }while (cmd.more() && !cmd.stuck());
  cmd.skiprparen();
  cmd.check(bWARNING);
  range = vmax - vmin;
}
/*--------------------------------------------------------------------------*/
void MODEL_LOGIC::print(int where, int)const
{
  mprintf(where, ".model  %s logic (", label); 
  mprintf(where, " delay=%s ",ftos(delay,"", 7, 0));
  mprintf(where, " rise=%s ", ftos(rise, "", 7, 0));
  mprintf(where, " fall=%s ", ftos(fall, "", 7, 0));
  mprintf(where, " rs=%s ",   ftos(rs,   "", 7, 0));
  mprintf(where, " rw=%s ",   ftos(rw,   "", 7, 0));
  mprintf(where, " thh=%s ",  ftos(th1,  "", 7, 0));
  mprintf(where, " thl=%s ",  ftos(th0,  "", 7, 0));
  mprintf(where, " mr=%s ",   ftos(mf,   "", 7, 0));
  mprintf(where, " mf=%s ",   ftos(mr,   "", 7, 0));
  mprintf(where, " over=%s ", ftos(over, "", 7, 0));
  mprintf(where, " vmax=%s ", ftos(vmax, "", 7, 0));
  mprintf(where, " vmin=%s ", ftos(vmin, "", 7, 0));
  mprintf(where, ")\n");
}
/*--------------------------------------------------------------------------*/
DEV_LOGIC::DEV_LOGIC()
{
  struct logic *xx;
  x = create_extra_stuff((generic_t*)&defalt);
  xx = (struct logic*)x;
  n = xx->n;
  devclass = SUBCKT;
  ++Count;
}
/*--------------------------------------------------------------------------*/
DEV_LOGIC::DEV_LOGIC(CONST DEV_LOGIC& p):ELEMENT(p)
{
  struct logic *xx;
  xx = (struct logic*)x;
  n = xx->n;
  ++Count;
}
/*--------------------------------------------------------------------------*/
void DEV_LOGIC::parse(CS& cmd)
{
  struct logic *xx;
  xx = (struct logic*)x;

  parselabel(cmd);
  xx->incount = parsenodes(cmd, PORTSPERGATE);
  xx->incount -= 2;
  cmd.ctostr(xx->modelname, LABELEN, TOKENTERM);
  cmd.stuck();
  /**/ ::set(cmd, "AND",  &xx->type, lAND)
    || ::set(cmd, "NAND", &xx->type, lNAND)
    || ::set(cmd, "OR",   &xx->type, lOR)
    || ::set(cmd, "NOR",  &xx->type, lNOR)
    || ::set(cmd, "XOR",  &xx->type, lXOR)
    || ::set(cmd, "INV",  &xx->type, lINV);
  if (cmd.stuck()){
    cmd.check(bWARNING);
  }
}
/*--------------------------------------------------------------------------*/
void DEV_LOGIC::print(int where, int)const
{
  struct logic *xx;
  xx = (struct logic*)x;

  printlabel(where);
  printnodes(where, PORTSPERGATE);
  mprintf(where, " %s %s\n", xx->modelname, type_name[xx->type]); 
}
/*--------------------------------------------------------------------------*/
void DEV_LOGIC::expand()
{
  struct logic* c = (struct logic*)x;
  assert(c);
  c->model = MODEL_CARD::rootmodel->find_model(c->modelname);
  assert(c->model);
  
  char cktname[BUFLEN];
  sprintf(cktname, "%s%s%u", c->modelname, type_name[c->type], c->incount);
  expandsubckt(this,cktname);
  if (!subckt){
    error(bDANGER, "%s: no model, forcing digital\n", printlabel());
  }else{
    subckt->expand_group();
  }
  assert(!constant); /* is a BUG */
}
/*--------------------------------------------------------------------------*/
double DEV_LOGIC::probe_tr_num(const char *what)const
{
  CS cmd(what);
  if (cmd.pmatch("V")){
    printf("DEV_LOGIC::probe_tr_num");
    return n[OUT2].v0();
  }else { /* bad parameter */
    return NOT_VALID;
  }
}
/*--------------------------------------------------------------------------*/
double DEV_LOGIC::probe_ac_num(const char *)const
{
  return NOT_VALID;
}
/*--------------------------------------------------------------------------*/
bool DEV_LOGIC::dotr()
{
  assert(!isbypassed());
  struct logic* xx = (struct logic*)x;
  assert(xx);
  const MODEL_LOGIC* m = (const MODEL_LOGIC*)xx->model;
  assert(m);
  NODE* nn = &(nstat[n[1].m]);
  
  if (subckt  &&  ((gatemode == mANALOG)
		   ||  (OPT::mode == mMIXED && SIM::phase == pINIT_DC)
		   ||  (OPT::mode == mANALOG))){
    gatemode = nn->nodemode = mANALOG;
    return converged = subckt->dotr_group();
  }
  
/* if it gets here, either OPT::mode == mMIXED  &&  quality == qGOOD
 *			or OPT::mode == mDIGITAL
 */
  
  bypass = true;
  gatemode = nn->nodemode = mDIGITAL;
  
  if (SIM::phase == pINIT_DC){
    tr_review();
    nn->finaltime = 0.;
  }
  
  if (SIM::time0 >= nn->finaltime){	/* in transition, time to propagate */
    bypass = false;
    nn->lv0 = nn->lv1;
    nn->ls0 = nn->ls1;
    nn->quality = qGOOD;
    nn->diter = STATUS::iter[iTOTAL];
    nn->finaltime = BIGBIG;
    nn->lastchange = SIM::time0;
  }
  
  if (nn->finaltime != BIGBIG  ||  !bypass  ||  !SIM::inc_mode){
    m0.x = 0.;
    y0.x = 0.;
    y0.f1 = -toanalog(m,&(n[OUT2]));
    y0.f0 = 0.;
    m0.f1 = 1./m->rs;
    m0.c0 = y0.f1 / -m->rs;
  }
  store_values();
  SIM::loadq << this;		/* BUG:: always load logic??????? */
  return converged = true;
}
/*--------------------------------------------------------------------------*/
void DEV_LOGIC::trload()
{
  NODE* nn = &(nstat[n[1].m]);
  if (gatemode == mANALOG){
    subckt->trload_group();
  }else if (nn->finaltime != BIGBIG  ||  !bypass  ||  !SIM::inc_mode){
    node_t vdd;				/* BUG: this is a mess. fix it. */
    node_t ground;			/* it is a fudge to make	*/
    vdd = n[OUT1];			/* the logic device load like a */
    ground.e = ground.m = ground.t = 0;	/* fixed source.		*/
    n[OUT1] = ground;
    trload_passive();
    n[OUT1] = vdd;
  } /* else logic mode, latent */
}
/*--------------------------------------------------------------------------*/
void DEV_LOGIC::trunload()
{
  if (subckt)
    subckt->trunload_group();
  trunload_passive();
}
/*--------------------------------------------------------------------------*/
void DEV_LOGIC::doac()
{
  error(bWARNING, "%s: no logic in AC analysis\n", printlabel());
}
/*--------------------------------------------------------------------------*/
NODE* tologic(const MODEL_LOGIC *m, const node_t *node)
{
  NODE* n;
  
  if (node->m == INVALIDNODE){
    node_t ground;
    ground.e = ground.m = ground.t = 0;
    error(bDANGER, "%u:(%u,%u,%u):internal error: invalid node\n",
	  STATUS::iter[iTOTAL], node->e, node->t, node->m);
    return &(nstat[ground.m]);
  }
  
  n = &(nstat[node->m]);
  if (!m){
    m = n->family;
  }else if (!n->family){
    n->family = m;
  }
  
  if (n->nodemode == mDIGITAL){
    ;/* nothing */
  }else{ /* n->nodemode == mANALOG */
    if (n->diter < n->aiter){
      double sv = n->v0() / m->range;	/* new scaled voltage */
      bool oldstate = n->lv0;		/* save to see if change */

      n->dt = SIM::time0 - n->lastchange;
      
      if (sv >= m->th1){
	n->lv0 = n->lv1 = true;
      }else if (sv <= m->th0){
	n->lv0 = n->lv1 = false;
      }else{					/* transition region */
	double oldsv = n->vt1() / m->range;	/* old scaled voltage */
	double diff  = sv - oldsv;
	
	if (diff > OPT::abstol){			/* rising */
	  if (n->lv1  &&  (n->lv0  ||  diff < n->dt/(m->mr * m->rise))){
	    n->quality = qBAD;		/* inflection or too slow */
	    n->failuremode = "slow rise";
	  }
	  n->lv1 = true;
	}else if (diff < -OPT::abstol){			/* falling */
	  if (!(n->lv1)  &&  (!(n->lv0) || -diff < n->dt/(m->mf * m->fall))){
	    n->quality = qBAD;		/* inflection or too slow */
	    n->failuremode = "slow fall";
	  }
	  n->lv1 = false;
	}else{
	  error(bDANGER, "inflection???\n");
	  n->quality = qBAD;		/* inflection or too slow */
	  n->failuremode = "inflection";
	  /* n->lv1 unchanged */
	}
      }
      if (sv > 1.+m->over  ||  sv < -m->over){	/* out of range */
	n->quality = qBAD;
	n->failuremode = "out of range";
      }
      if (n->quality != qGOOD  &&  n->lv0 != oldstate){
	++n->quality;
      }
      n->family = m;
      n->diter = STATUS::iter[iTOTAL];
      n->lastchange = SIM::time0;
    }
  }
  if (m != n->family){
    n->quality = qBAD;
    n->failuremode = "family mismatch";
  } 
  return n;
}
/*--------------------------------------------------------------------------*/
double toanalog(const MODEL_LOGIC *m, const node_t *node)
{
  NODE* n;

  n = &(nstat[node->m]);
  if (!m){
    m = n->family;
  }else if (!n->family){
    n->family = m;
  }
  
  if (n->lv0 == n->lv1){
    return (n->lv0) ? m->vmax : m->vmin;
  }else if (SIM::time0 <= (n->finaltime-((n->lv0)?(m->fall):(m->rise)))){
    return (n->lv0) ? m->vmax : m->vmin;
  }else if (SIM::time0 >= n->finaltime){    
    return (n->lv1) ? m->vmax : m->vmin;
  }else{
    double start,end,interp;
    start = (n->lv0) ? m->vmax : m->vmin;
    end   = (n->lv1) ? m->vmax : m->vmin;
    interp = (n->finaltime-SIM::time0) / ((n->lv0)?(m->fall):(m->rise));
    return end - (end - start) * interp;
  }
}
/*--------------------------------------------------------------------------*/
double DEV_LOGIC::tr_review()
{
  struct logic* xx = (struct logic*)x;
  assert(xx);
  const MODEL_LOGIC* m = (const MODEL_LOGIC*)xx->model;
  assert(m);
  
  NODE* ns[PORTSPERGATE];
  int connections;		/* review */
  int lastchangenode = 0;	/* review */
  int lastchangeiter = 0;	/* review, once */
  int quality = qGOOD;		/* both */
  const char* failuremode = "default";	/* review & print */
  
  
  for (connections = 1;
       connections < PORTSPERGATE  &&  this->n[connections].e != INVALIDNODE;
       connections++){
    ns[connections] = tologic(m,&(this->n[connections]));
    if (quality > ns[connections]->quality){
      quality = ns[connections]->quality;
      failuremode = ns[connections]->failuremode;
    }
    if (ns[connections]->diter >= lastchangeiter){
      lastchangeiter = ns[connections]->diter;
      lastchangenode = connections;
    }
  }
  
  /* connections == number of connections, now const */
  /* if lastchangenode == 1, no new changes, bypass */
  
  if (subckt  &&  
      ((OPT::mode == mANALOG)  ||  (OPT::mode == mMIXED && quality != qGOOD))){
    if (gatemode == mDIGITAL){
      error(bTRACE, "%s:%u:%g switch to analog (review), %s\n",
	    printlabel(), STATUS::iter[iTOTAL], SIM::time0, failuremode);
      trunload_passive();
    }
    gatemode = nstat[this->n[1].m].nodemode = mANALOG;
    return /*timef =*/ subckt->tr_review_group();
  }
  
  /* if it gets here, either OPT::mode == mMIXED  &&  quality == qGOOD
   *			or OPT::mode == mDIGITAL
   */
  
  if (gatemode == mANALOG){
    error(bTRACE, "%s:%u:%g switch to digital (review)\n",
	  printlabel(), STATUS::iter[iTOTAL], SIM::time0);
    subckt->trunload_group();
  }
  gatemode = ns[1]->nodemode = mDIGITAL;
  bypass = true;
  
  if (!SIM::bypass_ok || lastchangenode!=1 || SIM::phase==pINIT_DC){
    NODE out;
    int ii;
    out = *ns[1];
    bypass = false;
    switch (xx->type){
    case lAND:
      out.lv1 = true;
      for (ii = 2;  ii < connections;  ii++)
	out.lv1 &= ns[ii]->lv0;
      break;
    case lNAND:
      out.lv1 = true;
      for (ii = 2;  ii < connections;  ii++)
	out.lv1 &= ns[ii]->lv0;
      out.lv1 = !out.lv1;
      break;
    case lOR:
      out.lv1 = false;
      for (ii = 2;  ii < connections;  ii++)
	out.lv1 |= ns[ii]->lv0;
      break;
    case lNOR:
      out.lv1 = false;
      for (ii = 2;  ii < connections;  ii++)
	out.lv1 |= ns[ii]->lv0;
      out.lv1 = !out.lv1;
      break;
    case lXOR:
      out.lv1 = ns[2]->lv0 ^ ns[3]->lv0;
      break;
    case lINV:
      out.lv1 = !ns[2]->lv0;
      break;
    default:
      error(bWARNING,"%s: %s, bad logic type\n",printlabel(),xx->type);
      break;
    }
    if (out.lv1 != ns[1]->lv1){
      if (out.finaltime != BIGBIG){ 			/* in transition */
	out.quality = qBAD;
	out.failuremode = "race";
      }
      out.diter = STATUS::iter[iTOTAL];
      out.finaltime = SIM::time0 + m->delay;
      error(bTRACE, "%s:%u:%g new event\n",
	    printlabel(), STATUS::iter[iTOTAL], SIM::time0);
      new_event(out.finaltime);
      out.lastchange = SIM::time0;
      *ns[1] = out;
      if (lastchangenode == 1){
	error(bDANGER, "%s:%u:%g non-event state change\n",
	      printlabel(), STATUS::iter[iTOTAL], SIM::time0);
      }
    }else if (lastchangenode != 1){
      error(bTRACE,"%s:%u:%g null transition\n",
	    printlabel(), STATUS::iter[iTOTAL], SIM::time0);
    }else{
      error(bTRACE,"%s:%u:%g null evaluation\n",
	    printlabel(), STATUS::iter[iTOTAL], SIM::time0);
    }
  }
  return /*timef =*/ BIGBIG;
}
/* ***BUG*** review and advance need to be separated.
 * If a step is rejected by review, the logic values could become corrupt.
 */
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
