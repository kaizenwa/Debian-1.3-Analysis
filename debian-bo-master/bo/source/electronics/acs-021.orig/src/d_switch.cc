/*$Id: d_switch.cc,v 11.37 96/03/24 10:08:59 al Exp $ -*- C++ -*-
 * voltage (and current) controlled switch.
 * netlist syntax:
 * device:  Sxxxx n+ n- vc+ vc- mname <on>|<off> <model-card-args>
 * model:   .model mname SW <args>
 * current controlled switch
 * device:  Wxxxx n+ n- controlelement mname <on>|<off> <model-card-args>
 * model:   .model mname CSW <args>
 */
#include "ap.h"
#include "d_switch.h"
#include "error.h"
#include "io.h"
#include "u_status.h"
#include "s__.h"
#include "l_compar.h"
#include "l_lib.h"
/*--------------------------------------------------------------------------*/
//		MODEL_SWITCH::MODEL_SWITCH(const char*);
//	void	MODEL_SWITCH::parse(CS&);
//	void	MODEL_SWITCH::print(int,int)const;
//		SWITCH_BASE::SWITCH_BASE();
//		SWITCH_BASE::SWITCH_BASE(CONST SWITCH_BASE& p);
//	void	SWITCH_BASE::parse_sb(CS&,int);
//	void	SWITCH_BASE::print(int,int)const;
//	void	SWITCH_BASE::expand_sb();
//	bool	SWITCH_BASE::dotr();
//	void	SWITCH_BASE::doac();
//	void	DEV_CSWITCH::expand();
/*--------------------------------------------------------------------------*/
static SWITCH_COMMON Default_SWITCH;
/*--------------------------------------------------------------------------*/
MODEL_SWITCH::MODEL_SWITCH(const char *name):MODEL_CARD(name)
{
  vt = 0.;
  vh = 0.;
  ron = 1.;
  roff = 1e12;
  type = VOLTAGE;
}
/*--------------------------------------------------------------------------*/
void MODEL_SWITCH::parse(CS& cmd)
{
  cmd.skiparg();		// skip known ".model"
  cmd.ctostr(label, LABELEN, TOKENTERM);
  cmd.stuck();
  /**/ set(cmd, "SW",  &type, VOLTAGE)
    || set(cmd, "CSW", &type, CURRENT);
  if (cmd.stuck()){
    cmd.check(bWARNING);
  }
  cmd.skiplparen();
  cmd.stuck();
  do{
    cmd.get("VT",   &vt);
    cmd.get("VH",   &vh,  mPOSITIVE);
    cmd.get("IT",   &vt);
    cmd.get("IH",   &vh,  mPOSITIVE);
    cmd.get("RON",  &ron);
    cmd.get("ROFF", &roff);
  }while (cmd.more() && !cmd.stuck());
  cmd.skiprparen();
  cmd.check(bWARNING);
}
/*--------------------------------------------------------------------------*/
void MODEL_SWITCH::print(int where, int)const
{
  switch (type){
  case VOLTAGE:
    mprintf(where, ".model  %s  sw  (", label);
    mprintf(where, " vt=%s ",  ftos(vt,  "", 7, 0));
    mprintf(where, " vh=%s ",  ftos(vh,  "", 7, 0));
    break;
  case CURRENT:
    mprintf(where, ".model  %s  csw  (", label);
    mprintf(where, " it=%s ",  ftos(vt,  "", 7, 0));
    mprintf(where, " ih=%s ",  ftos(vh,  "", 7, 0));
    break;
  }
  mprintf(where, " ron=%s ",   ftos(ron,   "", 7, 0));
  mprintf(where, " roff=%s ",  ftos(roff,  "", 7, 0));
  mprintf(where, ")\n");
}
/*--------------------------------------------------------------------------*/
SWITCH_BASE::SWITCH_BASE()
{
  devclass = ONEPORT;
  attach_common(&Default_SWITCH);
  inputlabel[0] = '\0';
  input = NULL;
  ic = current_state = previous_state = UNKNOWN;
}
/*--------------------------------------------------------------------------*/
SWITCH_BASE::SWITCH_BASE(CONST SWITCH_BASE& p):ELEMENT(p)
{
  strcpy(inputlabel,p.inputlabel);
  input = NULL;
  ic = p.ic;
  current_state = p.current_state;
  previous_state = p.previous_state;
}
/*--------------------------------------------------------------------------*/
void SWITCH_BASE::parse_sb(CS& cmd, int numnodes)
{
  assert(common);
  SWITCH_COMMON* c = new SWITCH_COMMON(*(const SWITCH_COMMON*)common);
  assert(c);

  parselabel(cmd);
  parsenodes(cmd, numnodes);
  if (numnodes == IPRINTNODES){		// if current controlled
    cmd.ctostr(inputlabel, LABELEN, TOKENTERM);
    inputlabel[0] = to_upper(inputlabel[0]);
  }
  cmd.ctostr(c->modelname, LABELEN, TOKENTERM);
  c->model = NULL;
  cmd.stuck();
  /**/ ::set(cmd, "OFF",    &ic, OFF)
    || ::set(cmd, "ON",	    &ic, ON)
    || ::set(cmd, "UNKNOWN",&ic, UNKNOWN);
  if (cmd.stuck()){
    cmd.check(bWARNING);
  }
  attach_common(c);
}
/*--------------------------------------------------------------------------*/
void SWITCH_BASE::print(int where, int)const
{
  const SWITCH_COMMON* c = (const SWITCH_COMMON*)common;
  printlabel(where);
  printnodes(where, NUMNODES);
  if (input){
    mputc(' ', where);
    input->printlabel(where);
  }else{
    mprintf(where, " %s ", inputlabel);
  }
  mprintf(where, " %s ", c->modelname);

  switch (ic){
    case OFF:	   mprintf(where, " off ");	break;
    case ON:	   mprintf(where, " on ");	break;
    case UNKNOWN: 				break;
  }
  mprintf(where, "\n");
}
/*--------------------------------------------------------------------------*/
void SWITCH_BASE::expand_sb()
{
  const MODEL_SWITCH* m = (const MODEL_SWITCH*)attach_model();
  assert(m);

  val = m->ron;
  y0.f0 = LINEAR;
  y0.f1 = (ic == ON) ? m->ron : m->roff;	/* unknown is off */
  previous_state = current_state = ic;
  m0.f1 = 1./y0.f1;
  m0.c0 = 0.;
  ev    = y0.f1;
  acg   = m0.f1;
  nodamp = true;
  assert(loss == 0.);
  assert(!constant); /* depends on input */
}
/*--------------------------------------------------------------------------*/
bool SWITCH_BASE::dotr()
{
  assert(!isbypassed());

  const SWITCH_COMMON* c = (const SWITCH_COMMON*)common;
  assert(c);
  const MODEL_SWITCH* m = (const MODEL_SWITCH*)c->model;
  assert(m);

  state_t new_state;
  
  if (STATUS::iter[iSTEP] <= 1){
    if (SIM::phase == pINIT_DC){   
      y0.f1 = (ic == ON) ? m->ron : m->roff;	/* unknown is off */
      current_state = ic;
      m0.f1 = 1./y0.f1;
    }
    previous_state = current_state;
  }
  
  y0.x = (input)			/* y0.x is controlling value */
    ? CARD::probe(input,"I")		/* current controlled */
    : n[IN1].v0() - n[IN2].v0();	/* voltage controlled */
  if (y0.x > m->vt + m->vh){
    new_state = ON;
  }else if (y0.x < m->vt - m->vh){
    new_state = OFF;
  }else{
    new_state = previous_state;
  }

  if (new_state != current_state){
    y0.f1 = (new_state == ON) ? m->ron : m->roff;	/* unknown is off */
    current_state = new_state;
    m0.f1 = 1./y0.f1;
    SIM::loadq << this;
    store_values();
    converged = false;
  }else{
    assert(y1 == y0);
    converged = true;
  }
  return converged;
}
/*--------------------------------------------------------------------------*/
void SWITCH_BASE::doac()
{
  ev  = y0.f1;
  acg = m0.f1;
  acload_passive();
}
/*--------------------------------------------------------------------------*/
void DEV_CSWITCH::expand()
{
  input = findbranch_samescope(inputlabel,this);
  if (!input)
    error(bERROR,"%s: can't find %s\n",printlabel(),inputlabel);
  expand_sb();
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
