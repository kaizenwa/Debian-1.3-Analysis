/*$Id: e_compon.cc,v 11.38 96/03/24 17:59:50 al Exp $ -*- C++ -*-
 * Base class for elements of a circuit
 */
#include "ap.h"
#include "e_compon.h"
#include "io.h"
#include "l_compar.h"
#include "error.h"
#include "l_lib.h"
/*--------------------------------------------------------------------------*/
//	const MODEL_Base* COMPONENT_COMMON::attach_model()const;
//	void	COMPONENT::parselabel(CS&);
//	int	COMPONENT::parsenodes(CS&,int);
//	void	COMPONENT::printnodes(int,int)const;
//	void	COMPONENT::set(............);
//	void	COMPONENT::set(............);
//	void	COMPONENT::tr_eval();
//	void	COMPONENT::ac_eval();
//	void	COMPONENT::attach_common(const COMPONENT_COMMON *);
//	void	COMPONENT::detach_common();
//	bool	COMPONENT::conv_check()const;
static	int	name2number(CS&);
static	char*	number2name(char*,int);
/*--------------------------------------------------------------------------*/
const MODEL_Base* COMPONENT_COMMON::attach_model()const
{
  COMPONENT_COMMON* c = (COMPONENT_COMMON*)this;
  assert(c);
  c->model = MODEL_CARD::rootmodel->find_model(c->modelname);
  assert(c->model);
  return c->model;
}
/*--------------------------------------------------------------------------*/
void COMPONENT::parselabel(CS& cmd)
{
  cmd.ctostr(label, LABELEN, TOKENTERM);
  label[0] = to_upper(label[0]);
}
/*--------------------------------------------------------------------------*/
/* parsenodes: parse circuit connections from input string
 * result in brh.
 * n array must hold at least numnodes+1
 * cnt updated.
 */
int COMPONENT::parsenodes(CS& cmd, int numnodes)
{
  int count = 0;
  int ii;
  for (ii = 0;  ii < numnodes;  ii++){
    n[ii].t = n[ii].e = name2number(cmd);
    if (n[ii].e != INVALIDNODE)
      count = ii+1;
  }
  n[ii].t = n[ii].e = INVALIDNODE;
  return count;
}
/*--------------------------------------------------------------------------*/
/* printnodes: print a node list
 */
void COMPONENT::printnodes(int where, int numnodes)const
{
  for (int ii = 0;   ii < numnodes  &&  n[ii].e != INVALIDNODE;   ii++){
    char name[LABELEN+1];
    mprintf(where, " %s ", number2name(name,n[ii].e));
  }
}
/*--------------------------------------------------------------------------*/
/* set: set values, used in model building
 */
void COMPONENT::set(const char *Label, CARD *Parent,
		    CONST COMPONENT_COMMON *Common, double Value,
		    node_t N0, node_t N1)
{
  strncpy(label, Label, LABELEN);
  label[LABELEN] = '\0';
  label[0] = to_upper(label[0]);
  parent = Parent;
  attach_common(Common);
  y0.f1 = val = Value;
  y0.x = y0.f0 = 0.;
  n[0] = N0;
  n[1] = N1;
}
/*--------------------------------------------------------------------------*/
/* set: set values, used in model building
 */
void COMPONENT::set(const char *Label, CARD *Parent,
		    CONST COMPONENT_COMMON *Common, double Value,
		    node_t N0, node_t N1, node_t N2, node_t N3)
{
  set(Label, Parent, Common, Value, N0, N1);
  n[2] = N2;
  n[3] = N3;
}
/*--------------------------------------------------------------------------*/
void COMPONENT::tr_eval()
{
  if (common && common->has_tr_eval()){
    common->tr_eval(this);
  }else if (trfun){
    (*trfun)(this);
  }else{
    y0.f1 = val;
    y0.f0 = y0.x * y0.f1;
  }
  converged = conv_check();
}
/*--------------------------------------------------------------------------*/
void COMPONENT::ac_eval()
{
  if (common && common->has_ac_eval()){
    common->ac_eval(this);
  }else if (acfun){
    (*acfun)(this);
  }else{
    ev = y0.f1;
  }
}
/*--------------------------------------------------------------------------*/
void COMPONENT::attach_common(CONST COMPONENT_COMMON *c)
{
  detach_common();
  if (c){
    common = c->attach();
  }
}
/*--------------------------------------------------------------------------*/
void COMPONENT::detach_common()
{
  if (common){
    if (common->detach() == 0){
      COMPONENT_COMMON *c = (COMPONENT_COMMON*)common;
      delete c;
      common = NULL;
    }
  }
}
/*--------------------------------------------------------------------------*/
/* conv_check: check branch for convergence
 * should be a macro for speed
 */
bool COMPONENT::conv_trace()const
{
  bool isconverged = conchk(y1.f1, y0.f1, OPT::abstol)
		  && conchk(y1.f0, y0.f0, OPT::abstol);

  if ((STATUS::iter[iSTEP] >= OPT::itl[OPT::TRACE]) && !isconverged){
    error(bPICKY,"%s: non-convergence\n", printlabel());
    error(bPICKY,"(x) (%s,%s,%s)\n",
          ftos(y0.x, "", 7, 0),
          ftos(y1.x, "", 7, 0),
          ftos(y2.x, "", 7, 0));
    error(bPICKY,"(f0) (%s,%s,%s)\n",
          ftos(y0.f0, "", 7, 0),
          ftos(y1.f0, "", 7, 0),
          ftos(y2.f0, "", 7, 0));
    error(bPICKY,"(f1) (%s,%s,%s)\n",
	  ftos(y0.f1, "", 7, 0),
	  ftos(y1.f1, "", 7, 0),
	  ftos(y2.f1, "", 7, 0));
  }
  return isconverged;
}
/*--------------------------------------------------------------------------*/
/* name2number: convert node name to node number
 * returns node number
 * cnt updated
 */
static int name2number(CS& cmd)
{
  int test = cmd.cursor();
  int node = cmd.ctoi();
  return  (test == cmd.cursor())  ?  INVALIDNODE  :  node;
}
/*--------------------------------------------------------------------------*/
/* number2name: convert node number to node name
 * result in name (must be big enough)
 * returns name
 */
static char* number2name(char *name, int node)
{
  sprintf(name, "%d", node);
  return name;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
