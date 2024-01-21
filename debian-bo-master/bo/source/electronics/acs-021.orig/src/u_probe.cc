/*$Id: u_probe.cc,v 11.22 96/02/18 11:45:18 al Exp $ -*- C++ -*-
 * general probe object
 */
#include "ap.h"
#include "e_node.h"
#include "e_card.h"
#include "u_status.h"
#include "u_probe.h"
#include "s__.h"
/*--------------------------------------------------------------------------*/
//		PROBE::PROBE(const char*,int, CKT_BASE*,double,double);
//	PROBE & PROBE::detach();
//	char*	PROBE::label(void)const;
//	double	PROBE::value(void)const;
//	double	PROBE::probe_node(void)const;
/*--------------------------------------------------------------------------*/
extern NODE* nstat;
/*--------------------------------------------------------------------------*/
PROBE::PROBE(const char *What, int Node, CKT_BASE *Brh, double Lo, double Hi)
{ 
  strcpy(what,What);
  node = Node;
  brh = Brh;
  low = Lo;
  high = Hi;
}
/*--------------------------------------------------------------------------*/
PROBE & PROBE::detach()
{
  if (brh){
    brh->decprobes();
    brh = NULL;
  }
  *what = '\0';
  node = 0;
  brh = NULL;
  return *this;
}
/*--------------------------------------------------------------------------*/
/* label: returns a string corresponding to a possible probe point
 * (suitable for printing)
 * It has nothing to do with whether it was selected or not
 * the return string is static, and overwritten on every call.
 */
char* PROBE::label(void)const
{
  static char string[BUFLEN+8];
  assert(!(brh && node));
  if (*what){
    if (!brh){
      sprintf(string, "%s(%u)", what, node);
    }else{
      sprintf(string, "%s(%s)", what, brh->printlabel());
    }
  }else{
    if (!brh){
      sprintf(string, "un(%u)", node);
    }else{
      sprintf(string, "un(%s)", brh->printlabel());
    }
  }
  strcpy(&(string[BUFLEN-3]), "++");
  return string;
}
/*--------------------------------------------------------------------------*/
double PROBE::value(void)const
{
  assert(!(brh && node));
  if (!brh){
    if (node <= STATUS::total_nodes  &&  nstat[NODE::to_internal(node)].needsanalog){
      return nstat[NODE::to_internal(node)].probe_num(what);
    }else{
      return probe_node();
    }
  }else{
    return brh->probe_num(what);
  }
}
/*--------------------------------------------------------------------------*/
double PROBE::probe_node(void)const
{
  CS cmd(what);
  if (cmd.pmatch("ITer")  &&  node < iCOUNT){
    return (double)STATUS::iter[node];
  }else if (cmd.pmatch("Control")  &&  node < cCOUNT){
    return (double)STATUS::control[node];
  }else if (cmd.pmatch("DAMP")){
    return SIM::damp;
  }else if (cmd.pmatch("GEnerator")){
    return SIM::genout;
  }else if (cmd.pmatch("TEmperature")){
    return SIM::temp + ABS_ZERO;
  }else if (cmd.pmatch("TIme")){
    return SIM::time0;
  }else{
    return NOT_VALID;
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
