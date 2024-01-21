/*$Id: e_card4.cc,v 11.22 96/02/18 11:43:20 al Exp $ -*- C++ -*-
 * Base class for "cards" in the circuit description file
 * This file contains query methods
 */
#include "io.h"
//#include "u_status.h"
#include "e_card.h"
/*--------------------------------------------------------------------------*/
//	char*	CARD::printlabel(int where=0)const;
//	double	CARD::probe(const CARD*,const char*);
// 	int	CARD::connects_to(int node)const;
/*--------------------------------------------------------------------------*/
double CARD::initial_voltage = NOT_INPUT;
double CARD::initial_current = NOT_INPUT;
double CARD::initial_condition = NOT_INPUT;
/*--------------------------------------------------------------------------*/
char* CARD::printlabel(int where)const
{
  static char buffer[BUFLEN];/* CAUTION: static area overwritten every call */
  const CARD *brh;
  brh = this;
  strcpy(buffer, label);
  while (brh = brh->parent, exists(brh)){
    strcat(buffer, ".");
    strncat(buffer, brh->label, BUFLEN-strlen(buffer)-2);
  }
  strcpy(&(buffer[BUFLEN-3]), "++");
  if (where)
    mprintf(where, "%s  ", buffer);
  return buffer;
}
/*--------------------------------------------------------------------------*/
/*static*/ double CARD::probe(const CARD *This, const char *what)
{
  if (exists(This)){
    return This->probe_num(what);
  }else{				/* return 0 if doesn't exist */
    return 0.0;				/* happens when optimized models */
  }					/* don't have all parts */
}
/*--------------------------------------------------------------------------*/
/* connects_to: does this part connect to this node?
 * input: a node
 * returns: how many times this part connects to it.
 * does not traverse subcircuits
 */
int CARD::connects_to(int node)const
{
  int count = 0;
  if (isdevice()){
    for (int ii = 0;  n[ii].t != INVALIDNODE;  ii++){
      if (node == n[ii].t)
        ++count;
    }
  }
  return count;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
