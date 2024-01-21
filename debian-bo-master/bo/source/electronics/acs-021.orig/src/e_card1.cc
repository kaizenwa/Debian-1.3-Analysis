/*$Id: e_card1.cc,v 11.34 96/03/21 22:37:49 al Exp $ -*- C++ -*-
 * Base class for "cards" in the circuit description file
 * This file contains iterators -- functions that process a list of cards
 */
#include "e_card.h"
/*--------------------------------------------------------------------------*/
//	void	CARD::expand_group();
//	bool	CARD::dotr_group();
//	void	CARD::trload_group();
//	void	CARD::trunload_group();
//	void	CARD::doac_group();
//	double	CARD::tr_review_group();
/*--------------------------------------------------------------------------*/
/* expand_group: expand (flatten) a list of components (subckts)
 * Scan component list.  Expand each subckt: create actual elements
 * for flat representation to use for simulation.
 * Recursive to allow for nested subckts.
 */
void CARD::expand_group()
{
  CARD *brh;
  brh = this;
  do {
    brh->expand();
    brh = brh->next();
  } while (brh != this);
}
/*--------------------------------------------------------------------------*/
/* dotr_group: evaluate a list of models
 * evaluates a list (or sublist), checks convergence, etc.
 * does not load the matrix
 * argument is the head of the netlist.
 * recursively called to evaluate subcircuits
 */
bool CARD::dotr_group()
{
  assert(!isbypassed());
  bool isconverged = true;
  CARD *brh;
  brh = this;
  do {
    if (!brh->constant && !brh->evaluated()){
      isconverged &= brh->dotr();
    }
    brh = brh->next();
  } while (brh != this);
  return isconverged;
}
/*--------------------------------------------------------------------------*/
/* trload_group: load list of models to the matrix
 * recursively called to load subcircuits
 */
void CARD::trload_group()
{
  CARD *brh;
  brh = this;
  do {
    if (!brh->constant || !SIM::inc_mode)
      brh->trload();
    brh = brh->next();
  } while (brh != this);
}
/*--------------------------------------------------------------------------*/
/* trunload_group: remove a list of models from the matrix
 * recursively called to unevaluate subcircuits
 */
void CARD::trunload_group()
{
  CARD *brh;
  brh = this;
  do {
    brh->trunload();
    brh = brh->next();
  } while (brh != this);
}
/*--------------------------------------------------------------------------*/
void CARD::doac_group()
{
  CARD *brh;
  brh = this;
  do {
    if (!brh->evaluated())
      brh->doac();
    brh = brh->next();
  } while (brh != this);
}
/*--------------------------------------------------------------------------*/
double CARD::tr_review_group()
{
  double worsttime = BIGBIG;
  double thistime;
  CARD *brh;
  brh = this;
  do {
    thistime = brh->tr_review();
    if (thistime < worsttime){
      worsttime = thistime;
    }
    brh = brh->next();
  } while (brh != this);
  return worsttime;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
