/*$Id: e_card3.cc,v 11.22 96/02/18 11:43:18 al Exp $ -*- C++ -*-
 * Base class for "cards" in the circuit description file
 * This file contains methods to manage a list of them
 *		including definitions of the base and static elements
 */
#include "e_card.h"
#include "d_coment.h"
/*--------------------------------------------------------------------------*/
//	CARD*	CARD::insertbefore(CARD* before);
//	CARD*	CARD::insertafter(CARD* after);
//	void	CARD::unlink();
/*--------------------------------------------------------------------------*/
CARD* CARD::rootcard = new DEV_COMMENT;
CARD* CARD::putbefore = NULL;
/*--------------------------------------------------------------------------*/
CARD* CARD::insertbefore(CARD* before)
{
  assert(prevcard == this);  // not already linked
  assert(nextcard == this);
  if (before){
    prevcard = before->prev();
    nextcard = before;
  }
  nextcard->prevcard = prevcard->nextcard = this;

  if (stprev){
    stnext = stprev->stnext;
  }else if (stnext){
    stprev = stnext->stprev;
  }else{ /* make self link */
    stprev = stnext = this;
  }
  stnext->stprev = stprev->stnext = this;
  return this;
}    
/*--------------------------------------------------------------------------*/
CARD* CARD::insertafter(CARD* after)
{
  assert(prevcard == this);  // not already linked
  assert(nextcard == this);
  if (after){
    nextcard = after->next();
    prevcard = after;
  }
  nextcard->prevcard = prevcard->nextcard = this;

  if (stprev){
    stnext = stprev->stnext;
  }else if (stnext){
    stprev = stnext->stprev;
  }else{ /* make self link */
    stprev = stnext = this;
  }
  stnext->stprev = stprev->stnext = this;
  return this;
}    
/*--------------------------------------------------------------------------*/
/* unlinkbranch: unlink a branch from parts list
 * doesn't free memory, just unlinks it
 * returns count actually unlinked (either 1 or 0)
 * updates pointers, first, last, etc.
 * so list is consistent on return
 */
void CARD::unlink()
{
  assert(nextcard);             /* BUG:: for unknown reasons,           */
  assert(prevcard);             /* stprev may be NULL on final cleanup  */
  //assert(stnext);             /* causing dump on exit                 */
  //assert(stprev);             /* shows only on SGI with NCC or DCC    */
  nextcard->prevcard = prevcard;
  prevcard->nextcard = nextcard;
  if (stnext)
    stnext->stprev = stprev;
  if (stprev)
    stprev->stnext = stnext;
  nextcard = prevcard = stnext = stprev = this;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
