/*$Id: c_insert.cc,v 11.22 96/02/18 11:42:13 al Exp $ -*- C++ -*-
 * insert command
 * insert new nodes
 */
#include "ap.h"
#include "e_card.h"
#include "error.h"
#include "u_status.h"
#include "c_comand.h"
#include "s__.h"
/*--------------------------------------------------------------------------*/
//	void	CMD::insert(CS&);
/*--------------------------------------------------------------------------*/
void CMD::insert(CS& cmd)
{
  int nod;		/* first node number to insert */
  int ncnt;		/* how many nodes to insert */
  CARD *brh;		/* branch pointer, for loop */
  CARD *stop;		/* stop loop */
  
  SIM::uninit();
  nod = cmd.ctoi();
  if (nod <= 0)
    error(bERROR, "insert which node?\n");
  if (nod > STATUS::total_nodes)
    error(bERROR, "%u nodes\n", STATUS::total_nodes);
  
  ncnt = cmd.ctoi();
  if (ncnt <= 0)
    ncnt = 1;
  
  stop = brh = CARD::first();
  assert(exists(stop));
  do {
    if (brh->isdevice()){
      for (int ii = 0;  brh->n[ii].e != INVALIDNODE;  ii++){
	if (brh->n[ii].e >= nod)		/* move them up */
	  brh->n[ii].e += ncnt;
      }
    }
  } while (brh = brh->next(), brh != stop);
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
