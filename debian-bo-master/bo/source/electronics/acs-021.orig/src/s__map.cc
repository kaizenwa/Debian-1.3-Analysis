/*$Id: s__map.cc,v 11.22 96/02/18 11:44:37 al Exp $ -*- C++ -*-
 * Allocates space for the admittance matrix.  Allocation below the
 * diagonal is by row, above the diagonal is by column, and stored backwards.
 * This broken vector is stored.  The length of it increases with increasing
 * position.  The maximum length of the nth vector is 2n-1.  For a band matrix
 * only those elements that are non-zero or are nearer to the diagonal than a
 * non-zero element are stored.
 */
#include "e_node.h"
#include "e_card.h"
#include "error.h"
#include "u_opt.h"
#include "u_status.h"
#include "s__.h"
/*--------------------------------------------------------------------------*/
//static void	 SIM::map_nodes();
//static void	 SIM::order_reverse();
//static void	 SIM::order_forward();
//static void	 SIM::order_auto();
//static void	 SIM::map_list(CARD*);
/*--------------------------------------------------------------------------*/
/* map_nodes: map intermediate node number to internal node number.
 * Ideally, this function would find some near-optimal order
 * and squash out gaps.
 */
/*static*/ void SIM::map_nodes()
{
  nm = new int[STATUS::total_nodes+1];
  STATUS::order.reset().start();
  switch (OPT::order){
    default:
      error(bWARNING, "invalid order spec: %d\n", OPT::order);
    case OPT::oAUTO:	 order_auto();	   break;
    case OPT::oREVERSE:	 order_reverse();  break;
    case OPT::oFORWARD:	 order_forward();  break;
  }
  STATUS::order.stop();
  map_list(CARD::first());
}
/*--------------------------------------------------------------------------*/
/* order_reverse: force ordering to reverse of user ordering
 *  subcircuits at beginning, results on border at the bottom
 */
/*static*/ void SIM::order_reverse()
{
  nm[0] = 0;
  for (int node = 1;  node <= STATUS::total_nodes;  ++node)
    nm[node] = STATUS::total_nodes - node + 1;
}
/*--------------------------------------------------------------------------*/
/* order_forward: use user ordering, with subcircuits added to end
 * results in border at the top (worst possible if lots of subcircuits)
 */
/*static*/ void SIM::order_forward()
{
  nm[0] = 0;
  for (int node = 1;  node <= STATUS::total_nodes;  ++node)
    nm[node] = node;
}
/*--------------------------------------------------------------------------*/
/* order_auto: full automatic ordering
 * reverse, for now
 */
/*static*/ void SIM::order_auto()
{
  nm[0] = 0;
  for (int node = 1;  node <= STATUS::total_nodes;  ++node)
    nm[node] = STATUS::total_nodes - node + 1;
}
/*--------------------------------------------------------------------------*/
/* map_list: map intermediate node to internal (working) node number.
 * recursive: actual mapping done here
 */
/*static*/ void SIM::map_list(CARD *stop)
{
  assert(exists(stop));
  CARD *brh;
  brh = stop;
  do {
    if (brh->isdevice()){
      int ii;
      for (ii = 0;  brh->n[ii].t != INVALIDNODE;  ii++){
	brh->n[ii].m = nm[brh->n[ii].t];
      }
      brh->n[ii].m = INVALIDNODE;
      if (brh->sub()){
	map_list(brh->sub());
      }
    }
  } while (brh = brh->next(),  brh != stop);
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
