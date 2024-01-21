/*$Id: s_ac_slv.cc,v 11.22 96/02/18 11:44:47 al Exp $ -*- C++ -*-
 * ac solution
 */
#include "m_matrix.h"
#include "u_status.h"
#include "e_node.h"
#include "e_card.h"
#include "s_ac.h"
/*--------------------------------------------------------------------------*/
//	void	AC::solve(void);
//	void	AC::clear(void);
/*--------------------------------------------------------------------------*/
void AC::solve(void)
{
  clear();
  STATUS::load.start();
  ++STATUS::iter[iTOTAL];
  CARD::doac_all();
  STATUS::load.stop();
  acx.lu_decomp();
  acx.fbsub(ac);
}
/*--------------------------------------------------------------------------*/
void AC::clear(void)
{
  acx.zero();
  for (int ii=0;  ii <= STATUS::total_nodes;  ++ii){
    ac[ii] = 0.;
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
