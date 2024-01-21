/*$Id: s__aux.cc,v 11.22 96/02/18 11:44:33 al Exp $ -*- C++ -*-
 * aux functions associated with the SIM class
 */
#include "e_node.h"
#include "u_status.h"
#include "s__.h"
/*--------------------------------------------------------------------------*/
//	void SIM::keep();
//	void SIM::restore();
/*--------------------------------------------------------------------------*/
void SIM::keep()
{
  if (!freezetime){
    for (int ii = 1;  ii <= STATUS::total_nodes;  ii++){
      vdc[ii] = v0[ii];
    }
    last_time = (time0 > 0.) ? time0 : 0.;
  }
}
/*--------------------------------------------------------------------------*/
void SIM::restore()
{
  for (int ii = 1;  ii <= STATUS::total_nodes;  ii++){
    v0[ii] = vdc[ii];
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
