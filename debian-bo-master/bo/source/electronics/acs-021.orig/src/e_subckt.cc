/*$Id: e_subckt.cc,v 11.30 96/03/17 19:21:11 al Exp $ -*- C++ -*-
 * Base class for elements of a circuit
 */
#include "error.h"
#include "e_subckt.h"
#include "io_trace.h"
/*--------------------------------------------------------------------------*/
// 	bool	BASE_SUBCKT::dotr();
// 	void	BASE_SUBCKT::trload();
// 	void	BASE_SUBCKT::trunload();
// 	void	BASE_SUBCKT::doac();
//	double	BASE_SUBCKT::tr_review();
/*--------------------------------------------------------------------------*/
bool BASE_SUBCKT::dotr()
{
  assert(subckt);
  bypass = (parent && parent->bypass);
  if (1 || !bypass || !SIM::inc_mode){
    converged = subckt->dotr_group();
  }else{
    trace0(printlabel());
    assert(0);
    assert(converged);
  }
  return converged;
}
/*--------------------------------------------------------------------------*/
void BASE_SUBCKT::trload()
{
  assert(subckt);
  subckt->trload_group();
}
/*--------------------------------------------------------------------------*/
void BASE_SUBCKT::trunload()
{
  assert(subckt);
  subckt->trunload_group();
}
/*--------------------------------------------------------------------------*/
void BASE_SUBCKT::doac()
{
  assert(subckt);
  subckt->doac_group();
}
/*--------------------------------------------------------------------------*/
double BASE_SUBCKT::tr_review()
{
  assert(subckt);
  return subckt->tr_review_group();
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
