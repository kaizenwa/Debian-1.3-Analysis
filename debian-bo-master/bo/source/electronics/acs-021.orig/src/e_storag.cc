/*$Id: e_storag.cc,v 11.27 96/03/03 16:38:57 al Exp $ -*- C++ -*-
 * Base class for storage elements of a circuit
 */
#include "e_storag.h"
#include "u_opt.h"
#include "s__.h"
/*--------------------------------------------------------------------------*/
	STORAGE::STORAGE(){method_a=mUNKNOWN;}
	STORAGE::STORAGE(CONST STORAGE& p):ELEMENT(p){method_a=p.method_a;}
//	void	STORAGE::expand_pre();
//	bool	STORAGE::advance();
/*--------------------------------------------------------------------------*/
void STORAGE::expand_pre()
{
  switch (method_u){
    case mGEAR:
    case mTRAPEZOID: method_a = method_u;    break;
    case mSTIFF:     method_a = mGEAR;	     break;
    case mUNKNOWN:   method_a = OPT::method; break;
  }
}
/*--------------------------------------------------------------------------*/
/* advance_time: set up guesses for model evaluation
 * saves old model inputs, and computes new inputs (volts and current)
 * also saves old matrix parameters, for convergence checking
 * return true if time has changed
 */
bool STORAGE::advance()
{
  if (SIM::mode == sDC  ||  SIM::phase == pINIT_DC){	// initial dc
    mt1 = m0;
    time0 = SIM::time0;
    time1 = time2 = time3 = 0.;
    return false;
  }else if (time0 != SIM::time0){	// new time step
    if (time0 < SIM::time0){		// forward
      mt1 = m0;
      yt3 = yt2;
      yt2 = yt1;
      yt1 = y0;
      time3 = time2;
      time2 = time1;
      time1 = time0;
    }					// else backward, don't save
    time0 = SIM::time0;
    return true;
  }else{
    return false;
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
