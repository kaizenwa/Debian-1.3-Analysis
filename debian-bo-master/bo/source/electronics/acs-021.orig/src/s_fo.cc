/*$Id: s_fo.cc,v 11.22 96/02/18 11:44:55 al Exp $ -*- C++ -*-
 * tran and fourier commands -- top
 * performs transient analysis, silently, then fft.
 * outputs results of fft
 */
#include "m_matrix.h"
#include "u_opt.h"
#include "u_status.h"
#include "s_fo.h"
/*--------------------------------------------------------------------------*/
//	void	FOURIER::command(CS&);
/*--------------------------------------------------------------------------*/
extern run_mode_t run_mode;
/*--------------------------------------------------------------------------*/
void FOURIER::command(CS& cmd)
{
  mode = sFOURIER;
  reset_timers();
  STATUS::four.reset().start();
  init();
  alloc_vectors();
  aa.allocate().dezero(OPT::gmin).setminpivot(OPT::pivtol);
  lu.allocate().dezero(OPT::gmin).setminpivot(OPT::pivtol);
  setup(cmd);
  fftallocate();
  STATUS::set_up.stop();
  if (run_mode == rEXECUTE){
    sweep();
    foout();
  }
  fftunallocate();
  unalloc_vectors();
  lu.unallocate();
  aa.unallocate();
  
  STATUS::four.stop();
  STATUS::total.stop();
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
