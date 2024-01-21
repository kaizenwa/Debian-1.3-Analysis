/*$Id: s_ac.cc,v 11.22 96/02/18 11:44:42 al Exp $ -*- C++ -*-
 * ac analysis top
 */
#include "m_matrix.h"
#include "u_status.h"
#include "u_opt.h"
#include "s_ac.h"
/*--------------------------------------------------------------------------*/
//	void	AC::command(CS&);
/*--------------------------------------------------------------------------*/
extern run_mode_t run_mode;
/*--------------------------------------------------------------------------*/
void AC::command(CS& cmd)
{
  mode = sAC;
  reset_timers();
  STATUS::ac.reset().start();
  init();
  alloc_vectors();
  acx.allocate().setminpivot(OPT::pivtol);
  setup(cmd);
  STATUS::set_up.stop();
  if (run_mode == rEXECUTE)
    sweep();
  acx.unallocate();
  unalloc_vectors();

  STATUS::ac.stop();
  STATUS::total.stop();
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
