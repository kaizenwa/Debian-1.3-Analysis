/*$Id: u_statu2.cc,v 11.22 96/02/18 11:45:22 al Exp $ -*- C++ -*-
 * Displays the status of the system.  Makes all the calculations associated
 * with allocating memory but does not actually allocate it, unless necessary
 * to make the rest of the calculations.
 *
 *   If "allocate" is changed, this must also be changed.
 */
#include "io.h"
#include "m_matrix.h"
#include "c_comand.h"
#include "d_diode.h"
#include "d_logic.h"
#include "d_mos.h"
#include "d_subckt.h"
#include "u_opt.h"
#include "u_status.h"
#include "s__.h"
/*--------------------------------------------------------------------------*/
//	void	STATUS::command(CS& cmd);
/*--------------------------------------------------------------------------*/
void STATUS::command(CS&)
{ 
  mprintf(IO::mstdout,"ACS   System status\n");
  init();
    
  overhead = total - evaluate - load - lud - back - output - review;

  mprintf(IO::mstdout,
  "command      --------  last  --------    --------  total  --------\n");
  mprintf(IO::mstdout,
  "               user      sys    total       user      sys    total\n");
  get.print();
  op.print();
  dc.print();
  tran.print();
  four.print();
  ac.print();

  mprintf(IO::mstdout,
  "function     --------  last  --------    --------  total  --------\n");
  mprintf(IO::mstdout,
  "               user      sys    total       user      sys    total\n");
  set_up.print();
  order.print();

  
  mprintf(IO::mstdout,
  "function     --------  last  --------    --------  total  --------\n");
  mprintf(IO::mstdout,
  "               user      sys    total       user      sys    total\n");
  evaluate.print();
  load.print();
  lud.print();
  back.print();
  review.print();
  output.print();
  overhead.print();
  if (OPT::showall){
    aux1.print();
    aux2.print();
    aux3.print();
  }
  total.print();

  mprintf(IO::mstdout,
	  "iterations: op=%d, dc=%d, tran=%d, fourier=%d, total=%d\n",
	  iter[sOP], iter[sDC], iter[sTRAN], iter[sFOURIER], iter[iTOTAL]);
  mprintf(IO::mstdout,
	  "nodes: user=%d, subckt=%d, model=%d, total=%d\n",
	  user_nodes, subckt_nodes, model_nodes, total_nodes);
  mprintf(IO::mstdout,
	  "diodes=%d, bjts=%d, jfets=%d, mosfets=%d, gates=%d, subckts=%d\n",
	  DEV_DIODE::count(), 0, 0, DEV_MOS::count(),
	  DEV_LOGIC::count(), DEV_SUBCKT::count());
  mprintf(IO::mstdout,"density=%.1f%%\n", aa.density()*100.);
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
