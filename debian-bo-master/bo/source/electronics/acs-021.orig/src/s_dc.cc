/*$Id: s_dc.cc,v 11.22 96/02/18 11:44:50 al Exp $ -*- C++ -*-
 * dc analysis top
 */
#include "u_status.h"
#include "s_dc.h"
/*--------------------------------------------------------------------------*/
//	void	DC::command(CS&);
//	void	OP::command(CS&);
/*--------------------------------------------------------------------------*/
void DC::command(CS& cmd)
{
  time0 = -1.;
  mode = sDC;
  STATUS::dc.reset().start();
  command_base(cmd);
  STATUS::dc.stop();
}
/*--------------------------------------------------------------------------*/
void OP::command(CS& cmd)
{
  time0 = -1.;
  mode = sOP;
  STATUS::op.reset().start();
  command_base(cmd);
  STATUS::op.stop();
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
