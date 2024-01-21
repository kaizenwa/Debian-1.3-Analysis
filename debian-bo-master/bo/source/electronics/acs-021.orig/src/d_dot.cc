/*$Id: d_dot.cc,v 11.22 96/02/18 11:42:45 al Exp $ -*- C++ -*-
 * processing for DOTCARD netlist item (pseudo-device)
 */
#include "ap.h"
#include "d_dot.h"
#include "error.h"
#include "io.h"
#include "mode.h"
#include "c_comand.h"
/*--------------------------------------------------------------------------*/
//	void	DEV_DOT::parse(CS& cmd);
//	void	DEV_DOT::print(int,int)const;
/*--------------------------------------------------------------------------*/
extern run_mode_t run_mode;
/*--------------------------------------------------------------------------*/
void DEV_DOT::parse(CS& cmd)
{
  size_t maxlen = (char*)&(acbias) - (char*)&(label[0]);
  strncpy(label, cmd.fullstring(), maxlen);
  label[maxlen] = '\0';
  if (run_mode != rIGNORE){
    cmd.skip();
    CMD::cmdproc(cmd.tail());
  }
}
/*--------------------------------------------------------------------------*/
void DEV_DOT::print(int where, int)const
{
  mprintf(where, "%s\n", label);
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
