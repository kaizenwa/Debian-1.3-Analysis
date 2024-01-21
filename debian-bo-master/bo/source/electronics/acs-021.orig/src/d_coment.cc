/*$Id: d_coment.cc,v 11.22 96/02/18 11:42:40 al Exp $ -*- C++ -*-
 * processing for COMMENT netlist item (pseudo-device)
 */
#include "ap.h"
#include "d_coment.h"
#include "error.h"
#include "io.h"
/*--------------------------------------------------------------------------*/
//	void	DEV_COMMENT::parse(CS& cmd);
//	void	DEV_COMMENT::print(int,int)const;
/*--------------------------------------------------------------------------*/
void DEV_COMMENT::parse(CS& cmd)
{
  size_t maxlen = (char*)&(acbias) - (char*)&(label[0]);
  strncpy(label, cmd.fullstring(), maxlen);
  label[maxlen] = '\0';
}
/*--------------------------------------------------------------------------*/
void DEV_COMMENT::print(int where, int)const
{
  if (label[0] != '*'  ||  label[1] != '+')
    mprintf(where, "%s\n", label);
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
