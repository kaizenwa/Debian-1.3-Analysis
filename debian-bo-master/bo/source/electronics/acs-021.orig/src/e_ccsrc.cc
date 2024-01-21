/*$Id: e_ccsrc.cc,v 11.22 96/02/18 11:43:21 al Exp $ -*- C++ -*-
 * current controlled source base
 */
#include "ap.h"
#include "e_ccsrc.h"
#include "io.h"
#include "l_compar.h"
/*--------------------------------------------------------------------------*/
//	void	CCSRC_BASE::parse(CS& cmd);
//	void	CCSRC_BASE::print(int,int)const;
/*--------------------------------------------------------------------------*/
void CCSRC_BASE::parse(CS& cmd)
{
  parselabel(cmd);
  parsenodes(cmd,PRINTNODES);
  cmd.ctostr(inputlabel, LABELEN, TOKENTERM);
  inputlabel[0] = to_upper(inputlabel[0]);
  parseexpr(cmd);
}
/*--------------------------------------------------------------------------*/
void CCSRC_BASE::print(int where, int /*detail*/)const
{
  printlabel(where);
  printnodes(where,PRINTNODES);
  if (input){
    mputc(' ', where);
    input->printlabel(where);
  }else{
    mprintf(where, " %s  ", inputlabel);
  }
  printexpr(where);
  mputc('\n', where);
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
