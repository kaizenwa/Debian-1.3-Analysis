/*$Id: e_elem2.cc,v 11.30 96/03/17 19:21:04 al Exp $ -*- C++ -*-
 * Base class for elements of a circuit
 */
#include "ap.h"
#include "e_elemnt.h"
#include "u_opt.h"
#include "u_status.h"
#include "io.h"
#include "s__.h"
/*--------------------------------------------------------------------------*/
//	void	ELEMENT::parse(CS&);
//	void	ELEMENT::print(int,int)const;
//	void	ELEMENT::trsetup();
/*--------------------------------------------------------------------------*/
void ELEMENT::parse(CS& cmd)
{
  parselabel(cmd);
  parsenodes(cmd,DEFAULT_NUMNODES);
  parseexpr(cmd);
}
/*--------------------------------------------------------------------------*/
void ELEMENT::print(int where, int /*detail*/)const
{
  printlabel(where);
  printnodes(where,DEFAULT_NUMNODES);
  printexpr(where);
  mputc('\n', where);
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
