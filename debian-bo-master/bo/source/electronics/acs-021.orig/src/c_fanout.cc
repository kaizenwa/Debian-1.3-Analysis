/*$Id: c_fanout.cc,v 11.22 96/02/18 11:42:03 al Exp $ -*- C++ -*-
 * Prints out a list of all node connections.
 */
#include "ap.h"
#include "e_card.h"
#include "error.h"
#include "io.h"
#include "u_status.h"
#include "c_comand.h"
/*--------------------------------------------------------------------------*/
//	void	CMD::fanout(CS&);
static	void	checklist(const CARD*,int);
/*--------------------------------------------------------------------------*/
void CMD::fanout(CS& cmd)
{
  int start = -1;
  int stop  = -1;
  IO::where |= IO::mstdout;
  for (;;){
    if (cmd.is_digit()){
      int temp = cmd.ctoi();
      if (cmd.match('-')){
	cmd.skip();
	start = temp;
      }else if (start < 0){
	start = temp;
	if (stop < 0)
	  stop = start;
      }else{
	stop = temp;
      }
    }else if (cmd.match('-')){
      cmd.skip();
      cmd.skipbl();
      if (cmd.is_digit())
	stop = cmd.ctoi();
    }else if (outset(cmd,"","")){
      ;/*nothing*/
    }else{
      cmd.check(bWARNING);
      break;
    }
  }
  
  initio(IO::where,(FILE*)NULL);
  if (start < 0) 
    start = 0;
  if (stop < 0  ||  stop > STATUS::total_nodes)
    stop = STATUS::user_nodes;
  if (start>STATUS::total_nodes)
    error(bERROR, "%u nodes\n", STATUS::total_nodes);
  
  mprintf(IO::where,"Node:  Branches\n");
  for (int ii = start;  ii <= stop;  ++ii){
    mprintf(IO::where,"%4u:",ii);
    checklist(CARD::first(),ii);
    mputc('\n',IO::where);
  }
}
/*--------------------------------------------------------------------------*/
/* checklist: print a list of all elements connecting to a node
 *	input brh is one of the elements in a ring list
 *	recursive, for subckts
 */
static void checklist(const CARD *stop, int nod)
{
  if (stop){
    const CARD * brh = stop;
    do{
      if (brh->connects_to(nod))
	mprintf(IO::where," %s",brh->printlabel());
      if (brh->issubckt())
	checklist(brh->sub(),nod);
      brh = brh->next();
    }while (brh != stop);
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
