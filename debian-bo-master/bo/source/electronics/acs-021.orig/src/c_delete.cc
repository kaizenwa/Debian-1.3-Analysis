/*$Id: c_delete.cc,v 11.38 96/03/24 17:59:11 al Exp $ -*- C++ -*-
 * delete and clear commands
 */
#include "ap.h"
#include "e_card.h"
#include "error.h"
#include "c_comand.h"
#include "s__.h"
#include "l_compar.h"
/*--------------------------------------------------------------------------*/
//	void	CMD::clear(CS&);
//	void	CMD::del(CS&);
static	void	bylabel(CS&);
static	void	all();
/*--------------------------------------------------------------------------*/
/* cmd_clear: clear the whole circuit, including faults, etc
 *   equivalent to unfault; unkeep; delete all; title = (blank)
 */
void CMD::clear(CS&)
{
  {CS nil("");      unfault(nil);}
  {CS nil("");      unmark(nil);}
  {CS clr("clear"); ic(clr);}
  {CS clr("clear"); nodeset(clr);}
  {CS clr("clear"); alarm(clr);}
  {CS clr("clear"); plot(clr);}
  {CS clr("clear"); print(clr);}
  {CS All("all");   del(All);}
  {CS q("'");       title(q);}
}
/*--------------------------------------------------------------------------*/
/* cmd_delete:  delete command
 */
void CMD::del(CS& cmd)
{
  SIM::uninit();
  if (cmd.pmatch("ALL")){
    all();
  }else{
    while (cmd.more())
      bylabel(cmd);
  }
}
/*--------------------------------------------------------------------------*/
/* bylabel: delete circuit element by label
 * 	all lines with matching label (with wild cards * and ?) deleted.
 *	although it looks like a loop, it matches one label
 *	syntax warning if no match
 */
static void bylabel(CS& cmd)
{
  CARD *brh;		   /* scanning branch */
  CARD *next;		   /* look ahead because delete loses the link */
  int mark = cmd.cursor(); /* where we started parsing */
  int cmax = cmd.cursor(); /* where we got after being successful */
  
  brh = CARD::first();
  for (;;){
    cmd.reset(mark);
    brh =findbranch(cmd, brh, CARD::root());
    cmax = max(cmax, cmd.cursor());
    if (!exists(brh))
      break;
    next = brh->next();
    delete brh;
    brh = next;
  }
  cmd.reset(cmax);
  if (mark == cmax){
    cmd.check(bWARNING);
    cmd.skiparg();
  }
}
/*--------------------------------------------------------------------------*/
/* all: delete all parts of a circuit, line by line
 */
static void all()
{
  CARD *brh;
  CARD *next;
  for (next=brh=CARD::first();  brh != CARD::root();  brh=next){
    next = brh->next();
    delete brh;
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
