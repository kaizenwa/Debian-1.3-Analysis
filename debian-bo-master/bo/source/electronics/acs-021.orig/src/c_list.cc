/*$Id: c_list.cc,v 11.38 96/03/24 17:59:17 al Exp $ -*- C++ -*-
 * list and save commands.
 * save is list with direction to file
 */
#include "ap.h"
#include "e_card.h"
#include "error.h"
#include "io.h"
#include "c_comand.h"
/*--------------------------------------------------------------------------*/
//	void	CMD::list(CS&);
//	void	CMD::save(CS&);
static	void	list_save(CS&);
/*--------------------------------------------------------------------------*/
extern const char head[];
/*--------------------------------------------------------------------------*/
void CMD::list(CS& cmd)
{
  IO::where = IO::mstdout;
  list_save(cmd);
}
/*--------------------------------------------------------------------------*/
void CMD::save(CS& cmd)
{
  cmd.reset();			/* back up to beginning of input line */
  outset(cmd,"","");		/* (outset will re-eat words save or list) */
  list_save(cmd);		/* its purpose is to set up where to send */
				/* the stuff. */
}
/*--------------------------------------------------------------------------*/
static void list_save(CS& cmd)
{
  CARD *brh;

  mprintf(IO::where&~IO::mstdout, "%s\n", head);

  if (cmd.end()){			/* no args: list all		    */
    for (brh = CARD::first();  brh != CARD::root();  brh = brh->next()){
      brh->print(IO::where, false);
    }
  }else{				/* some args: be selective	    */
    int arg1 = cmd.cursor();
    brh = findbranch(cmd, CARD::first(), CARD::last());
    if (!exists(brh)){
      cmd.warn(bERROR);
    }
    
    if (cmd.match('-')){		/* there is a dash:  a range	    */
      CARD *stop;
      cmd.skip();
      stop = findbranch(cmd, brh, CARD::root());
      if (!exists(stop)){
	cmd.check(bERROR);
	stop = CARD::last();
      }
      for (;;){
	brh->print(IO::where, false);
	if (brh == stop)
	  break;
	brh = brh->next();
      }
    }else{				/* no dash: a list		    */
      int next;
      do {				/* each arg			    */
	next = cmd.cursor();
	do {				/* all that match this arg	    */
	  brh->print(IO::where, true);
	  cmd.reset(arg1);
	  brh = findbranch(cmd, brh->next(), CARD::root());
	} while (exists(brh));
	cmd.reset(arg1 = next);
	brh = findbranch(cmd, CARD::first(), CARD::root());
      } while (exists(brh));
    }
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
