/*$Id: c_comand.cc,v 11.22 96/02/18 11:41:56 al Exp $ -*- C++ -*-
 * simple commands and stubs for the missing commands
 */
#include "ap.h"
#include "error.h"
#include "io.h"
#include "mode.h"
#include "u_opt.h"
#include "u_status.h"
#include "c_comand.h"
#include "declare.h"	/* plclear */
/*--------------------------------------------------------------------------*/
   void	  CMD::alter(CS&cmd){cmd.warn(bWARNING);}
   void	  CMD::disto(CS&cmd){cmd.warn(bWARNING);}
   void	  CMD::model(CS&cmd){cmd.warn(bWARNING);}
   void	  CMD::noise(CS&cmd){cmd.warn(bWARNING);}
   void	  CMD::sens(CS&cmd){cmd.warn(bWARNING);}
   void	  CMD::subckt(CS&cmd){cmd.warn(bWARNING);}
   void	  CMD::temp(CS&cmd){cmd.warn(bWARNING);}
   void	  CMD::tf(CS&cmd){cmd.warn(bWARNING);}
/*--------------------------------------------------------------------------*/
   void	  CMD::options(CS&cmd){static OPT o; o.command(cmd);}
   void	  CMD::status(CS&cmd){static STATUS s; s.command(cmd);}
/*--------------------------------------------------------------------------*/
//   void	  CMD::comment(CS&);
//   void	  CMD::end(CS&);
//   void	  CMD::pause(CS&);
//   void	  CMD::quit(CS&);
//   void	  CMD::title(CS&);
/*--------------------------------------------------------------------------*/
int CMD::count = 0;
extern run_mode_t run_mode;
extern char head[];
static int oldcount = 0;     /* so we can check for consecutive comments */
/*--------------------------------------------------------------------------*/
/* cmd_comment: user command
 * if there are two consecutive comments, exit graphics mode
 * (this is a kluge)
 * the syntax check exists because this is called whenever nothing else is,
 * by the command interpreter.  another kluge.
 */
void CMD::comment(CS& cmd)
{
  if (count == oldcount+1)
    plclear();
  oldcount = count;
  cmd.check(bWARNING);
}
/*--------------------------------------------------------------------------*/
void CMD::end(CS& cmd)
{
  if (run_mode == rEXECUTE){
    if (OPT::acct)
      status(cmd);
    CS nil("");
    clear(nil);
    exit(0);
  }
}
/*--------------------------------------------------------------------------*/
void CMD::pause(CS&)
{
  oldcount = count;
  mprintf(IO::mstderr, "Continue? ");
  int ch = getchar();
  if (ch=='n' || ch=='N' || ch=='C'-'@' || ch=='['-'@') {
    error(bERROR, "\r");
  }
}
/*--------------------------------------------------------------------------*/
void CMD::quit(CS&)
{
  if (run_mode == rEXECUTE){
    CS nil("");
    clear(nil);
    exit(0);
  }
}
/*--------------------------------------------------------------------------*/
void CMD::title(CS& cmd)
{
  if (cmd.more()){
    strncpy(head, cmd.tail(), BUFLEN);
    head[BUFLEN] = '\0';
  }else{
    mprintf(IO::mstdout, "%s\n", head);
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
