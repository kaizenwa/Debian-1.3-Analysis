/*$Id: c__cmd.cc,v 11.37 96/03/24 10:10:27 al Exp $ -*- C++ -*-
 * command interpreter and dispatcher
 */
#include "ap.h"
#include "error.h"
#include "io.h"
#include "u_opt.h"
#include "l_timer.h"
#include "c_comand.h"
#include "declare.h"	/* plclose */
/*--------------------------------------------------------------------------*/
//	void	CMD::cmdproc(const char*);
/*--------------------------------------------------------------------------*/
/* cmdproc: process a command
 * parse, and act on, a command string
 */
void CMD::cmdproc(const char *cs)
{
  CS cmd(cs);
  static TIMER timecheck;
  bool didsomething = true;
  
  error(bTRACE, "%s\n", cmd.fullstring());
  
  timecheck.check().reset().start();

       if (cmd.pmatch("Ac"))	   ac(cmd);
  else if (cmd.pmatch("ALArm"))    alarm(cmd);
  else if (cmd.pmatch("ALTer"))    alter(cmd);
  else if (cmd.pmatch("Build"))    build(cmd);
  else if (cmd.pmatch("CHDir"))    chdir(cmd);
  else if (cmd.pmatch("CDir"))	   chdir(cmd);
  else if (cmd.pmatch("CLEAR"))    clear(cmd);
  else if (cmd.pmatch("CRTSET"))   crtset(cmd);
  else if (cmd.pmatch("DC"))	   dc(cmd);
  else if (cmd.pmatch("DELete"))   del(cmd);
  else if (cmd.pmatch("DIsto"))    disto(cmd);
  else if (cmd.pmatch("Edit"))	   edit(cmd);
  else if (cmd.pmatch("END"))	   end(cmd);
  else if (cmd.pmatch("ENDS"))	   ends(cmd);
  else if (cmd.pmatch("EXIt"))	   quit(cmd);
  else if (cmd.pmatch("FANout"))   fanout(cmd);
  else if (cmd.pmatch("FAult"))    fault(cmd);
  else if (cmd.pmatch("FOurier"))  fourier(cmd);
  else if (cmd.pmatch("Generator"))generator(cmd);
  else if (cmd.pmatch("GET"))	   get(cmd);
  else if (cmd.pmatch("Help"))	   help(cmd);
  else if (cmd.pmatch("IC"))	   ic(cmd);
  else if (cmd.pmatch("INsert"))   insert(cmd);
  else if (cmd.pmatch("List"))	   list(cmd);
  else if (cmd.pmatch("LOg"))	   logger(cmd);
  else if (cmd.pmatch("MArk"))	   mark(cmd);
  else if (cmd.pmatch("MErge"))    merge(cmd);
  else if (cmd.pmatch("MODEl"))    model(cmd);
  else if (cmd.pmatch("Modify"))   modify(cmd);
  else if (cmd.pmatch("NODeset"))  nodeset(cmd);
  else if (cmd.pmatch("NOIse"))    noise(cmd);
  else if (cmd.pmatch("OP"))	   op(cmd);
  else if (cmd.pmatch("OPTions"))  options(cmd);
  else if (cmd.pmatch("PAuse"))    pause(cmd);
  else if (cmd.pmatch("PLot"))	   plot(cmd);
  else if (cmd.pmatch("PRint"))    print(cmd);
  else if (cmd.pmatch("PRobe"))    print(cmd);
  else if (cmd.pmatch("Quit"))	   quit(cmd);
  else if (cmd.pmatch("Restore"))  restore(cmd);
  else if (cmd.pmatch("SAve"))	   save(cmd);
  else if (cmd.pmatch("SENs"))	   sens(cmd);
  else if (cmd.pmatch("SEt"))	   options(cmd);
  else if (cmd.pmatch("SPectrum")) fourier(cmd);
  else if (cmd.pmatch("STatus"))   status(cmd);
  else if (cmd.pmatch("SUbckt"))   subckt(cmd);
  else if (cmd.pmatch("SWeep"))    sweep(cmd);
  else if (cmd.pmatch("TEmp"))	   temp(cmd);
  else if (cmd.pmatch("TF"))	   tf(cmd);
  else if (cmd.pmatch("TItle"))    title(cmd);
  else if (cmd.pmatch("TRansient"))tr(cmd);
  else if (cmd.pmatch("UNFault"))  unfault(cmd);
  else if (cmd.pmatch("UNMark"))   unmark(cmd);
  else if (cmd.pmatch("Width"))    options(cmd);
  else if (cmd.pmatch("!$$"))	   system(cmd);
  else if (cmd.pmatch("<$$"))	   run(cmd);
  else if (cmd.pmatch(">$$"))	   file(cmd);
  else{    /* comment or error */
    comment(cmd);
    didsomething = false;
  }
  if (OPT::acct  &&  didsomething){
    mprintf(IO::mstdout,"time=%8.2f\n", timecheck.check().elapsed());
  }
  plclose();
  IO::suppresserrors = false;
  outreset();
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
