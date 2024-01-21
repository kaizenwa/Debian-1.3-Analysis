/*$Id: s__out.cc,v 11.22 96/02/18 11:44:39 al Exp $ -*- C++ -*-
 * tr,dc analysis output functions (and some ac)
 */
#include "constant.h"
#include "io.h"
#include "l_lib.h"
#include "u_status.h"
#include "u_probe.h"
#include "s__.h"
#include "declare.h"	/* plottr, plopen */
/*--------------------------------------------------------------------------*/
//	void	SIM::out(double);
//	void	SIM::head(double,double,bool,const char*);
//	void	SIM::print(double);
//	void	SIM::alarm(void);
//	void	SIM::store(void);
/*--------------------------------------------------------------------------*/
/* SIM::out: output the data, "keep" for ac reference
 */
void SIM::out(double x)
{
  STATUS::output.start();
  plottr(x);
  print(x);
  alarm();
  store();
  STATUS::iter[iPRINTSTEP] = 0;
  STATUS::control[cSTEPS] = 0;
  STATUS::output.stop();
}
/*--------------------------------------------------------------------------*/
/* SIM::head: print column headings and draw plot borders
 */
void SIM::head(double start, double stop, bool linear, const char *col1)
{
  if (!plopen(mode,start,stop,linear)){
    if (col1  &&  *col1)
      mprintf(IO::where,"#%-10s", col1);
    for (int ii = 0;  ii < printlist.count();  ii++){
      mprintf(IO::where," %-10.10s", printlist[ii].label());
    }
    mprintf(IO::where,"\n");
  }
}
/*--------------------------------------------------------------------------*/
/* SIM::print: print the list of results (text form) to IO::where
 * The argument is the first column (independent variable, aka "x")
 */
void SIM::print(double x)
{
  if (!IO::ploton){
    if (x != NOT_VALID)
      mprintf(IO::where,ftos(x,"           ",5,IO::formaat));
    for (int ii = 0;  ii < printlist.count();  ii++){
      double value = printlist[ii].value();
      mprintf(IO::where,ftos(value,"           ",5,IO::formaat));
    }
    mprintf(IO::where,"\n");
  }
}
/*--------------------------------------------------------------------------*/
/* SIM::alarm: print a message when a probe is out of range
 */
void SIM::alarm(void)
{
  for (int ii = 0;  ii < alarmlist.count();  ii++){
    PROBE& prb = alarmlist[ii];
    double value = prb.value();
    if (prb.inrange()){
      mprintf(IO::where, "%s=%s\n", 
	      prb.label(), ftos(value,"           ",5,IO::formaat));
    }
  }
}
/*--------------------------------------------------------------------------*/
/* SIM::store: a stub: will be store data in preparation for post processing
 */
void SIM::store(void)
{
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
