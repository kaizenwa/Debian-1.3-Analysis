/*$Id: s_tr.h,v 11.22 96/02/18 11:46:49 al Exp $ -*- C++ -*-
 * Transient analysis
 */
#include "s__.h"
#ifndef S_TR_H
#define S_TR_H
/*--------------------------------------------------------------------------*/
class TRANSIENT : public SIM {
public:
	void	command(CS&);
private:
	void	review();
	void	setup(CS&);
protected:
	void	options(CS&);
	void	sweep();
private:
	void	first();
	bool	next();
protected:
  double tstart;	// sweep start time
  double tstop;		// sweep stop time
  double tstep;		// printed step size
  double dtmax;		// max internal step size (step / skip)
  double dtratio;	// ratio of max/min dt
  int skip;		// fixed step size: internal steps per external
  bool cold;		// flag: start time=0, all voltages=0
  bool cont;		// flag: continue from previous run
private:
  bool echo;		// flag: echo the input when using input data file
  int field;		// which field to use in input file
  trace_t trace;	// enum: show extended diagnostics
  double approxtime;	// guess at best time for next step
  int control;		// why this time (enum)
  bool printnow;		// flag: print this step
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
