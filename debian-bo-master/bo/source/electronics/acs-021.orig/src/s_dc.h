/*$Id: s_dc.h,v 11.22 96/02/18 11:46:46 al Exp $ -*- C++ -*-
 * DC and OP analysis
 */
#include "s__.h"
#ifndef S_DC_H
#define S_DC_H
/*--------------------------------------------------------------------------*/
class CARD;
/*--------------------------------------------------------------------------*/
class DCOP : public SIM {
public:				/* s_dc_set.cc */
	void	finish();
protected:
	void	options(CS&);
private:
	void	by(CS&);
	void	decade(CS&);
	void	times(CS&);
private:			/* s_dc_swp.cc */
	void	sweep();
	void	first(int);
	bool	next(int);

protected:
  double start[DCNEST];
  double stop[DCNEST];
  double step[DCNEST];
  bool linswp[DCNEST];
  double *(sweepval[DCNEST]);	/* pointer to thing to sweep, dc command */
  CARD *(zap[DCNEST]);		/* to branch to zap, for re-expand */
private:
  bool loop;			/* flag: do it again backwards */
  bool reverse;			/* flag: sweep backwards */
  bool cont;			/* flag: continue from previous run */
  trace_t trace;		/* enum: show extended diagnostics */
};
/*--------------------------------------------------------------------------*/
class DC : public DCOP {
public:
  	void	command(CS&);
private:
	void	setup(CS&);
};
/*--------------------------------------------------------------------------*/
class OP : public DCOP {
public:
  	void	command(CS&);
private:
	void	setup(CS&);
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
