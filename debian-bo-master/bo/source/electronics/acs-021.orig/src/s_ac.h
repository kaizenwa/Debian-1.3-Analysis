/*$Id: s_ac.h,v 11.22 96/02/18 11:46:44 al Exp $ -*- C++ -*-
 * AC analysis
 */
#include "s__.h"
#ifndef S_AC_H
#define S_AC_H
/*--------------------------------------------------------------------------*/
class AC : private SIM {
public:
	void	command(CS&);
private:
	void	sweep();
	void	first();
	bool	next();
	void	solve();
	void	clear();
	void	setup(CS&);
	void	by(CS&);
	void	decade(CS&);
	void	lin(CS&);
	void	octave(CS&);
	void	times(CS&);
private:
  double start;		// sweep start time
  double stop;		// sweep stop time
  double step;		// printed step size
  bool linswp;		// flag: use linear sweep (vs log sweep)
  bool echo;		// flag: echo the input when using input data file
  bool cold;		// flag: power off.  all DC voltages are 0
  bool cont;		// flag: continue.  don't do op first
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
