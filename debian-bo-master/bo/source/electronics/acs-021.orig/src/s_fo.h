/*$Id: s_fo.h,v 11.22 96/02/18 11:46:47 al Exp $ -*- C++ -*-
 * Fourier analysis
 */
#include "s_tr.h"
#ifndef S_FO_H
#define S_FO_H
/*--------------------------------------------------------------------------*/
class FOURIER : private TRANSIENT {
public:
	FOURIER(){fdata=NULL;}
  	void	command(CS&);
private:
  	void	setup(CS&);	/* s_fo_set.cc */
	void	fftallocate();
	void	fftunallocate();
	void	foout();	/* s_fo_out.cc */
	void	fohead(int);
	void	foprint(COMPLEX*);
	void	store();
private:
  double fstart;	/* user start frequency */
  double fstop;		/* user stop frequency */
  double fstep;		/* fft frequecncy step */
  int    timesteps;	/* number of time steps in tran analysis, incl 0 */
  COMPLEX **fdata;	/* storage to allow postprocessing */
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
