/*$Id: d_trln.h,v 11.22 96/02/18 11:45:55 al Exp $ -*- C++ -*-
 * structures, etc. for transmission lines
 */
#include "e_elemnt.h"
#ifndef D_TRNLIN_H
#define D_TRNLIN_H
/*--------------------------------------------------------------------------*/
enum {NUM_INIT_COND = 4};
/*--------------------------------------------------------------------------*/
class TRANSLINE_COMMON : public COMPONENT_COMMON {
public:
  TRANSLINE_COMMON();
  TRANSLINE_COMMON(const TRANSLINE_COMMON& p){*this = p; attachcount=0;}
  double	z0;		/* characteristic impedance */
  double	td;		/* delay time (not used, yet) */
  double	f;		/* specification frequency */
  double	nl;		/* length (wavelengths) at f */
  double	ic[NUM_INIT_COND];/* initial conditions: v1, i1, v2, i2	*/
  double	reson;		/* quarter wave frequency */
  int		icset;		/* flag: initial condition set */
};
/*--------------------------------------------------------------------------*/
class DEV_TRANSLINE : public ELEMENT {
public:
	DEV_TRANSLINE();
	DEV_TRANSLINE(CONST DEV_TRANSLINE& p):ELEMENT(p){}
	CARD*	clone()CONST{return new DEV_TRANSLINE(*this);}
	void	parse(CS&);
 	void	print(int,int)const;
	bool	dotr();
	void	doac();
private:
	void	setinitcond(CS&);
  enum	{NUMNODES = 4};
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
