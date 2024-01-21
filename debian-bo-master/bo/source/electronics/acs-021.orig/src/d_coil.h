/*$Id: d_coil.h,v 11.22 96/02/18 11:45:38 al Exp $ -*- C++ -*-
 * Inductance (L) component
 */
#include "e_storag.h"
#ifndef D_COIL_H
#define D_COIL_H
/*--------------------------------------------------------------------------*/
class DEV_INDUCTANCE : public STORAGE {
public:
	DEV_INDUCTANCE(){devclass=ONEPORT;}
	DEV_INDUCTANCE(CONST DEV_INDUCTANCE& p):STORAGE(p){}
	CARD*	clone()CONST{return new DEV_INDUCTANCE(*this);}
	void	expand();
	bool	dotr();
	void	trload();
	void	trunload();
	void	doac();
	double	tr_review();
private:
	void	integrate();
};
/*--------------------------------------------------------------------------*/
class DEV_MUTUAL_L : public COMPONENT {
public:
	DEV_MUTUAL_L();
	DEV_MUTUAL_L(CONST DEV_MUTUAL_L& p);
	CARD*	clone()CONST{return new DEV_MUTUAL_L(*this);}
	void	parse(CS&);
	void	print(int where, int detail)const;
	void	expand();
private:
  char	outputlabel[LABELEN+1];
  CARD*	output;
  char	inputlabel[LABELEN+1];
  CARD*	input;
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
