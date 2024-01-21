/*$Id: d_admit.h,v 11.22 96/02/18 11:45:29 al Exp $ -*- C++ -*-
 $ Admittance (Y) component
 */
#include "e_elemnt.h"
#ifndef D_ADMIT_H
#define D_ADMIT_H
/*--------------------------------------------------------------------------*/
class DEV_ADMITTANCE : public ELEMENT {
public:
	DEV_ADMITTANCE(){devclass=ONEPORT;}
	DEV_ADMITTANCE(CONST DEV_ADMITTANCE& p):ELEMENT(p){}
	CARD*	clone()CONST{return new DEV_ADMITTANCE(*this);}
	void	expand();
	bool	dotr();
	void	trload(){trload_passive();}
	void	trunload(){trunload_passive();}
	void	doac();
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
