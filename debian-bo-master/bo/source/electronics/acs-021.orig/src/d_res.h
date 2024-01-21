/*$Id: d_res.h,v 11.22 96/02/18 11:45:50 al Exp $ -*- C++ -*-
 * Resistance (R) component
 */
#include "e_elemnt.h"
#ifndef D_RES_H
#define D_RES_H
/*--------------------------------------------------------------------------*/
class DEV_RESISTANCE : public ELEMENT {
public:
	DEV_RESISTANCE(){devclass=ONEPORT;}
	DEV_RESISTANCE(CONST DEV_RESISTANCE& p):ELEMENT(p){}
	CARD*	clone()CONST{return new DEV_RESISTANCE(*this);}
	void	expand();
	bool	dotr();
	void	trload(){trload_passive();}
	void	trunload(){trunload_passive();}
	void	doac();
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
