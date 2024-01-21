/*$Id: d_cs.h,v 11.22 96/02/18 11:45:41 al Exp $ -*- C++ -*-
 $ fixed current source
 */
#include "e_elemnt.h"
#ifndef D_CS_H
#define D_CS_H
/*--------------------------------------------------------------------------*/
class DEV_CS : public ELEMENT {
public:
	DEV_CS(){devclass=SOURCE;}
	DEV_CS(CONST DEV_CS& p):ELEMENT(p){}
	CARD*	clone()CONST{return new DEV_CS(*this);}
	void	expand();
	bool	dotr();
	void	trload(){trload_source();}
	void	trunload(){trunload_source();}
	void	doac();
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
