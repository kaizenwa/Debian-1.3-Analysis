/*$Id: d_vs.h,v 11.22 96/02/18 11:46:00 al Exp $ -*- C++ -*-
 * fixed voltage source
 */
#include "e_elemnt.h"
#ifndef D_VS_H
#define D_VS_H
/*--------------------------------------------------------------------------*/
class DEV_VS : public ELEMENT {
public:
	DEV_VS(){devclass=SOURCE;}
	DEV_VS(CONST DEV_VS& p):ELEMENT(p){}
	CARD*	clone()CONST{return new DEV_VS(*this);}
	void	expand();
	bool	dotr();
	void	trload(){trload_loss();trload_source();}
	void	trunload(){trunload_source();}
	void	doac();
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
