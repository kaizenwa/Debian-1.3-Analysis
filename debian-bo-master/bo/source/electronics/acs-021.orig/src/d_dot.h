/*$Id: d_dot.h,v 11.28 96/03/03 23:08:44 al Exp $ -*- C++ -*-
 $ comment statement
 */
#include "e_card.h"
#ifndef D_DOT_H
#define D_DOT_H
/*--------------------------------------------------------------------------*/
class DEV_DOT : public CARD {
public:
	DEV_DOT(){devclass=NOTDEVICE;constant=true;}
	DEV_DOT(const DEV_DOT& p):CARD(p){constant=true;}
	CARD*	clone()CONST{return new DEV_DOT(*this);}
	void	parse(CS&);
	void	print(int,int)const;
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
