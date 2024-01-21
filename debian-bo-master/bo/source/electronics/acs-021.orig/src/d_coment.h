/*$Id: d_coment.h,v 11.28 96/03/03 23:08:41 al Exp $ -*- C++ -*-
 $ comment statement
 */
#include "e_card.h"
#ifndef D_COMENT_H
#define D_COMENT_H
/*--------------------------------------------------------------------------*/
class DEV_COMMENT : public CARD {
public:
	DEV_COMMENT(){devclass=NOTDEVICE;constant=true;}
	DEV_COMMENT(const DEV_COMMENT& p):CARD(p){constant=true;}
	CARD*	clone()CONST{return new DEV_COMMENT(*this);}
	void	parse(CS& cmd);
	void	print(int where, int detail)const;
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
