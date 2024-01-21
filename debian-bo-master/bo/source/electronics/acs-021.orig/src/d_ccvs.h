/*$Id: d_ccvs.h,v 11.22 96/02/18 11:45:36 al Exp $ -*- C++ -*-
 * current controlled voltage source
 */
#include "e_ccsrc.h"
#ifndef D_CCVS_H
#define D_CCVS_H
/*--------------------------------------------------------------------------*/
class DEV_CCVS : public CCSRC_BASE {
public:
	DEV_CCVS(){devclass=TWOPORT;}
	DEV_CCVS(CONST DEV_CCVS& p):CCSRC_BASE(p){}
	CARD*	clone()CONST{return new DEV_CCVS(*this);}
	void	expand();
	bool	dotr();
	void	trload(){trload_loss();trload_active();}
	void	trunload(){trunload_active();}
	void	doac();
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
