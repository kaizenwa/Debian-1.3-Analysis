/*$Id: d_cccs.h,v 11.22 96/02/18 11:45:34 al Exp $ -*- C++ -*-
 * current controlled current source
 */
#include "e_ccsrc.h"
#ifndef D_CCCS_H
#define D_CCCS_H
/*--------------------------------------------------------------------------*/
class DEV_CCCS : public CCSRC_BASE {
public:
	DEV_CCCS(){devclass=TWOPORT;}
	DEV_CCCS(CONST DEV_CCCS& p):CCSRC_BASE(p){}
	CARD*	clone()CONST{return new DEV_CCCS(*this);}
	void	expand();
	bool	dotr();
	void	trload(){trload_active();}
	void	trunload(){trunload_active();}
	void	doac();
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
