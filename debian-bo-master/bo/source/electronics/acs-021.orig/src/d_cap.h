/*$Id: d_cap.h,v 11.22 96/02/18 11:45:31 al Exp $ -*- C++ -*-
 $ Capacitance (c) component
 */
#include "e_storag.h"
#ifndef D_CAP_H
#define D_CAP_H
/*--------------------------------------------------------------------------*/
class DEV_CAPACITANCE : public STORAGE {
public:
	DEV_CAPACITANCE(){devclass=ONEPORT;}
	DEV_CAPACITANCE(CONST DEV_CAPACITANCE& p):STORAGE(p){}
	CARD*	clone()CONST{return new DEV_CAPACITANCE(*this);}
	void	expand();
	bool	dotr();
	void	trload(){trload_passive();}
	void	trunload(){trunload_passive();}
	void	doac();
	double	tr_review();
private:
	void	integrate();
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
