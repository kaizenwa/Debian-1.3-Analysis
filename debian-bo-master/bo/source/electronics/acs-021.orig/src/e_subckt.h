/*$Id: e_subckt.h,v 11.22 96/02/18 11:46:20 al Exp $ -*- C++ -*-
 * base class for elements made of subcircuits
 */
#include "e_compon.h"
#ifndef E_SUBCKT_H
#define E_SUBCKT_H
/*--------------------------------------------------------------------------*/
class BASE_SUBCKT : public COMPONENT {
protected:
	BASE_SUBCKT(){}
	BASE_SUBCKT(CONST BASE_SUBCKT& p):COMPONENT(p){}
public:
	bool	dotr();
	void	trload();
	void	trunload();
	void	doac();
	double	tr_review();
 friend void	expandsubckt(CARD*,const char*);
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
