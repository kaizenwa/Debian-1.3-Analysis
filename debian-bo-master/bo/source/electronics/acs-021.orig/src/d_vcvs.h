/*$Id: d_vcvs.h,v 11.22 96/02/18 11:45:58 al Exp $ -*- C++ -*-
 * voltage controled voltage source
 */
#include "e_elemnt.h"
#ifndef D_VCVS_H
#define D_VCVS_H
/*--------------------------------------------------------------------------*/
class DEV_VCVS : public ELEMENT {
public:
	DEV_VCVS(){devclass=TWOPORT;}
	DEV_VCVS(CONST DEV_VCVS& p):ELEMENT(p){}
	CARD*	clone()CONST{return new DEV_VCVS(*this);}
	void	parse(CS&);
	void	print(int,int)const;
	void	expand();
	bool	dotr();
	void	trload(){trload_loss();trload_active();}
	void	trunload(){trunload_active();}
	void	doac();
private:
  enum	{NUMNODES = 4};
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
