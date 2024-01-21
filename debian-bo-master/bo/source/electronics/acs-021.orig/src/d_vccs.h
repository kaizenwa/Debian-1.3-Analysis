/*$Id: d_vccs.h,v 11.22 96/02/18 11:45:57 al Exp $ -*- C++ -*-
 * Admittance (Y) component
 */
#include "e_elemnt.h"
#ifndef D_VCCS_H
#define D_VCCS_H
/*--------------------------------------------------------------------------*/
class DEV_VCCS : public ELEMENT {
public:
	DEV_VCCS(){devclass=TWOPORT;}
	DEV_VCCS(CONST DEV_VCCS& p):ELEMENT(p){}
	CARD*	clone()CONST{return new DEV_VCCS(*this);}
	void	parse(CS&);
	void	print(int,int)const;
	void	expand();
	bool	dotr();
	void	trload(){trload_active();}
	void	trunload(){trunload_active();}
	void	doac();
private:
  enum	{NUMNODES = 4};
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
