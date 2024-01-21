/*$Id: e_ccsrc.h,v 11.22 96/02/18 11:46:08 al Exp $ -*- C++ -*-
 $ current controlled source base
 */
#include "e_elemnt.h"
#ifndef E_CCSRC_H
#define E_CCSRC_H
/*--------------------------------------------------------------------------*/
class CCSRC_BASE : public ELEMENT {
friend class DEV_MUTUAL_L;
protected:
	CCSRC_BASE(){inputlabel[0]='\0';input=NULL;}
	CCSRC_BASE(CONST CCSRC_BASE& p):ELEMENT(p)
		{strcpy(inputlabel,p.inputlabel);input=NULL;}
public:
	void	parse(CS&);
	void	print(int where, int detail)const;

protected:
  char	   inputlabel[LABELEN+1];
  ELEMENT* input;
private:
  enum	{PRINTNODES = 2, NUMNODES = 4};
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
