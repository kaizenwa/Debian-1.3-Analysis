/*$Id: e_compon.h,v 11.36 96/03/24 01:24:09 al Exp $ -*- C++ -*-
 * branch structure type definitions
 * device types (enumeration type?)
 */
#include "e_model.h"
#include "io_trace.h"
#include "l_compar.h"
#ifndef E_COMPON_H
#define E_COMPON_H
/*--------------------------------------------------------------------------*/
class COMPONENT_COMMON;
class COMPONENT;
class MODEL_Base;
/*--------------------------------------------------------------------------*/
//#define conchk(o,n,a)	((fabs((n)-(o))<=(a))\
//	|| (fabs(o)>(a) && uporder(OPT::lowlim, (n)/(o), OPT::uplim)))
#define conchk(o,n,a)	(fabs((n)-(o))<=(OPT::reltol*fabs(n)+(a)))
/*--------------------------------------------------------------------------*/
class COMPONENT_COMMON {
protected:
	 COMPONENT_COMMON()
	 	{model=NULL; attachcount=1; strcpy(modelname,"-error-");}
	 COMPONENT_COMMON(const COMPONENT_COMMON& p)
	 				{*this = p; attachcount=0;}
public:
 virtual const MODEL_Base* attach_model()const;
 virtual void	tr_eval(COMPONENT*x)const {assert(model); model->tr_eval(x);}
 virtual void	ac_eval(COMPONENT*x)const {assert(model); model->ac_eval(x);}
 virtual bool	has_tr_eval()const	{return false;}
 virtual bool	has_ac_eval()const	{return false;}
  CONST COMPONENT_COMMON* attach()CONST	{++attachcount; return this;}
	 int detach()CONST		{return --attachcount;}

  const MODEL_Base * model;
  char	modelname[LABELEN+1];
protected:
 mutable int attachcount;
};
/*--------------------------------------------------------------------------*/
/* note on attachcount ...
 * The default constructor inits it to 1 for the static default versions
 *	that will never be deleted
 * The copy constructor inits it to 0 so it can be deleted.
 */
/*--------------------------------------------------------------------------*/
class COMPONENT : public CARD {
protected:
  COMPONENT()
	{common=NULL;}
  COMPONENT(CONST COMPONENT& p):CARD(p)
	{common=NULL;attach_common(p.common);}
  ~COMPONENT()
	{detach_common();}
  void	parselabel(CS&);
  int	parsenodes(CS&,int);
  void	printnodes(int,int)const;
  void	tr_eval();
  void	ac_eval();
  bool	has_tr_eval()const
	{return (trfun || hasprobes() 
		 || (common && common->has_tr_eval()));}
  bool	has_ac_eval()const
	{return (acfun || hasprobes() 
		 || (common && common->has_ac_eval()));}
  const MODEL_Base* attach_model()const
	{return common->attach_model();}
  void	attach_common(CONST COMPONENT_COMMON *);
  void	detach_common();
  bool	conv_check()const
	{return conchk(y1.f1, y0.f1, OPT::abstol)
	     && conchk(y1.f0, y0.f0, OPT::abstol);}
  bool	conv_trace()const;
public:
  void	set(const char *Label, CARD *Parent,
	    CONST COMPONENT_COMMON *Common, double Value,
	    node_t N0, node_t N1);
  void	set(const char *Label, CARD *Parent,
	    CONST COMPONENT_COMMON *Common, double Value,
	    node_t N0, node_t N1, node_t N2, node_t N3);
public:
  CONST COMPONENT_COMMON* common;
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
