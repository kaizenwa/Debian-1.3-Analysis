/*$Id: e_model.h,v 11.30 96/03/17 19:22:02 al Exp $ -*- C++ -*-
 * base class for all models
 */
#include "e_card.h"
#ifndef E_MODEL_H
#define E_MODEL_H
/*--------------------------------------------------------------------------*/
class MODEL_Base {
public:
 virtual void	tr_eval(COMPONENT*)const	{assert(0);}
 virtual void	ac_eval(COMPONENT*)const	{assert(0);}
};
/*--------------------------------------------------------------------------*/
class MODEL_CARD : public CARD, public MODEL_Base {
public:
	MODEL_CARD(const char *name = "");
	MODEL_CARD(const MODEL_CARD&)	{assert(0);}
	const MODEL_Base* find_model(const char* name)const;
public:
  static MODEL_CARD* rootmodel;
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
