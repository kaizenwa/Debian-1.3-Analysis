/*$Id: e_model.cc,v 11.22 96/02/18 11:43:39 al Exp $ -*- C++ -*-
 * base class for all models
 */
#include "l_lib.h"
#include "error.h"
#include "e_model.h"
#include "d_diode.h"
/*--------------------------------------------------------------------------*/
//	const MODEL_CARD* find_model(const char* name)const;
/*--------------------------------------------------------------------------*/
static MODEL_DIODE modeldummy("-error-");
MODEL_CARD* MODEL_CARD::rootmodel = &modeldummy;
/*--------------------------------------------------------------------------*/
MODEL_CARD::MODEL_CARD(const char *name)
{
  devclass = NOTDEVICE;
  strncpy(label, name, LABELEN);
  label[LABELEN] = '\0';
  stprev = rootmodel;
}
/*--------------------------------------------------------------------------*/
/* find_model: look for a model, knowing its type
 * looks only in list of same type
 * return ptr to it, throw exception if it fails.
 */
const MODEL_Base* 
MODEL_CARD::find_model(const char* name)const
{
  const MODEL_CARD* brh;
  brh = this;	
  for (;;){
    brh = (MODEL_CARD*)brh->stnext;
    if (brh == this){
      error(bERROR, "can't find model: %s\n", name);
      return NULL;			/* didn't find it */
    }else if (wmatch(brh->label, name)){
      return brh;			/* found it */
    }
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
