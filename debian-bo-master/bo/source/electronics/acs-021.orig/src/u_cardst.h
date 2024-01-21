/*$Id: u_cardst.h,v 11.22 96/02/18 11:46:51 al Exp $ -*- C++ -*-
 * a structure to stash a "card" for fault. sweep, etc.
 */
#include "md.h"
#ifndef U_CARDST_H
#define U_CARDST_H
/*--------------------------------------------------------------------------*/
class CS;
class CARD;
class generic_t;
/*--------------------------------------------------------------------------*/
class CARDSTASH {
private:
  CARD* brh;
  double value;
  generic_t* x;
public:
  CARDSTASH()			{brh = NULL;	value = 0.;	x = NULL;}
  CARDSTASH(CARDSTASH& p)	{brh = p.brh;	value =p.value;	x = p.x;}
  void operator=(CARD*);
  void restore();
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
