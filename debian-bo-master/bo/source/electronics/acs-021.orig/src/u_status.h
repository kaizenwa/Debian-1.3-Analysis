/*$Id: u_status.h,v 11.22 96/02/18 11:46:58 al Exp $ -*- C++ -*-
 * place to store all kinds of statistics
 */
#include "l_timer.h"
#include "mode.h"
#include "s__.h"
#ifndef STATUS_H
#define STATUS_H
/*--------------------------------------------------------------------------*/
class CS;
/*--------------------------------------------------------------------------*/
class STATUS : public SIM {
public:
	void command(CS& cmd);
private:
 virtual void	setup(CS&)	{assert(0);}
 virtual void	sweep()		{assert(0);}
public:
  static TIMER get;
  static TIMER op;
  static TIMER dc;
  static TIMER tran;
  static TIMER four;
  static TIMER ac;
  static TIMER set_up;
  static TIMER order;
  static TIMER evaluate;
  static TIMER load;
  static TIMER lud;
  static TIMER back;
  static TIMER review;
  static TIMER output;
  static TIMER overhead;
  static TIMER aux1;
  static TIMER aux2;
  static TIMER aux3;
  static TIMER total;
  static int user_nodes;
  static int subckt_nodes;
  static int model_nodes;
  static int total_nodes;
  static int control[cCOUNT];
  static int iter[iCOUNT];
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
