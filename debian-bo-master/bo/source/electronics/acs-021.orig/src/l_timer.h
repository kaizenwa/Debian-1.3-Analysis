/*$Id: l_timer.h,v 11.22 96/02/18 11:46:32 al Exp $ -*- C++ -*-
 * CPU time accounting
 */
#ifndef U_TIMER_H
#define U_TIMER_H
#include "md_bool.h"
/*--------------------------------------------------------------------------*/
class TIMER {
private:
  enum {T_NAME_LEN = 8};
  double ref_user;	// time the clock was started
  double ref_system;
  double last_user;	// time of timed operation
  double last_system;
  double total_user;	// time since program start
  double total_system;
  bool running;
  char name[T_NAME_LEN+1];
public:
	TIMER();
	TIMER(const char*);
	TIMER&	fullreset();
	TIMER&	reset();
	TIMER&	zstart();
	TIMER&	start();
	TIMER&	stop();
	TIMER&	check();
	double	elapsed()const{return last_user+last_system;}
  	TIMER&	print();
	TIMER& 	operator=(const TIMER&);
friend	TIMER	operator-(const TIMER&,const TIMER&);
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
