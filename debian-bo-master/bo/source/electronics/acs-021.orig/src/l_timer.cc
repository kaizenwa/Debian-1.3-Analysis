/*$Id: l_timer.cc,v 11.28 96/03/03 23:07:59 al Exp $	-*- C++ -*-
 * Time a command, or whatever
 */
/*--------------------------------------------------------------------------*/
#include <assert.h>
#include "io.h"
#include "l_timer.h"
/*--------------------------------------------------------------------------*/
//		TIMER::TIMER();
//		TIMER::TIMER(const char*);
//	TIMER&	TIMER::fullreset();
//	TIMER&	TIMER::reset();
//	TIMER&	TIMER::zstart();
//	TIMER&	TIMER::start();
//	TIMER&	TIMER::stop();
//	TIMER&	TIMER::check();
//	TIMER&	TIMER::print();
//	TIMER&	TIMER::operator=(const TIMER&);
	TIMER	operator-(const TIMER&,const TIMER&);
/*--------------------------------------------------------------------------*/
TIMER::TIMER()
{
  fullreset();
}
/*--------------------------------------------------------------------------*/
TIMER::TIMER(const char* label)
{
  strncpy(name, label, T_NAME_LEN);
  name[T_NAME_LEN] = '\0';
  fullreset();
}
/*--------------------------------------------------------------------------*/
TIMER& TIMER::fullreset()
{
  total_user   = 0.;
  total_system = 0.;
  return reset();
}
/*--------------------------------------------------------------------------*/
TIMER& TIMER::reset()
{
  last_user   = 0.;
  last_system = 0.;
  ref_user    = 0.;
  ref_system  = 0.;
  running = false;
  return *this;
}
/*--------------------------------------------------------------------------*/
TIMER& TIMER::zstart()
{
  return reset().start();
}
/*--------------------------------------------------------------------------*/
TIMER& TIMER::start()
{
  assert(!running);
  if (running){
    stop();
  }
  struct rusage x;
  getrusage(RUSAGE_SELF,&x);
  ref_user =
    (double)(x.ru_utime.tv_sec) + (double)(x.ru_utime.tv_usec)*1e-6; 
  ref_system =
    (double)(x.ru_stime.tv_sec) + (double)(x.ru_stime.tv_usec)*1e-6; 
  running = true;

  return *this;
}
/*--------------------------------------------------------------------------*/
TIMER& TIMER::stop()
{
  assert(running);
  if (running){
    struct rusage x;
    double utime, stime;
    double runtime;
    getrusage(RUSAGE_SELF,&x);
    
    utime=(double)(x.ru_utime.tv_sec)+(double)(x.ru_utime.tv_usec)*1e-6; 
    runtime = utime - ref_user;
    ref_user	=  0.;
    last_user	+= runtime;
    total_user	+= runtime;
    
    stime=(double)(x.ru_stime.tv_sec)+(double)(x.ru_stime.tv_usec)*1e-6; 
    runtime = stime - ref_system;
    ref_system   =  0.;
    last_system	 += runtime;
    total_system += runtime;
    running = false;
  }
  return *this;
}
/*--------------------------------------------------------------------------*/
TIMER& TIMER::check()
{
  if (running){
    stop();
    start();
  }
  return *this;
}
/*--------------------------------------------------------------------------*/
TIMER& TIMER::print()
{
  mprintf(IO::mstdout, "%10s %8.2f %8.2f %8.2f   %8.2f %8.2f %8.2f\n", name,
	  last_user,  last_system,  last_user  + last_system,
	  total_user, total_system, total_user + total_system);
  return *this;
}
/*--------------------------------------------------------------------------*/
TIMER& TIMER::operator=(const TIMER& x)
{
  last_user    = x.last_user;
  last_system  = x.last_system;
  ref_user     = x.ref_user;
  ref_system   = x.ref_system;
  total_user   = x.total_user;
  total_system = x.total_system;
  running      = x.running;
  // but don't copy the name
  return *this;
}
/*--------------------------------------------------------------------------*/
TIMER operator-(const TIMER& x, const TIMER& y)
{
  TIMER z("temp");
  z.last_user    = x.last_user    - y.last_user;
  z.last_system  = x.last_system  - y.last_system;
  z.ref_user     = 0.;	// when did the difference start running?
  z.ref_system   = 0.;
  z.total_user   = x.total_user   - y.total_user;
  z.total_system = x.total_system - y.total_system;
  z.running      = false;
  // but don't copy the name
  return z;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
