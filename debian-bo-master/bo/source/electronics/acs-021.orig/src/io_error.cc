/*$Id: io_error.cc,v 11.22 96/02/18 11:43:52 al Exp $ -*- C++ -*-
 * Error handler.
 * Collection of functions to handle all types of errors
 * including user interrupts, system errors, overflow, etc.
 */
#include "ap.h"
#include "error.h"
#include "io.h"
#include "u_opt.h"
#include <signal.h>
#include "l_jmpbuf.h"
/*--------------------------------------------------------------------------*/
	void	error(int,const char*,...);
/*--------------------------------------------------------------------------*/
extern JMP_BUF env;
/*--------------------------------------------------------------------------*/
/* error: error message printer
 * print it, if severe enough
 * terminate command, if really bad
 */
void error(int badness, const char *fmt, ...)
{
  char buffer[BIGBUFLEN];
  va_list arg_ptr;

  if (badness >= OPT::picky) {
    va_start(arg_ptr,fmt);
    vsprintf(buffer,fmt,arg_ptr);
    va_end(arg_ptr);
    mputs(buffer,IO::mstderr);
  }
  if (badness >= bDISASTER)
    abort();
  if (badness >= bEXIT)
    exit(badness);
  if (badness >= bERROR)
    longjmp(env.p,1);
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
