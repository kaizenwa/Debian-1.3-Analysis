/*$Id: md_unix.cc,v 11.22 96/02/18 11:44:21 al Exp $ -*- C++ -*-
 * Non-portable functions for unix systems (Sun, Next, ....)
 */
#include "error.h"
#include <errno.h>
#include <signal.h>
/*--------------------------------------------------------------------------*/
	void    initialize_io(void);
	void    setup_traps(void);
static	void	new_ex_handler();
static	void	sig_int(SIGNALARGS);
static	void	sig_fpe(SIGNALARGS);
	void	shell(void);
/*--------------------------------------------------------------------------*/
void initialize_io(void)
{
  IO::stream[(int)fileno(stdin )] = stdin;
  IO::stream[(int)fileno(stdout)] = stdout;
  IO::stream[(int)fileno(stderr)] = stderr;
  IO::mstdout = IO::mstderr = 1<<fileno(stdout);
  IO::mprint = 0;
}
/*--------------------------------------------------------------------------*/
void setup_traps(void)
{
  signal(SIGFPE,sig_fpe);
  signal(SIGINT,sig_int);
  set_new_handler(new_ex_handler);
}
/*--------------------------------------------------------------------------*/
static void new_ex_handler()
{
  error(bERROR, "out of memory\n");
}
/*--------------------------------------------------------------------------*/
/* sig_int: what to do on receipt of interrupt signal (SIGINT)
 * cancel batch files, then back to command mode.
 * (actually, control-c trap)
 */
/*ARGSUSED*/
static void sig_int(SIGNALARGS)
{
  signal(SIGINT,sig_int);
  error(bERROR, "\n");
}
/*--------------------------------------------------------------------------*/
static void sig_fpe(SIGNALARGS)
{
  signal(SIGFPE,sig_fpe);
  error(bDANGER, "floating point error: %x\n", errno);
}
/*--------------------------------------------------------------------------*/
void shell(void)
{
  system(getenv("SHELL"));
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
