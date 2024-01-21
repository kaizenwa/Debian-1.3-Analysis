/*$Id: md_msdos.cc,v 11.28 96/03/03 23:08:06 al Exp $ -*- C++ -*-
 * Non-portable functions for MSDOS
 */
#include "error.h"
#include <dos.h>
#include <process.h>
#include <signal.h>
#include <time.h>
/*--------------------------------------------------------------------------*/
	void    initialize_io(void);
	void    setup_traps(void);
static	void	new_ex_handler();
static	void	sig_int(int);
static	void	sig_fpe(int);
	void	getrusage(int,struct rusage*);
	void	shell(void);
	int	system(const char*);
	int	matherr(struct exception *e);
static	char	get_switchar(void);
/*--------------------------------------------------------------------------*/
/*extern int _fmode;*/	    /* defined in stdlib.h */
/*extern char **environ;*/  /* defined in stdlib.h */
extern char e_int[];
#ifdef __BORLANDC__
extern unsigned _stklen = 15000U;
#endif
/*--------------------------------------------------------------------------*/
void initialize_io(void)
{
  IO::stream[fileno(stdin )] = stdin;
  IO::stream[fileno(stdout)] = stdout;
  IO::stream[fileno(stderr)] = stderr;
  IO::stream[fileno(stdaux)] = stdaux;
  IO::stream[fileno(stdprn)] = stdprn;
  IO::mstdout = IO::mstderr = 1<<fileno(stdout);
  IO::mprint = 1<<fileno(stdprn);
  setbuf(stdprn, NULL);
}
/*--------------------------------------------------------------------------*/
void setup_traps(void)
{
  _fpreset();
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
static void sig_int(int /*sig*/)
{
  signal(SIGINT,sig_int);
  error(bERROR, "\n");
}
/*--------------------------------------------------------------------------*/
/*ARGSUSED*/
static void sig_fpe(int /*sig*/)
{
  int sw = _status87();
  _fpreset();
  signal(SIGFPE,sig_fpe);
  error(bDEBUG, "floating point error: %x\n", sw);
  if (!sw)
    error(bDEBUG, e_int, "CPU glitch");
  if (sw & SW_INVALID)
    error(bDANGER, e_int, "Invalid operation");
  if (sw & SW_DENORMAL)
    error(bDANGER, e_int, "Denormalized operand");
  if (sw & SW_ZERODIVIDE)
    error(bDANGER, e_int, "Zero divide");
  if (sw & SW_OVERFLOW)
    error(bDANGER, e_int, "Overflow");
  if (sw & SW_UNDERFLOW)
    error(bDEBUG, e_int, "Underflow");
  if (sw & SW_INEXACT)
    error(bDEBUG, e_int, "Precision (Inexact result)");
}
/*--------------------------------------------------------------------------*/
void getrusage(int /*who*/, struct rusage *rusage)
{
  double ticks = (double)clock();
  rusage->ru_stime.tv_sec = rusage->ru_stime.tv_usec = 0;
  rusage->ru_utime.tv_usec =
    (long)fmod(ticks, (double)CLK_TCK) * (1000000./(double)CLK_TCK);
  rusage->ru_utime.tv_sec = (long)(ticks/(double)CLK_TCK);
}
/*--------------------------------------------------------------------------*/
void shell(void)
{
  char *shell;
  int errcod;
  
  if (!(shell=getenv("COMSPEC")))
    error(bERROR, e_int, "comspec");
  
  errcod = spawnle(P_WAIT, shell, shell, NULL, environ);
  _fpreset();
  if (errcod == EOF) {
    switch (errno) {
      case E2BIG:   error(bERROR, e_int, "arg list");
      case EINVAL:  error(bERROR, e_int, "mode flag");
      case ENOENT:  error(bERROR, "no shell");
      case ENOEXEC: error(bERROR, "bad shell");
      case ENOMEM:  error(bERROR, "out of memory");
      default:	    error(bERROR, e_int, "system");
    }
  }
}
/*--------------------------------------------------------------------------*/
int system(const char *string)
{
  char *shell;
  char args[200];
  int errcod;
  
  if (!(shell=getenv("COMSPEC")))
    error(bERROR, e_int, "comspec");
  
  sprintf(args,"%cc %s",get_switchar(),string);
  
  errcod = spawnle(P_WAIT, shell, shell, args, NULL, environ);
  _fpreset();
  if (errcod == EOF) {
    switch (errno) {
      case E2BIG:   error(bERROR, e_int, "arg list");
      case EINVAL:  error(bERROR, e_int, "mode flag");
      case ENOENT:  error(bERROR, "no shell");
      case ENOEXEC: error(bERROR, "bad shell");
      case ENOMEM:  error(bERROR, "out of memory");
      default:	    error(bERROR, e_int, "system");
    }
  }
  return 0;
}
/*--------------------------------------------------------------------------*/
int matherr(struct exception *e)
{
  int badness = bDANGER;
  int fixed = false;
  char *whyS [] = {
    "bogus floating point error",
    "argument domain error",
    "argument singularity ",
    "overflow range error ",
    "underflow range error",
    "total loss of significance",
    "partial loss of significance",
    "floating point stack overflow"
    };
  if (e->type == UNDERFLOW){	/* flush underflow to 0 */
    badness = bDEBUG;
    fixed = true;
    e->retval = 0;
  }else if (e->type == TLOSS){	/* total loss of precision, */
    badness = bDEBUG;		/* but ignore the problem */
    fixed = true;
  }
  error(badness, e_int, whyS[e->type]);
  error(badness, "%s %g %g %g\n", e->name, e->arg1, e->arg2, e->retval);
  return fixed;
}
/*--------------------------------------------------------------------------*/
static char get_switchar(void)
{
  union REGS r;
  
  r.h.al = 0;
  r.h.ah = 0x37;
  intdos(&r, &r);
  return r.h.dl;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
