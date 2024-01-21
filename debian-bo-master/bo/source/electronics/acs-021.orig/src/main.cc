/*$Id: main.cc,v 11.28 96/03/03 23:08:04 al Exp $ -*- C++ -*-
 * top level module
 * it all starts here
 */
#include "md.h"
#include "io.h"
#include "mode.h"
#include "patchlev.h"
#include "c_comand.h"
#include "declare.h"	/* lots */
#include "l_jmpbuf.h"
/*--------------------------------------------------------------------------*/
	int	main(int,const char*[]);
static	void    sign_on(void);
static	void    read_startup_files(void);
static	void    process_cmd_line(int,const char*[]);
static	void	finish(void);
/*--------------------------------------------------------------------------*/
extern JMP_BUF env;
extern run_mode_t run_mode;
/*--------------------------------------------------------------------------*/
int main(int argc, const char *argv[])
{
  initialize_io();
  sign_on();
  if (setjmp(env.p)){		
    finish();			/* error clean up (from longjmp())	*/
  }else{
    read_startup_files();
    setup_traps();
    process_cmd_line(argc,argv);
  }  
  for (;;){
    char cmdbuf[BUFLEN];
    CMD::count++;
    if (!getcmd("-->", cmdbuf, BUFLEN))
      break;
    run_mode = rEXECUTE;
    CMD::cmdproc(cmdbuf);
  }
  return 0;
}
/*--------------------------------------------------------------------------*/
static void sign_on(void)
{
  mprintf(IO::mstdout,"ACS (Al's Circuit Simulator) 0.%u\n",PATCHLEVEL);
  mprintf(IO::mstdout,"Never trust any version less than 1.0\n");
  mprintf(IO::mstdout,"Copyright 1994, Albert Davis\n");
  mprintf(IO::mstdout,"ACS comes with ABSOLUTELY NO WARRANTY\n");
  mprintf(IO::mstdout,"This is free software, and you are welcome\n");
  mprintf(IO::mstdout,"to redistribute it under certain conditions.\n");
  mprintf(IO::mstdout,"See the file \"COPYING\" for details\n");
}
/*--------------------------------------------------------------------------*/
static void read_startup_files(void)
{
  char *name;
  if (!!(name = findfile(SYSTEMSTARTFILE, SYSTEMSTARTPATH, R_OK))){
    char cmdbuf[BUFLEN];
    sprintf(cmdbuf, "get %s", name);
    CMD::cmdproc(cmdbuf);
  }
  if (!!(name = findfile(USERSTARTFILE, USERSTARTPATH, R_OK))){
    char cmdbuf[BUFLEN];
    sprintf(cmdbuf, "get %s", name);
    CMD::cmdproc(cmdbuf);
  }
  CMD::cmdproc("clear");
}
/*--------------------------------------------------------------------------*/
static void process_cmd_line(int argc, const char *argv[])
{
  if (argc > 1){
    char cmdbuf[BUFLEN];
    sprintf(cmdbuf, "< %s", argv[1]);
    CMD::cmdproc(cmdbuf);
  }
}
/*--------------------------------------------------------------------------*/
/* finish: clean up after a command
 * deallocates space, closes plot windows, resets i/o redirection, etc.
 * This is done separately for exception handling.
 * If a command aborts, clean-up is still done, leaving a consistent state.
 */
static void finish(void)
{
  plclose();
  IO::suppresserrors = false;
  outreset();
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
