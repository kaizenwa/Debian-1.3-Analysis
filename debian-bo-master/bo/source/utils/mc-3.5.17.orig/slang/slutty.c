/* slutty.c --- Unix Low level terminal (tty) functions for S-Lang */
/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */

#include <config.h>
#include <stdio.h>
#include <signal.h>
/* sequent support thanks to Kenneth Lorber <keni@oasys.dt.navy.mil> */
/* SYSV (SYSV ISC R3.2 v3.0) provided by iain.lea@erlm.siemens.de */

#ifndef sequent
# include <stdlib.h>
#endif
#include <sys/time.h>
#ifndef sequent
# include <unistd.h>
# include <termios.h>
#endif

#ifdef SYSV
# ifndef CRAY
#   include <sys/termio.h>
#   include <sys/stream.h>
#   include <sys/ptem.h>
#   include <sys/tty.h>
# endif
#endif

#ifndef sun
#include <sys/ioctl.h>
#endif

#ifdef HAVE_SYS_SELECT_H
#    include <sys/select.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include "slang.h"
#include "_slang.h"

#define TTY_DESC 2
int SLang_TT_Read_FD = TTY_DESC;
int SLang_TT_Baud_Rate;

static int Baud_Rates[20] = 
{
   0, 50, 75, 110, 134, 150, 200, 300, 600, 1200, 1800, 2400, 4800, 
   9600, 19200, 38400, 0, 0, 0, 0
};



#ifdef sequent
typedef struct
  {
      struct tchars t;
      struct ltchars lt;
      struct sgttyb s;
  }
TTY_Termio_Type;
#else
typedef struct termios TTY_Termio_Type;
#endif

TTY_Termio_Type Old_TTY;

/* this next works on ultrix for setting termios */
#ifdef TCGETS
#define GET_TERMIOS(fd, x) ioctl(fd, TCGETS, x)
#define SET_TERMIOS(fd, x) ioctl(fd, TCSETS, x)
#else
# ifdef sequent
#  define X(x,m)  &(((TTY_Termio_Type *)(x))->m)
#  define GET_TERMIOS(fd, x)	\
        ioctl(fd, TIOCGETC, X(x,t));\
	ioctl(fd, TIOCGLTC, X(x,lt));\
	ioctl(fd, TIOCGETP, X(x,s))
#  define SET_TERMIOS(fd, x)	\
	ioctl(fd, TIOCSETC, X(x,t));\
	ioctl(fd, TIOCSLTC, X(x,lt));\
	ioctl(fd, TIOCSETP, X(x,s))
# else
#  define GET_TERMIOS(fd, x) tcgetattr(fd, x)
#  define SET_TERMIOS(fd, x) tcsetattr(fd, TCSAFLUSH, x)
/* #  define SET_TERMIOS(fd, x) tcsetattr(fd, TCSANOW, x) */
# endif
#endif

static int TTY_Inited = 0;

/* Used to be 255, but breaks with some input languages that required 255 */
#define IGNORE_THING 0

int SLang_init_tty (int abort_char, int no_flow_control, int opost)
{
   TTY_Termio_Type newtty;
   
   if (TTY_Inited) return 0;
    
   SLang_Abort_Char = abort_char;
   SLang_TT_Read_FD = fileno (stdin);
   GET_TERMIOS(SLang_TT_Read_FD, &Old_TTY);
   GET_TERMIOS(SLang_TT_Read_FD, &newtty);
#ifdef sequent
   newtty.s.sg_flags &= ~(ECHO);
   newtty.s.sg_flags &= ~(CRMOD);
   /*   if (Flow_Control == 0) newtty.s.sg_flags &= ~IXON; */
   newtty.t.t_eofc = 1;
   newtty.t.t_intrc = SLang_Abort_Char;	/* ^G */
   newtty.t.t_quitc = 255;
   newtty.lt.t_suspc = 255;   /* to ignore ^Z */
   newtty.lt.t_dsuspc = 255;    /* to ignore ^Y */
   newtty.lt.t_lnextc = 255;
   newtty.s.sg_flags |= CBREAK;		/* do I want cbreak or raw????? */
#else
   
   /* get baud rate */
   
   newtty.c_iflag &= ~(ECHO | INLCR | ICRNL);
#ifdef ISTRIP
   newtty.c_iflag &= ~ISTRIP;
#endif
   if (opost == 0) newtty.c_oflag &= ~OPOST;

   if (SLang_TT_Baud_Rate == 0)
     {
/* Note:  if this generates an compiler error, simply remove 
   the statement */
#ifdef HAVE_CFGETOSPEED
	SLang_TT_Baud_Rate = cfgetospeed (&newtty);
	
	
	SLang_TT_Baud_Rate = ((SLang_TT_Baud_Rate > 0) && (SLang_TT_Baud_Rate < 19)
			      ? Baud_Rates[SLang_TT_Baud_Rate]
			      : 0);
#endif
     }
   
   if (no_flow_control) newtty.c_iflag &= ~IXON; else newtty.c_iflag |= IXON;

   newtty.c_cc[VMIN] = 1;
   newtty.c_cc[VTIME] = 0;
   newtty.c_cc[VEOF] = 1;
   newtty.c_lflag = ISIG | NOFLSH;
   newtty.c_cc[VINTR] = SLang_Abort_Char;   /* ^G */
   newtty.c_cc[VQUIT] = IGNORE_THING;
   newtty.c_cc[VSUSP] = IGNORE_THING;   /* to ignore ^Z */
#ifdef VSWTCH
   newtty.c_cc[VSWTCH] = IGNORE_THING;   /* to ignore who knows what */
#endif
#endif /*sequent*/
   SET_TERMIOS(SLang_TT_Read_FD, &newtty);
   TTY_Inited = 1;
   return 0;
}

void SLtty_set_suspend_state (int mode)
{
   TTY_Termio_Type newtty;
   GET_TERMIOS (SLang_TT_Read_FD, &newtty);
#ifdef sequent
   if (mode == 0) newtty.lt.t_suspc = 255;
   else newtty.lt.t_suspc = Old_TTY.lt.t_suspc;
#else
   if (mode == 0) newtty.c_cc[VSUSP] = 255;
   else newtty.c_cc[VSUSP] = Old_TTY.c_cc[VSUSP];
#endif
   SET_TERMIOS (SLang_TT_Read_FD, &newtty);
   if (TTY_Inited == 0) return;   
}

void SLang_reset_tty (void)
{
   if (!TTY_Inited) return;
   SET_TERMIOS(SLang_TT_Read_FD, &Old_TTY);
   TTY_Inited = 0;
}

#ifdef __cplusplus
# define SIGNAL(a,b) signal((a), (SIG_PF)(b))
#else
# define SIGNAL signal
#endif

static void default_sigint (int sig)
{
   SLKeyBoard_Quit = 1;
   if (SLang_Ignore_User_Abort == 0) SLang_Error = USER_BREAK;
   SIGNAL (SIGINT, default_sigint);
}

void SLang_set_abort_signal (void (*hand)(int))
{
   if (hand == NULL) hand = default_sigint;
   SIGNAL (SIGINT, hand);
}

#ifndef FD_SET
#define FD_SET(fd, tthis) *(tthis) = 1 << fd
#define FD_ZERO(tthis)    *(tthis) = 0
typedef int fd_set;
#endif

static fd_set Read_FD_Set;


int SLsys_input_pending(int tsecs)
{
   struct timeval wait;
   long usecs, secs;
   int ret;

   secs = tsecs / 10;
   usecs = (tsecs % 10) * 100000;
   wait.tv_sec = secs;
   wait.tv_usec = usecs;
   
   FD_ZERO(&Read_FD_Set);
   FD_SET(SLang_TT_Read_FD, &Read_FD_Set);
   ret = select(SLang_TT_Read_FD + 1, &Read_FD_Set, NULL, NULL, &wait);
   return (ret);
}


unsigned int SLsys_getkey (void)
{
   int count = 10;		       /* number of times to retry if read fails */
   unsigned char c;
   
   while ((SLKeyBoard_Quit == 0)
	  && !SLsys_input_pending(100));
   
   while (count-- && !SLKeyBoard_Quit && (read(SLang_TT_Read_FD, (char *) &c, 1) < 0)) sleep(1);
   
   if (count <= 0)
     {
	return 0xFFFF;
     }

   /* only way for keyboard quit to be non zero is if ^G recived and sigint processed */
   if (SLKeyBoard_Quit) c = SLang_Abort_Char;
   SLKeyBoard_Quit = 0;
   return((unsigned int) c);
}
