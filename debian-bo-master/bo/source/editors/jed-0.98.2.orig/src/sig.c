/* -*- mode: C; mode: fold; -*- */
/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#if !defined (msdos) && !defined (__os2__)
/*{{{ Include Files */

#include "config.h"
#include "jed-feat.h"

#include <errno.h>
#include <slang.h>

#include "jdmacros.h"

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <signal.h>
#if !defined(VMS) || (__VMS_VER >= 70000000)
# include <sys/types.h>
#endif

#include "sig.h"
#include "file.h"
#include "sysdep.h"
#include "misc.h"
#include "screen.h"
#include "cmds.h"
#include "hooks.h"

/*}}}*/


#if !defined(VMS) || (__VMS_VER >= 70000000)
# ifdef HAS_RESIZE_PENDING
volatile int Jed_Resize_Pending;

# else
static
# endif
void jed_resize_display (int sig) /*{{{*/
{
   int r, c;
   int save_errno = errno;
   
# ifdef HAS_RESIZE_PENDING
   if (sig)
     {
	Jed_Resize_Pending = 1;
#  ifdef SIGWINCH
	SLsignal_intr (SIGWINCH, jed_resize_display);
#  endif
	errno = save_errno;
	return;
     }
   
   Jed_Resize_Pending = 0;
# endif
   
   get_term_dimensions(&c, &r);
   if (r > MAX_SCREEN_SIZE) r = MAX_SCREEN_SIZE;
   change_screen_size(c, r);

#ifndef HAS_RESIZE_PENDING
# ifdef SIGWINCH
   SLsignal_intr (SIGWINCH, jed_resize_display);
# endif
   errno = save_errno;
#endif   
}

/*}}}*/
#endif

#if !defined(VMS) || (__VMS_VER >= 70000000)
static void sig_exit_jed(int sig) /*{{{*/
{
   char buf[48];
   static int signal_in_progress = 0;
   
   if (signal_in_progress)
     return;
   
   signal_in_progress = 1;
   auto_save_all ();
   
   sprintf (buf, "Killed by signal %d.", sig);
   exit_error (buf, 0);
   exit (1);
}

/*}}}*/

/* a control-G puts us here */
static void my_interrupt(int sig) /*{{{*/
{
   sig = errno;
   
   SLKeyBoard_Quit = 1;
   if (Ignore_User_Abort == 0) SLang_Error = USER_BREAK;
   SLsignal_intr (SIGINT, my_interrupt);

   errno = sig;
}

/*}}}*/

# if defined( SIGTSTP ) || (defined( VMS) && ( __VMS_VER >= 70000000))
int Signal_Sys_Spawn_Flag = 0;
/* This should only be called from outside disturbance */
void sig_sys_spawn_cmd(int sig) /*{{{*/
{
   sig = errno;
   
   Signal_Sys_Spawn_Flag = 1;
   sys_spawn_cmd();
   update(NULL, 1, 0);		       /* force update */
   Signal_Sys_Spawn_Flag = 0;
   
   errno = sig;
}

/*}}}*/

# endif
#endif /* NOT VMS */

#ifdef SIGTTIN
static void background_read (int sig) /*{{{*/
{
   sig = errno;
   if (Stdin_Is_TTY == 0) 
     {
	exit_error ("Attempt to read from background-- exiting.", 0);
     }
   sig_sys_spawn_cmd (0);
   errno = sig;
}

/*}}}*/
#endif

void init_signals (void) /*{{{*/
{
#if !defined(VMS) || (__VMS_VER >= 70000000)
   
#ifdef SIGWINCH
   if (X_Get_Term_Size_Hook == NULL) 
     (void) SLsignal_intr(SIGWINCH, jed_resize_display);
#endif

   SLsignal_intr (SIGINT, my_interrupt);
   SLsignal (SIGHUP, sig_exit_jed);
   SLsignal (SIGQUIT, sig_exit_jed);
   SLsignal (SIGILL, sig_exit_jed);
   SLsignal (SIGTRAP, sig_exit_jed);
#if 0
   SLsignal (SIGIOT, sig_exit_jed);  /* used by abort */
#endif
#ifdef SIGPIPE
   SLsignal_intr (SIGPIPE, SIG_IGN);
#endif
   /* SIGNAL (SIGFPE, sig_exit_jed); */
#ifdef SIGBUS
   SLsignal (SIGBUS, sig_exit_jed);
#endif
   SLsignal (SIGSEGV, sig_exit_jed);
#ifdef SIGSYS
    SLsignal (SIGSYS, sig_exit_jed);
#endif
    SLsignal (SIGTERM, sig_exit_jed);
   
#ifdef SIGTSTP
   if (SIG_DFL != SLsignal_intr (SIGTSTP, sig_sys_spawn_cmd))
     Jed_Suspension_Not_Allowed = 1;
#endif

#ifdef SIGTTOU
   SLsignal_intr (SIGTTOU, background_read);
#endif
#ifdef SIGTTIN
   SLsignal_intr (SIGTTIN, background_read);
#endif
#endif /* VMS */
}

/*}}}*/

#endif				       /* Not OS2 msdos */
