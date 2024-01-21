/* -*- mode: C; mode: fold -*- */
/*  Copyright (c) 1995, 1996 John E. Davis (davis@space.mit.edu)
 *  All rights reserved.
 */
#include "config.h"
#include "slrnfeat.h"
/*{{{ Include files */

#include <stdio.h>
#include <signal.h>
#include <string.h>


#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef VMS
# include "vms.h"
#else
# ifndef sun
#  include <sys/ioctl.h>
# endif
# ifdef HAVE_TERMIOS_H
#  include <termios.h>
# endif
# ifdef SYSV
#  include <sys/termio.h>
#  include <sys/stream.h>
#  include <sys/ptem.h>
#  include <sys/tty.h>
# endif
# ifndef __os2__
#  include <sys/types.h>
#  include <sys/stat.h>
# endif
#endif /* !VMS */

#include <slang.h>
#include "jdmacros.h"


#include <errno.h>

#include "server.h"
#include "slrn.h"
#include "group.h"
#include "misc.h"
#include "startup.h"
#include "art.h"
#include "score.h"
#include "util.h"
#include "chmap.h"

#if SLRN_HAS_GROUPLENS
# include "grplens.h"
#endif
#if SLRN_HAS_SLANG
# include "interp.h"
#endif
#ifdef __os2__
# define INCL_VIO
# include <os2.h>
#endif


/*}}}*/

/*{{{ Global Variables */

int Slrn_TT_Initialized = 0;

/* If -1, force mouse.  If 1 the mouse will be used on in XTerm.  If 0, 
 * do not use it.
 */
int Slrn_Use_Mouse;
char *Slrn_Version = SLRN_VERSION;

int Slrn_Batch;
int Slrn_Suspension_Ok;

char *Slrn_Newsrc_File = NULL;
char *Slrn_Lib_Dir = NULL;
Slrn_Mode_Type *Slrn_Current_Mode;

#if !defined (SLRN_DEFAULT_SERVER_OBJ)
# if SLRN_HAS_NNTP_SUPPORT
#  define SLRN_DEFAULT_SERVER_OBJ SLRN_SERVER_ID_NNTP
# else
#  define SLRN_DEFAULT_SERVER_OBJ SLRN_SERVER_ID_SPOOL
# endif
#endif

#if !defined (SLRN_DEFAULT_POST_OBJ)
# if SLRN_HAS_NNTP_SUPPORT
#  define SLRN_DEFAULT_POST_OBJ SLRN_POST_ID_NNTP
# else
#  define SLRN_DEFAULT_POST_OBJ SLRN_POST_ID_INEWS
# endif
#endif


/*}}}*/
/*{{{ Static Variables */

static int Can_Suspend;
static volatile int Want_Suspension;
static volatile int Want_Window_Size_Change;
static void perform_suspend (int);

/*}}}*/
/*{{{ Static Function Declarations */

static int main_loop (void);
static int suspend_display_mode (int);
static int resume_display_mode (int, int);
static void init_suspend_signals (int);
/*}}}*/

/*{{{ Newsrc Locking Routines */

static void test_lock( char *file ) /*{{{*/
{
   int pid;
   FILE *fp;
   
   if ((fp = fopen (file, "r")) != NULL)
     {
	if (1 == fscanf (fp, "%d", &pid) )
	  {
	     if ((pid > 0)
#ifndef __os2__
		 && (0 == kill (pid, 0))
#endif
		 )
	       {
#ifdef __os2__
		  slrn_exit_error ("\
slrn: pid %d is locking the newsrc file. If you're not running another\n\
      copy of slrn, delete the file %s", 
				   pid, file);
#else
		  slrn_exit_error ("slrn: pid %d is locking the newsrc file.", pid);
#endif
	       }
	  }
	slrn_fclose (fp);
     }
}

/*}}}*/

static int make_lock( char *file ) /*{{{*/
{
   int pid;
   FILE *fp;
   
#ifdef VMS
   fp = fopen (file, "w", "fop=cif");
#else
   fp = fopen (file, "w");
#endif
   
   if (fp == NULL) return -1;
   
   pid = getpid ();
   fprintf (fp, "%d", pid);
   slrn_fclose (fp);
   return 0;
}

/*}}}*/

static void lock_file (int how) /*{{{*/
{
   char file[1024];
   char name[256];
   static int not_ok_to_unlock;
#if SLRN_HAS_RNLOCK
   int rnlock = 0;
   char file_rn[1024];
#endif
   
   if (Slrn_Newsrc_File == NULL) return;
   if (not_ok_to_unlock) return;
   
   not_ok_to_unlock = 1;

#ifndef __os2__
   sprintf (name, "%s-lock", Slrn_Newsrc_File);
#else
   sprintf (name, "lk-%s", Slrn_Newsrc_File);
#endif
     
   slrn_make_home_filename (name, file);
   
#if SLRN_HAS_RNLOCK
   if (0 == strcmp(".newsrc", Slrn_Newsrc_File))
     {
	rnlock = 1;
	slrn_make_home_filename (".rnlock", file_rn);
     }
#endif

   if (how == 1)
     {
	test_lock (file);
#if SLRN_HAS_RNLOCK
	if (rnlock) test_lock (file_rn);
#endif
	if (-1 == make_lock (file))
	  {
	     slrn_exit_error ("Unable to create lock file %s.", file);
	  }
	
#if SLRN_HAS_RNLOCK
	if (rnlock && (-1 == make_lock (file_rn)))
	  {
	     slrn_delete_file (file); /* delete the "normal" lock file */
	     slrn_exit_error ("Unable to create lock file %s.", file_rn);
	  }
#endif       
     }
   else
     {
	if (-1 == slrn_delete_file (file))
	  {
	     /* slrn_exit_error ("Unable to remove lockfile %s.", file); */
	  }
#if SLRN_HAS_RNLOCK
        if (rnlock && -1 == slrn_delete_file (file_rn))
	  {
	     /* slrn_exit_error ("Unable to remove lockfile %s.", file_rn); */
	  }
#endif
     }
   not_ok_to_unlock = 0;
}

/*}}}*/

/*}}}*/

/*{{{ Signal Related Functions */
	
/*{{{ Low-Level signal-related utility functions */

static void init_like_signals (int argc, int *argv, /*{{{*/
			       void (*f0)(int), 
			       void (*f1)(int), 
			       int state)
{
#ifdef HAVE_SIGACTION
   struct sigaction sa;
#endif
   int i;

   if (state == 0)
     {
	for (i = 0; i < argc; i++)
	  SLsignal_intr (argv[i], f0);
	return;
     }

   for (i = 0; i < argc; i++)
     {
	int sig = argv[i];
	SLsignal_intr (sig, f1);

#if defined(SLRN_POSIX_SIGNALS)
	if (-1 != sigaction (sig, NULL, &sa))
	  {
	     int j;
	     for (j = 0; j < argc; j++)
	       {
		  if (j != i) sigaddset (&sa.sa_mask, argv[j]);
	       }
	     
	     (void) sigaction (sig, &sa, NULL);
	  }
#endif
     }
}

/*}}}*/



/*}}}*/

/*{{{ Suspension signals */

#ifdef REAL_UNIX_SYSTEM
#define SUSPEND_STACK_SIZE 512
static char Suspend_Stack [SUSPEND_STACK_SIZE];
static unsigned int Suspension_Stack_Depth = 0;
static int Ok_To_Suspend = 0;
#endif

static int Suspend_Sigtstp_Suspension = 0;

void slrn_push_suspension (int ok) /*{{{*/
{
#ifdef REAL_UNIX_SYSTEM
   if (Suspension_Stack_Depth < SUSPEND_STACK_SIZE)
     {
	Suspend_Stack [Suspension_Stack_Depth] = Ok_To_Suspend;
     }
   else ok = 0;
   
   Suspension_Stack_Depth++;
   
   (void) slrn_handle_interrupts ();

   Ok_To_Suspend = ok;
#endif
}
/*}}}*/
void slrn_pop_suspension (void) /*{{{*/
{
#ifdef REAL_UNIX_SYSTEM
   
   if (Suspension_Stack_Depth == 0)
     {
	slrn_error ("pop_suspension: underflow!");
	return;
     }
   
   Suspension_Stack_Depth--;
   
   if (Suspension_Stack_Depth < SUSPEND_STACK_SIZE)
     {
	Ok_To_Suspend = Suspend_Stack [Suspension_Stack_Depth];
     }
   else Ok_To_Suspend = 0;

   (void) slrn_handle_interrupts ();   
#endif
}
/*}}}*/

/* This function is called by the SIGTSTP handler.  Since it operates 
 * in an asynchronous fashion, care must be exercised to control when that
 * can happen.  This is accomplished via the push/pop_suspension functions.
 */
static void sig_suspend (int sig)
{
#ifdef REAL_UNIX_SYSTEM
   sig = errno;
   
   if (Ok_To_Suspend 
       && (0 == Suspend_Sigtstp_Suspension))
     {
	perform_suspend (1);
     }
   else Want_Suspension = 1;
   
   init_suspend_signals (1);
   errno = sig;
#endif
}

static void init_suspend_signals (int state) /*{{{*/
{
   int argv[2];
   int argc = 0;
   
   if (Can_Suspend == 0)
     return;

#ifdef SIGTSTP
   argv[argc++] = SIGTSTP;
#endif
   
#ifdef SIGTTIN
   argv[argc++] = SIGTTIN;
#endif

   init_like_signals (argc, argv, SIG_DFL, sig_suspend, state);
}

/*}}}*/

static void perform_suspend (int smg_suspend_flag) /*{{{*/
{
#if !defined(SIGSTOP) || !defined(REAL_UNIX_SYSTEM)
   slrn_error ("Not implemented.");
   Want_Suspension = 0;
#else
   
   int init;
# ifdef SLRN_POSIX_SIGNALS
   sigset_t mask;

   Want_Suspension = 0;
   if (Can_Suspend == 0)
     {
	slrn_error ("Suspension not allowed by shell.");
	return;
     }
   
   sigemptyset (&mask);
   sigaddset (&mask, SIGTSTP);
   
   /* This function resets SIGTSTP to default */
   init = suspend_display_mode (smg_suspend_flag);
   
   kill (getpid (), SIGTSTP);
   
   /* If SIGTSTP is pending, it will be delivered now.  That's ok. */
   sigprocmask (SIG_UNBLOCK, &mask, NULL);
# else
   
   Want_Suspension = 0;
   if (Can_Suspend == 0)
     {
	slrn_error ("Suspension not allowed by shell.");
	return;
     }
   
   init = suspend_display_mode (smg_suspend_flag);
   kill(getpid(),SIGSTOP);
# endif
   
   resume_display_mode (smg_suspend_flag, init);

   SLtty_set_suspend_state (1);
#endif
}

/*}}}*/

void slrn_suspend_cmd (void)
{
   perform_suspend (0);
}

static void check_for_suspension (void)
{
#ifdef SIGTSTP
   void (*f)(int);
   
   f = SLsignal (SIGTSTP, SIG_DFL);
   (void) SLsignal (SIGTSTP, f);
   
   Can_Suspend = (f == SIG_DFL);
     
#else
   Can_Suspend = 0;
#endif
}

/*}}}*/

/*{{{ Hangup Signals */

static void slrn_hangup (int sig) /*{{{*/
{
   slrn_init_hangup_signals (0);
   
   if ((Slrn_Current_Mode != NULL)
       && (Slrn_Current_Mode->hangup_fun != NULL))
     (*Slrn_Current_Mode->hangup_fun) (sig);
   else
     slrn_write_newsrc ();
   
   slrn_quit (sig);
}

/*}}}*/

void slrn_init_hangup_signals (int state) /*{{{*/
{
   int argv[2];
   int argc = 0;

#ifdef SIGHUP
   argv[argc++] = SIGHUP;
#endif
#ifdef SIGTERM
   argv[argc++] = SIGTERM;
#endif
   
   init_like_signals (argc, argv, SIG_IGN, slrn_hangup, state);
}

/*}}}*/

/*}}}*/

#ifdef SIGWINCH
static void sig_winch_handler (int sig)
{
   if (Slrn_Batch) return;
   sig = errno;
   Want_Window_Size_Change = 1;
   SLsignal_intr (SIGWINCH, sig_winch_handler);
   errno = sig;
}
#endif

static void slrn_set_screen_size (int sig) /*{{{*/
{
   int r = 0, c = 0;
   int old_r, old_c;
#ifdef VMS
   int status, code;
   unsigned short chan;
   $DESCRIPTOR(dev_dsc, "SYS$INPUT:");
#endif
#ifdef __os2__
   VIOMODEINFO vioModeInfo;
#endif
   
#ifdef TIOCGWINSZ
   struct winsize wind_struct;

   if (Slrn_Batch)
     return;

   do
     {
	if ((ioctl(1,TIOCGWINSZ,&wind_struct) == 0)
	    || (ioctl(0, TIOCGWINSZ, &wind_struct) == 0)
	    || (ioctl(2, TIOCGWINSZ, &wind_struct) == 0))
	  {
	     c = (int) wind_struct.ws_col;
	     r = (int) wind_struct.ws_row;
	     break;
	  }
     }
   while (errno == EINTR);
   
#else
# ifdef VMS
   status = sys$assign(&dev_dsc,&chan,0,0,0);
   if (status & 1)
     {
	code = DVI$_DEVBUFSIZ;
	status = lib$getdvi(&code, &chan,0, &c, 0,0);
	if (!(status & 1))
	  c = 80;
	code = DVI$_TT_PAGE;
	status = lib$getdvi(&code, &chan,0, &r, 0,0);
	if (!(status & 1))
	  r = 24;
	sys$dassgn(chan);
     }
# else
#  ifdef __os2__
   vioModeInfo.cb = sizeof(vioModeInfo);
   VioGetMode (&vioModeInfo, 0);
   c = vioModeInfo.col;
   r = vioModeInfo.row;   
#  endif
# endif
#endif
   
   if (r <= 0)
     {
	char *s = getenv ("LINES");
	if (s != NULL) r = atoi (s);
     }
   
   if (c <= 0)
     {
	char *s = getenv ("COLUMNS");
	if (s != NULL) c = atoi (s);
     }
   
   if ((r <= 0) || (r > 200)) r = 24;
   if ((c <= 0) || (c > 250)) c = 80;
   
   old_r = SLtt_Screen_Rows;
   old_c = SLtt_Screen_Cols;
   
   SLtt_Screen_Rows = r;
   SLtt_Screen_Cols = c;
   
   if (SLtt_Screen_Rows > 3)
     Slrn_Group_Window.nrows = SLtt_Screen_Rows - 3;
   else
     Slrn_Group_Window.nrows = 1;

   Want_Window_Size_Change = 0;
   
   if ((Slrn_Current_Mode != NULL)
       && (Slrn_Current_Mode->sigwinch_fun != NULL))
     {
	(*Slrn_Current_Mode->sigwinch_fun) (old_r, old_c);
#if SLRN_HAS_SLANG
	if (SLang_Error == 0)
	  SLang_run_hooks ("resize_screen_hook", NULL, NULL);
#endif
     }
   
   if (sig)
     {
	SLsmg_reset_smg ();
	SLsmg_init_smg ();
	
	slrn_redraw ();
     }
}

/*}}}*/

static void init_display_signals (int mode) /*{{{*/
{
   init_suspend_signals (mode);
   
   if (mode)
     {
	SLang_set_abort_signal (NULL);
#ifdef SIGPIPE
	SLsignal (SIGPIPE, SIG_IGN);
#endif
#ifdef SIGTTOU
	/* Allow background writes */
	SLsignal (SIGTTOU, SIG_IGN);
#endif   
#ifdef SIGWINCH
	SLsignal_intr (SIGWINCH, sig_winch_handler);
#endif
     }
   else
     {
#ifdef SIGWINCH
	/* SLsignal_intr (SIGWINCH, SIG_DFL); */
#endif
     }
}

/*}}}*/

int slrn_handle_interrupts (void)
{
   if (Want_Suspension)
     {
	slrn_suspend_cmd ();
     }
   
   if (Want_Window_Size_Change)
     {
	slrn_set_screen_size (1);
     }
   
   return 0;
}


/*}}}*/

/*{{{ Screen Management and Terminal Init/Reset Functions */

static int init_tty (void) /*{{{*/
{
   if (Slrn_TT_Initialized & SLRN_TTY_INIT)
     {
	return 0;
     }
   
   if (Slrn_TT_Initialized == 0)
     init_display_signals (1);

   SLang_init_tty (7, 1, 0);

#ifdef REAL_UNIX_SYSTEM
   SLang_getkey_intr_hook = slrn_handle_interrupts;
#endif
   
   Slrn_TT_Initialized |= SLRN_TTY_INIT;
   
   return 0;
}

/*}}}*/

static int reset_tty (void) /*{{{*/
{
   if (0 == (Slrn_TT_Initialized & SLRN_TTY_INIT))
     {
	return 0;
     }
   
   SLang_reset_tty ();
   Slrn_TT_Initialized &= ~SLRN_TTY_INIT;
   
   if (Slrn_TT_Initialized == 0)
     init_display_signals (0);
   
   return 0;
}

/*}}}*/

static int init_smg (int use_resume) /*{{{*/
{
   if (Slrn_TT_Initialized & SLRN_SMG_INIT)
     return 0;

   slrn_enable_mouse (1);

   if (Slrn_TT_Initialized == 0)
     init_display_signals (1);
      
   if (use_resume)
     {
	SLsmg_resume_smg ();
	Slrn_TT_Initialized |= SLRN_SMG_INIT;
     }
   else
     {
	slrn_set_screen_size (0);
	SLsmg_init_smg ();
	Slrn_TT_Initialized |= SLRN_SMG_INIT;
	     
	/* We do not want the -> overlay cursor to affect the scroll. */
#ifndef __os2__
	SLsmg_Scroll_Hash_Border = 5;
#endif
	slrn_redraw ();
     }
      
   return 0;
}

/*}}}*/

static int reset_smg (int smg_suspend_flag) /*{{{*/
{
   if (0 == (Slrn_TT_Initialized & SLRN_SMG_INIT))
     return 0; 
   
   slrn_enable_mouse (0);
   
   if (smg_suspend_flag)
     SLsmg_suspend_smg ();
   else
     {
	SLsmg_gotorc (SLtt_Screen_Rows - 1, 0);
	slrn_smg_refresh ();
	SLsmg_reset_smg ();
     }
   
   /* SLsignal_intr (SIGWINCH, SIG_DFL); */
   
   Slrn_TT_Initialized &= ~SLRN_SMG_INIT;

   if (Slrn_TT_Initialized == 0)
     init_display_signals (0);
   
   return 0;
}

/*}}}*/

static int suspend_display_mode (int smg_suspend_flag) /*{{{*/
{
   int mode = Slrn_TT_Initialized;

   SLsig_block_signals ();
   
   reset_smg (smg_suspend_flag);
   reset_tty ();
   
   SLsig_unblock_signals ();

   return mode;
}

/*}}}*/

static int resume_display_mode (int smg_suspend_flag, int mode) /*{{{*/
{
   SLsig_block_signals ();
   
   if (mode & SLRN_TTY_INIT)
     init_tty ();
   
   if (mode & SLRN_SMG_INIT)
     init_smg (smg_suspend_flag);
   
   SLsig_unblock_signals ();
   return 0;
}

/*}}}*/

void slrn_set_display_state (int state) /*{{{*/
{
   if (Slrn_Batch) return;
   
   SLsig_block_signals ();
   
   if (state & SLRN_TTY_INIT)
     init_tty ();
   else
     reset_tty ();
   
   if (state & SLRN_SMG_INIT)
     init_smg (0);
   else
     reset_smg (0);
   
   SLsig_unblock_signals ();
}

/*}}}*/

void slrn_enable_mouse (int mode) /*{{{*/
{
#ifndef __os2__
   if (Slrn_Use_Mouse)
     {
	if (-1 == SLtt_set_mouse_mode (mode, (Slrn_Use_Mouse < 0)))
	  Slrn_Use_Mouse = 0;
     }
#endif
}

/*}}}*/

/*}}}*/

int slrn_get_new_news (int no_new_groups, int create_flag) /*{{{*/
{
   char *msg1 = "Checking for new groups ...";
   char *msg2 = "Checking news ...";
   char *msg3 = "Checking news via active file ...";
   char *msg;
   
   if (Slrn_Server_Obj->sv_initialize () != 0) return (-1);
   
   if (create_flag == 0)
     {
	if (no_new_groups == 0)
	  {
	     slrn_message_now (msg1);
	     slrn_check_new_groups (create_flag);
	  }
	
	if (Slrn_List_Active_File) msg = msg3;
	else msg = msg2;
	
	slrn_message_now (msg);
     }
   slrn_read_newsrc (create_flag);
   
   slrn_read_group_descriptions ();
   
   return 0;
}

/*}}}*/

void slrn_quit (int retcode) /*{{{*/
{
   if (Slrn_Server_Obj != NULL) 
     Slrn_Server_Obj->sv_close ();
   
   slrn_set_display_state (0);
   
   lock_file (0);
#if SLRN_HAS_GROUPLENS
   slrn_close_grouplens ();
#endif
   if (retcode) fprintf (stderr, "slrn: quiting on signal %d.\n", retcode);
   exit (retcode);
}

/*}}}*/

static void perform_cleanup (void)
{
   if (Slrn_Groups_Dirty)
     slrn_write_newsrc ();
      
   if (Slrn_Server_Obj != NULL)
     Slrn_Server_Obj->sv_close ();
   
#if SLRN_HAS_GROUPLENS
   slrn_close_grouplens ();
#endif

   lock_file (0);
}

void slrn_exit_error (char *fmt, ...) /*{{{*/
{
   va_list ap;
   static int trying_to_exit;

   if (trying_to_exit == 0)
     {
	trying_to_exit = 1;
	slrn_set_display_state (0);
	   
	if (fmt != NULL)
	  {
	     fprintf (stderr, "slrn fatal error:\n");
	     
	     va_start (ap, fmt);
	     vfprintf (stderr, fmt, ap);
	     va_end(ap);
	  }   
	perform_cleanup ();
     }

   putc ('\n', stderr);
   exit (1);
}

/*}}}*/

static void usage (char *extra) /*{{{*/
{
   fputs ("\
Usage: slrn [--inews ...] [--nntp ...] [--spool ...] OPTIONS\n\
-n              Do not check for new groups.  This usually results in\n\
                 a faster startup.\n\
-f newsrc-file  Name of the newsrc file to use.\n\
-C              Use colors.\n\
-create         Create a newsrc file by getting list of groups from server.\n\
-d              Get new text descriptions of each group from server.\n\
                 Note: This may take a LONG time to retrieve this information.\n\
                 The resulting file can be several hundred Kilobytes!\n\
-i init-file    Name of initialization file to use (default .slrnrc)\n\
-Dname          Add 'name' to list of predefined preprocessing tokens.\n\
-k              Do not process score file.\n\
-k0             Process score file but inhibit expensive scores.\n\
-a              Use active file for getting new news\n\
-m              Force XTerm mouse reporting\n\
--help          Print this usage.\n\
--version       Show version and supported features\n\
\n\
For additional info use one of forms:\n\
    slrn --inews --help\n\
    slrn --nntp --help\n\
    slrn --spool --help\n\
", 
	  stderr);

   
   if (extra != NULL)
     {
	fprintf (stderr, "\n%s\n", extra);
     }
   exit (1);
}

/*}}}*/

static char *make_slang_version (unsigned int v)
{
   unsigned int a, b, c;
   static char buf[32];
   
   a = v/10000;
   b = (v - a * 10000) / 100;
   c = v - (a * 10000) - (b * 100);

   sprintf (buf, "%u.%u.%u", a, b, c);
   return buf;
}


static void version (void) /*{{{*/
{
   char *os;
   
#ifdef VMS
   os = "VMS";
#else
# ifdef __os2__
   os = "OS/2";
# else
#  ifdef __unix__
   os = "Unix";
#  else
   os = "Unknown";
#  endif
# endif
#endif
     
#if defined(__DATE__) && defined(__TIME__)
   fprintf (stdout, "Slrn Version: %s (%s %s)\n", Slrn_Version, __DATE__, __TIME__);
#else
   fprintf (stdout, "Slrn Version: %s\n", Slrn_Version);
#endif
   fprintf (stdout, "S-Lang Library Version: %s\n", make_slang_version (SLang_Version));
   if (SLANG_VERSION != SLang_Version)
     {
	fprintf (stdout, "\t** Note: This program was compiled against version %s.\n",
		 make_slang_version (SLANG_VERSION));
     }
   fprintf (stdout, "OS Type: %s\n", os);

   fprintf (stdout, "\nslrn compiled with support for:");

#if SLRN_HAS_CHARACTER_MAP
   fputs ("\n\tcharacter set mapping", stdout);
#endif
   
#if SLRN_HAS_NNTP_SUPPORT
   fputs ("\n\tnntp", stdout);
# if SLRN_USE_SLTCP
   fputs (" (SLtcp socket code)", stdout);
# endif
# ifdef VMS
#  ifdef MULTINET
   fputs (" (Multinet)", stdout);
#  endif
#  ifdef UXC
   fputs (" (UCX)", stdout);
#  endif
#  ifdef NETLIB
   fputs (" (Netlib)", stdout);
#  endif
# endif
#endif
   
#if SLRN_HAS_SPOOL_SUPPORT
   fputs ("\n\tspool", stdout);
#endif
#if SLRN_HAS_INEWS_SUPPORT
   fputs ("\n\tinews", stdout);
#endif
#if SLRN_HAS_PULL_SUPPORT
   fputs ("\n\tslrnpull", stdout);
#endif
#if SLRN_HAS_GROUPLENS
   fputs ("\n\tGroupLens", stdout);
#endif
#if SLRN_HAS_SLANG
   fputs ("\n\tS-Lang Macros", stdout);
#endif
#if SLRN_HAS_MSGID_CACHE
   fputs ("\n\tMsg-id Cache", stdout);
#endif
   fputs ("\n\nDefault server object: ", stdout);
   switch (SLRN_DEFAULT_SERVER_OBJ)
     {
      case SLRN_SERVER_ID_NNTP:
	fputs ("nntp", stdout);
	break;

      case SLRN_SERVER_ID_SPOOL:
	fputs ("spool", stdout);
	break;
     }
   
   fputs ("\nDefault posting mechanism: ", stdout);
   switch (SLRN_DEFAULT_POST_OBJ)
     {
      case SLRN_POST_ID_NNTP:
	fputs ("nntp", stdout);
	break;

      case SLRN_POST_ID_INEWS:
	fputs ("inews", stdout);
#if SLRN_FORCE_INEWS
	fputs (" (fixed)", stdout);
#endif
	break;

      case SLRN_POST_ID_PULL:
	fputs ("slrnpull", stdout);
	break;
     }
   
   fputc ('\n', stdout);
   exit (0);
}

/*}}}*/
static int parse_object_args (char *obj, char **argv, int argc) /*{{{*/
{
   int num_parsed;
   int zero_ok = 1;
   
   if (obj == NULL)
     {
	zero_ok = 0;
#if SLRN_DEFAULT_SERVER_OBJ == SLRN_SERVER_ID_SPOOL
	obj = "spool";
#endif
#if SLRN_DEFAULT_SERVER_OBJ == SLRN_SERVER_ID_NNTP
	obj = "nntp";
#endif
     }
   
   num_parsed = slrn_parse_object_args (obj, argv, argc);
   if (num_parsed < 0)
     {
	if (num_parsed == -1)
	  slrn_exit_error ("%s is not a supported option.", *argv);
	else 
	  slrn_exit_error ("%s is not supported.", obj);
     }
   if ((num_parsed == 0) && (zero_ok == 0))
     usage (NULL);
   
   return num_parsed;
}

/*}}}*/

static void read_score_file (void)
{
   char file[SLRN_MAX_PATH_LEN];
   
   if (Slrn_Score_File == NULL)
     return;
   
   slrn_make_home_filename (Slrn_Score_File, file);
   
   if (-1 == slrn_read_score_file (file))
     {
	slrn_exit_error ("Error processing score file %s.", file);
     }
}


int main (int argc, char **argv) /*{{{*/
{
   char *hlp_file;
   int i;
   int create_flag = 0;
   int no_new_groups = 0;
   int no_score_file = 0;
   int use_color = 0;
   int use_mouse = 0;
   int dsc_flag = 0;
   int use_active = 0;
   FILE *fp;
   char file[512];
   char *init_file = SLRN_USER_SLRNRC_FILENAME;

   check_for_suspension ();
#ifdef __unix__
   (void) umask (077);
#endif
   
#ifdef __os2__
   SLdefine_for_ifdef ("OS2");
#else
# ifdef VMS
   SLdefine_for_ifdef ("VMS");
# else
   SLdefine_for_ifdef ("UNIX");
# endif
#endif

#if 0
   if (NULL != getenv ("AUTOSUBSCRIBE"))
     Slrn_Unsubscribe_New_Groups = 0;
   if (NULL != getenv ("AUTOUNSUBSCRIBE"))
     Slrn_Unsubscribe_New_Groups = 1;
#endif
   
   for (i = 1; i < argc; i++)
     {
	if (!strcmp ("--spool", argv[i])
	    || !strcmp ("--nntp", argv[i])
	    || !strcmp ("--inews", argv[i]))
	  {
	     i += parse_object_args (argv[i] + 2, argv + (i + 1), argc - (i + 1));
	  }
	else if (!strcmp ("--help", argv[i])) usage (NULL);
#if 0
	else if (!strcmp ("--batch", argv[i])) Slrn_Batch = 1;
#endif
	else if (!strcmp ("-create", argv[i])) create_flag = 1;
	else if (!strcmp ("-C", argv[i])) use_color = 1;
	else if (!strcmp ("-C-", argv[i])) use_color = -1;
	else if (!strcmp ("-a", argv[i])) use_active = 1;
	else if (!strcmp ("-n", argv[i])) no_new_groups = 1;
	else if (!strcmp ("-d", argv[i])) dsc_flag = 1;
	else if (!strcmp ("-m", argv[i])) use_mouse = 1;
	else if (!strcmp ("-k", argv[i])) no_score_file = 1;
	else if (!strcmp ("-k0", argv[i]))
	  Slrn_Perform_Scoring &= ~SLRN_EXPENSIVE_SCORING;
	else if (!strcmp ("--version", argv[i]))
	  {
	     version ();
	  }
	else if (!strncmp ("-D", argv[i], 2) && (argv[i][2] != 0))
	  {
	     if (SLdefine_for_ifdef (argv[i] + 2) == 0)
	       {
		  slrn_exit_error ("Unable to add preprocessor name %s.",
				   argv[i] + 2);
	       }
	  }
	else if (i + 1 < argc)
	  {
	     if (!strcmp ("-f", argv[i])) Slrn_Newsrc_File = argv[++i];
	     else if (!strcmp ("-i", argv[i])) init_file = argv[++i];
	     else 
	       {
		  i += parse_object_args (NULL, argv + i, argc - i);
		  i -= 1;
	       }
	  }
	else 
	  {
	     i += parse_object_args (NULL, argv + i, argc - i);
	     i -= 1;
	  }
     }
   

   if (-1 == slrn_init_objects ())
     {
	slrn_exit_error ("Error configuring server objects.");
     }


   if (Slrn_Server_Id == 0) Slrn_Server_Id = SLRN_DEFAULT_SERVER_OBJ;
   if (Slrn_Post_Id == 0) Slrn_Post_Id = SLRN_DEFAULT_POST_OBJ;

#if defined(__DATE__) && defined(__TIME__)
   fprintf (stdout, "slrn %s (%s %s)\n", Slrn_Version, __DATE__, __TIME__);
#else
   fprintf (stdout, "slrn %s\n", Slrn_Version);
#endif

   if (dsc_flag && create_flag)
     {
	usage ("The -d and -create flags must not be specified together.");
     }
   
   if (Slrn_Batch == 0)
     {
	SLtt_get_terminfo ();
	if (use_color == 1) SLtt_Use_Ansi_Colors = 1;
	else if (use_color == -1) SLtt_Use_Ansi_Colors = 0;
     }

   slrn_startup_initialize ();
   slrn_get_user_info ();
   
   if (Slrn_Lib_Dir == NULL)
     {
	Slrn_Lib_Dir = getenv ("SLRN_LIB_DIR");
     }
   if (Slrn_Lib_Dir == NULL) Slrn_Lib_Dir = SLRN_LIB_DIR;
   
#ifdef VMS
   sprintf (file, "%s%s", Slrn_Lib_Dir, "slrn.rc");
#else
   sprintf (file, "%s/%s", Slrn_Lib_Dir, "slrn.rc");
#endif

#if SLRN_HAS_SLANG
   if (-1 == slrn_init_slang ())
     {
	fprintf (stderr, "Error initializing S-Lang interpreter.\n");
     }
#endif

	
   slrn_init_hangup_signals (1);

   slrn_read_startup_file (file);      /* global file for all users */
   slrn_read_startup_file (init_file);

#if SLRN_HAS_CHARACTER_MAP
   if (-1 == slrn_set_charset (Slrn_Charset))
     slrn_exit_error ("Failed to select character set.");
#endif
   
#ifdef SIGINT
   if (Slrn_TT_Initialized == 0)
     SLsignal_intr (SIGINT, SIG_DFL);
#endif
   
   if ((-1 == slrn_select_server_object (Slrn_Server_Id))
       || (-1 == slrn_select_post_object (Slrn_Post_Id)))
     {
	slrn_exit_error ("Unable to select object.");
     }

   
#ifndef __os2__
   /* Allow blink characters if in mono */
   if (SLtt_Use_Ansi_Colors == 0)
     SLtt_Blink_Mode = 1;
#endif
   
   /* Now that we have read in the startup file, check to see if the user
    * has a username and a usable hostname.  Without those, we are not
    * starting up.
    */
   if (0 == slrn_is_fqdn (Slrn_User_Info.host))
     {
	slrn_exit_error ("\
Unable to find a fully qualified hostname.  You will have to specify a\r\n\
hostname in your %s file.\r\n", 
			 SLRN_USER_SLRNRC_FILENAME);
     }
   if ((NULL == Slrn_User_Info.username)
       || (0 == *Slrn_User_Info.username)
       || (NULL != slrn_strchr (Slrn_User_Info.username, '@')))
     {
	slrn_exit_error ("\
Unable to find your user name.  This means that a valid 'From' header line\r\n\
cannot be constructed.  Try setting the USER environment variable.\r\n");
     }
   
   if (no_score_file == 0) read_score_file ();
   
   hlp_file = getenv ("SLRNHELP");
   if (hlp_file != NULL)
     {
	slrn_parse_helpfile (hlp_file);
     }
   
   if ((Slrn_Newsrc_File == NULL)
       && ((Slrn_Newsrc_File = slrn_map_file_to_host (Slrn_Server_Obj->sv_name)) == NULL))
#if defined(VMS) || defined(__os2__)
     Slrn_Newsrc_File = "jnews.rc";
#else
   Slrn_Newsrc_File = ".jnewsrc";
#endif

   if (use_active) Slrn_List_Active_File = 1;
   if (use_mouse) Slrn_Use_Mouse = -1; /* -1 forces it. */

   if (dsc_flag)
     {
	if (Slrn_Server_Obj->sv_initialize () != 0)
	  {
	     slrn_exit_error ("Unable to initialize server.");
	  }
	slrn_get_group_descriptions ();
	Slrn_Server_Obj->sv_close ();
	return 0;
     }
   
   if (create_flag == 0)
     {
	/* Check to see if the .newrc file exists--- I should use the access
	 * system call but for now, do it this way.
	 */
	if (NULL == (fp = slrn_open_home_file (Slrn_Newsrc_File, "r", file, 0)))
	  {
	     fprintf(stderr, "Unable to open %s.  I will try .newsrc.\n", file);
	     if (NULL == (fp = slrn_open_home_file (".newsrc", "r", file, 0)))
	       {
		  slrn_exit_error ("\r\nUnable to open %s.\r\n\
If you want to create %s, add command line options:\r\n\
   -f %s -create\r\n", file, file, file);
	       }
	  }
	slrn_fclose (fp);
	
	lock_file (1);
     }

#if SLRN_HAS_SLANG
   (void) SLang_run_hooks ("startup_hook", NULL, NULL);
#endif
   
   
#if SLRN_HAS_GROUPLENS
   if (Slrn_Use_Group_Lens && (Slrn_Batch == 0))
     {
	fprintf (stdout, "Initializing GroupLens\n");
	if (-1 == slrn_init_grouplens ())
	  {
	     fprintf (stderr, "GroupLens disabled.\n");
	     Slrn_Use_Group_Lens = 0;
	  }
     }
#endif
	
   if (-1 == slrn_get_new_news (no_new_groups, create_flag))
     {
	slrn_exit_error ("Failed to initialize server.");
     }
   
   putc ('\n', stdout);
   
   slrn_set_display_state (SLRN_SMG_INIT | SLRN_TTY_INIT);

#if defined(__unix__) && !defined(__os2__)
   if (Slrn_Autobaud) SLtt_Baud_Rate = SLang_TT_Baud_Rate;
#endif

   main_loop ();
   
   slrn_quit (0);
   return 0;
}

/*}}}*/

/*{{{ Main Loop and Key Processing Functions */

void slrn_switch_to_mode (Slrn_Mode_Type *mode)
{
   Slrn_Current_Mode = mode;
   Slrn_Full_Screen_Update = 1;
   if ((mode != NULL) && (mode->enter_mode_hook != NULL))
     (*mode->enter_mode_hook) ();
}

void slrn_call_command (char *cmd) /*{{{*/
{
   SLKeymap_Function_Type *list;
   
   if ((Slrn_Current_Mode == NULL)
       || (Slrn_Current_Mode->keymap == NULL))
     list = NULL;
   else
     list = Slrn_Current_Mode->keymap->functions;
   
   while ((list != NULL) && (list->name != NULL))
     {
	if (0 == strcmp (cmd, list->name))
	  {
	     (void) (*list->f) ();
	     return;
	  }
	list++;
     }
   
   slrn_error ("call: %s not in current keymap.", cmd);
}

/*}}}*/

static int slrn_getkey (void)
{
   static char buf[32];
   static unsigned int buf_len;
   static int timeout_active;
   
   int ch;
   
   if (SLang_Key_TimeOut_Flag == 0)
     {
	timeout_active = 0;
	buf_len = 0;
     }
   else	if ((timeout_active || (0 == SLang_input_pending (10)))
	    && (buf_len + 2 < sizeof (buf)))
     {
	int r, c;
	
	buf[buf_len] = '-';
	buf[buf_len + 1] = 0;
	
	slrn_push_suspension (0);
	r = SLsmg_get_row (); c = SLsmg_get_column ();
	slrn_message (buf);
	SLsmg_gotorc (r, c);
	slrn_smg_refresh ();
	slrn_pop_suspension ();
	timeout_active = 1;
     }
   
   ch = SLang_getkey ();
   if (buf_len + 4 < sizeof (buf))
     {
	if (ch == 0) 
	  {
	     /* Need to handle NULL character. */
	     buf[buf_len++] = '^';
	     buf[buf_len] = '@';
	  }
	else if (ch == 27)
	  {
	     buf[buf_len++] = 'E';
	     buf[buf_len++] = 'S';
	     buf[buf_len++] = 'C';
	     buf[buf_len] = ' ';
	  }
	else buf[buf_len] = (char) ch;
     }
   buf_len++;
   
   return ch;
}

   
static void do_key (SLKeyMap_List_Type *map) /*{{{*/
{
   SLang_Key_Type *key;
   static SLKeyMap_List_Type *last_map;
   static SLang_Key_Type *last_key;

   
   Suspend_Sigtstp_Suspension = 1;
   key = SLang_do_key (map, slrn_getkey);
   Suspend_Sigtstp_Suspension = 0;

   if (Slrn_Message_Present || SLang_Error) 
     {
#if SLRN_HAS_SLANG
	if (SLang_Error) SLang_restart (0);
#endif
	slrn_clear_message ();
     }
   SLang_Error = SLKeyBoard_Quit = 0;
   
   if ((key == NULL) || (key->type == 0))
     {
	SLtt_beep ();
	return;
     }
   
   if (key->type == SLKEY_F_INTRINSIC)
     {
	if ((map == last_map) && (key->f.f == (FVOID_STAR) slrn_repeat_last_key))
	  key = last_key;
	
	/* set now to avoid problems with recursive call */
	last_key = key;
	last_map = map;
	
	if (key->type == SLKEY_F_INTRINSIC)
	  {
	     (((void (*)(void))(key->f.f)) ());
	     return;
	  }
     }

   /* Otherwise we have interpreted key. */
#if SLRN_HAS_SLANG
   
   last_key = key;
   last_map = map;

   Slrn_Full_Screen_Update = 1;
   
   if ((*key->f.s == '.')
       || !SLang_execute_function (key->f.s))
     SLang_load_string(key->f.s);
#endif
}

/*}}}*/

void slrn_set_prefix_argument (int rep) /*{{{*/
{
   static int repeat;
   
   repeat = rep;
   Slrn_Prefix_Arg_Ptr = &repeat;
}

/*}}}*/

void slrn_digit_arg (void) /*{{{*/
{
   char buf[20];
   unsigned char key;
   int i;
   
   i = 0;
   buf[i++] = (char) SLang_Last_Key_Char;
   
   SLang_Key_TimeOut_Flag = 1;
   
   while (1)
     {
	buf[i] = 0;
	key = (unsigned char) slrn_getkey ();
	if ((key < '0') || (key > '9')) break;
	buf[i++] = (char) key;
     }

   SLang_Key_TimeOut_Flag = 0;
   slrn_set_prefix_argument (atoi (buf));
   
   SLang_ungetkey (key);
   if ((Slrn_Current_Mode != NULL) 
       && (Slrn_Current_Mode->keymap != NULL))
     do_key (Slrn_Current_Mode->keymap);
   
   Slrn_Prefix_Arg_Ptr = NULL;
}

/*}}}*/

void slrn_repeat_last_key (void) /*{{{*/
{
   SLtt_beep ();
}

/*}}}*/

static int main_loop (void) /*{{{*/
{
   if (-1 == slrn_select_group_mode ())
     return -1;
   
#ifdef REAL_UNIX_SYSTEM
   if (Can_Suspend)
     SLtty_set_suspend_state (1);
#endif
   slrn_push_suspension (1);
   
#if SLRN_HAS_SLANG
   (void) SLang_run_hooks ("group_mode_startup_hook", NULL, NULL);
#endif

   if (Slrn_Batch) return -1;
   
   while (Slrn_Current_Mode != NULL)
     {
	if (SLKeyBoard_Quit)
	  {
	     SLKeyBoard_Quit = 0;
	     slrn_error ("Quit!");
	  }
	
	(void) slrn_handle_interrupts ();

	if (SLang_Error || !SLang_input_pending(0))
	  {
	     slrn_update_screen (1);
	  }

	do_key (Slrn_Current_Mode->keymap);
     }
   return -1;
}

/*}}}*/

/*}}}*/
