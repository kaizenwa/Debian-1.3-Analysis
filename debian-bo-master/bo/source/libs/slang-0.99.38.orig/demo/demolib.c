/* These routines are used by several of the demos. */

#include <stdio.h>
#include <signal.h>

#include <slang.h>

static void demolib_exit (int sig)
{
   SLang_reset_tty ();
   SLsmg_reset_smg ();

   if (sig)
     {
	fprintf (stderr, "Exiting on signal %d\n", sig);
	exit (1);
     }
   exit (sig);
}

#ifdef SIGTSTP
static void sigtstp_handler (int sig)
{
   demolib_exit (sig);
}
#endif

#ifdef SIGINT
static void sigint_handler (int sig)
{
   demolib_exit (sig);
}
#endif

static void init_signals (void)
{
#ifdef SIGTSTP
   SLsignal (SIGTSTP, sigtstp_handler);
#endif
#ifdef SIGINT
   SLsignal (SIGINT, sigint_handler);
#endif
}


static int demolib_init_terminal (int tty, int smg)
{
   /* Lines are all read in.  It is wise to block the occurance of display 
    * related signals while we are initializing.
    */
   
   SLsig_block_signals ();
   
   SLtt_get_terminfo ();
   
   /* SLkp_init assumes that SLtt_get_terminfo has been called. */
   if (tty && (-1 == SLkp_init ()))
     {
	SLsig_unblock_signals ();
	return -1;
     }
   
   init_signals ();
   
   if (tty) SLang_init_tty (-1, 0, 1);
#ifdef unix
   if (tty) SLtty_set_suspend_state (1);
#endif
   if (smg) SLsmg_init_smg ();

   SLsig_unblock_signals ();

   return 0;
}

