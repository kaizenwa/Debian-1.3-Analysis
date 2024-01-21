/* Signals manager.
   This file is part of Checker.
   Copyright 1994, 1995 Tristan Gingold
		  Written January 1994 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
/* Asynchronous events such as signals are a problem for Checker since it is
   not recursive.  Imagine what could happen if a function of Checker is
   interrupted and a user handler is called.  A simple way could be to block
   signals before Checker functions and to release after, but it will be too
   slow.  I prefer to delay signals.  Perhaps, things could be easier with
   Hurd...  */
#include "checker.h"
#include "errlist.h"
#include "message.h"
#include <signal.h>

#ifndef NO_SIGNALS

#ifdef SIG_HANDLER_T
#define SIG_HANDLER_CAST(x) (SIG_HANDLER_T)(x)
#else
#define SIG_HANDLER_CAST(x) x
#endif

/* Disable the signal manager.  Useful to debug Checker.  */
extern int flag_no_signals;

/* True if a signal is being delayed.  */
int chkr_sig_catched;

/* If DELAY_SIGNAL_CONDITION is not defined, a default 'configuration' is used.
   In this case, you need only to define:
     NSIGNALS, OTHER_SIG_HANDLER_ARGS, BLOCK_SIGNAL_WHEN_RETURN,
     SET_RIGHTS_FOR_HANDLER_ARGS, SIG_JUMP_TO.  */
#ifndef DELAY_SIGNAL_CONDITION
/* Signals are only delayed if we are in Checker.  */
#define DELAY_SIGNAL_CONDITION chkr_in_checker != 0

/* Save the signal: it will be emitted later.  On linux, we add it to a
   sig-set, and we will call kill later. */
#define SAVE_THIS_SIGNAL __sigaddset ((sigset_t *) &chkr_pending_signals, nsignal), 1

/* The pending signals that must be deliver when we are not in Checker. */
#define DECL_FOR_SAVE_THIS_SIGNAL volatile sigset_t chkr_pending_signals

/* We need to define chkr_send_delayed_signal.  */
#define NEED_CHKR_SEND_DELAYED_SIGNAL
#endif

/* Contains the action of signals.  */
static struct sigaction sig_tab[NSIGNALS];

/* Declaration for SAVE_THIS_SIGNAL. */
DECL_FOR_SAVE_THIS_SIGNAL;

/* True if we are in Checker: the program is checking a memory access.
 * Set and clear by codecheck.S or by maccess.c  */
int chkr_in_checker = 0;

/* This set contains the signals that will display a warning.  */
static sigset_t sigset_warn;

/* Is this set initialized ?  */
static int sigset_warn_initialized;

/* Function to initialize this set.  */
void init_sigset_warn (void);

/* Emit a warning when a signal is received.  */
void emit_warning_for_signal (int nsignal);

/* Defined later.  Contains everything we have to know about signals.  */
struct signal_info
{
  int number;
  int flags;
  char *name;
};
/* Flags for signal_info.flags */
#define FL_TERMINATE	(1 << 0)  /* default action is to terminate.  */
#define FL_CORE		(1 << 1)  /* default action produce a core.  */
#define FL_STOP		(1 << 2)  /* default action is to stop.  */
#define FL_IGNORE	(1 << 3)  /* default action is to ignore.  */
#define FL_FORCED	(1 << 4)  /* cannot be ignore or catched: KILL, STOP.  */
#define FL_SYNCHRON	(1 << 5)  /* synchronous signal: SEGV, ABRT, PIPE, FPE.  */
#define FL_NHANDLED	(1 << 6)  /* not handled by Checker.  */
#define FL_WARNS	(1 << 7)  /* emit a warning.  */
#define FL_DEFAULT	(1 << 8)  /* emit a warning by default. */

/* The array.  */
static struct signal_info *desc_signals[NSIGNALS];

/* The really used handler.
   It calls your handler after changing the rights of the stack or delay the
   signals.  When this functions is called, all signals are blocked.  */
static void 
chkr_sig_handler (int nsignal, OTHER_SIG_HANDLER_ARGS)
{
#if 0
 chkr_printf ("chkr_sig_handler: recu `%s', in_checker %d, sp=0x%08x\n",
 	desc_signals[nsignal]->name, chkr_in_checker, (uint)&nsignal);
#endif
 
 /*  If we are *in* Checker, this signal must be delayed.  */
 if (DELAY_SIGNAL_CONDITION)
   {
     /* Avoid impossible problems :-).  */
     if (nsignal > NSIGNALS || !desc_signals[nsignal])
       {
         chkr_printf (M_BAD_REC_SIG, nsignal);
         chkr_abort ();
       }
     
     /* If a synchronous signal was received, this means Checker is buggy.
        abort.  */
     if ((desc_signals[nsignal]->flags & FL_SYNCHRON) && chkr_in_checker)
       {
         chkr_printf (M_SYN_REC_SIG, desc_signals[nsignal]->name);
         chkr_abort ();
       }
     
     /* Well, even if the signal is ignored, the warning must be delayed. */

     /* The processus has received a signal, but it must not be delivered
        now, since we are in Checker and Checker is not recursive.  So, we
        just save the signal number and block it.  It will be deliver later.
      */

     /* Save everything about this signal to be sure we will be able to send
        it later.  */
     if (SAVE_THIS_SIGNAL)
       {
         /* Block this signal.  It is useless to call sigblock because the
            signal mask contained in the saved context will be restaured at
            the exit.  We have to change the mask in the context.  Not
            portable.  */
         BLOCK_SIGNAL_WHEN_RETURN (nsignal);
       }
     
     /* Well, a signal was sent.  */
     chkr_sig_catched++;
     
     /* That's all folks !  */
     return;
   }
 
 /* We are not in Checker: do not delay the signal.  */
 
 /* Set the right of the stack for the informations given by the OS.  */
 known_stack_limit = (PTR)&nsignal;
 SET_RIGHTS_FOR_HANDLER_ARGS;

 /* Warns.  */
 emit_warning_for_signal (nsignal);
 
 /* Deliver the signal.  This function must call sigreturn.  */
 /* If the signal is ignored by the user, it is not sent!  */
 if (sig_tab[nsignal].sa_handler == SIG_IGN)
   return;
 else if (sig_tab[nsignal].sa_handler == SIG_DFL)
   {
     sigset_t set;
     struct sigaction action;
     
     /* Set the default behavior.  */
     action.sa_handler = SIG_DFL;
     chkr_sigaction (nsignal, &action, &action);
     
     /* Unblock it.  */
     __sigfillset (&set);
     __sigdelset (&set, nsignal);
     sigprocmask (SIG_SETMASK, &set, 0);
     kill (my_pid, nsignal);	/* Bye, Bye... */
     
     /* Set the previous behavior if the signal is ignored by default.  */
     chkr_sigaction (nsignal, &action, 0);
     return;
   }
 else
   {
     /* Restore sa_mask.  */
     sigprocmask (SIG_SETMASK, &(sig_tab[nsignal].sa_mask), 0);

     /* Call the handler */
     SIG_JUMP_TO (sig_tab[nsignal].sa_handler);
   }
}

#ifdef NEED_CHKR_SEND_DELAYED_SIGNAL
/* Deliver delayed signals.  Called by maccess.c.
   Note: the order is not kept.  */
void
chkr_send_delayed_signals (void)
{
 sigset_t set;
 int n;
 
 for (n = 1; n < NSIGNALS; n++)
   if (__sigismember ((sigset_t *) &chkr_pending_signals, n))
     {
       __sigdelset ((sigset_t *) &chkr_pending_signals, n);
       chkr_sig_catched--;
       __sigemptyset (&set);
       __sigaddset (&set, n);
       sigprocmask (SIG_UNBLOCK, &set, 0);
       if (kill (my_pid, n) != 0)
         chkr_printf ("Fail to send a signal to myself.\n");
     }

 return;    
}
#endif /* NEED_CHKR_SEND_DELAYED_SIGNAL */

/* Called by chkr_sig_handler.  Emit a warning when a signal is received,
   because some signals are not always welcome.  */
void
emit_warning_for_signal (int nsignal)
{
  if (desc_signals[nsignal] && desc_signals[nsignal]->flags & FL_WARNS)
   {
     chkr_report (M_I_SIG_SG_ET);
     chkr_printf (M_REC_SIGNAL, nsignal, desc_signals[nsignal]->name);
     
     /* The action of this signal.  */
     if (sig_tab[nsignal].sa_handler == SIG_IGN)
       chkr_printf (M_SIG_IGNORED);
     else if (sig_tab[nsignal].sa_handler == SIG_DFL)
       {
         chkr_printf (M_SIG_DEFAULT);
         if (desc_signals[nsignal]->flags & FL_TERMINATE)
           chkr_printf (M_SIG_TERMINATE);
         if (desc_signals[nsignal]->flags & FL_CORE)
           chkr_printf (M_SIG_CORE);
         if (desc_signals[nsignal]->flags & FL_STOP)
           chkr_printf (M_SIG_STOP);
         if (desc_signals[nsignal]->flags & FL_IGNORE)
           chkr_printf (M_SIG_IGNORE);
         chkr_printf (").\n");
         if (desc_signals[nsignal]->flags & FL_TERMINATE)
           chkr_do_end ();
       }
     else
       chkr_printf (M_SIG_CAUGHT);
   }
}

 
/* The new sigaction syscall called by stubs-syscall.c  */
int
user_sigaction (int sig, const struct sigaction *act, struct sigaction *oldact)
{
  int ret;
  struct sigaction action;
  
  /* If the manager is disabled, use the standard one.  */
  if (flag_no_signals)
    return chkr_sigaction (sig, act, oldact);

  /* These signals can't be catched or nothing is changed.  */
  if (sig == SIGSTOP || sig == SIGKILL || act == (struct sigaction*)0)
    return chkr_sigaction (sig, act, oldact);
    
  if (act->sa_handler == SIG_DFL || act->sa_handler == SIG_IGN)
    {
      /* These signals are not delivered anymore.  */
      if (desc_signals[sig] && desc_signals[sig]->flags & FL_WARNS)
        ret = chkr_sigaction (sig, 0, oldact);
      else
        ret = chkr_sigaction (sig, act, oldact);
      if (oldact)
        {
          /* Must be filled with the user value saved and not with what
             Checker has used. */
          oldact->sa_handler = sig_tab[sig].sa_handler;
          oldact->sa_mask = sig_tab[sig].sa_mask;
        }
      return ret;
    }
   
  action = *act;
  
  /* Use the handler of Checker.  */
  action.sa_handler = SIG_HANDLER_CAST (chkr_sig_handler);
  
  /* Mask all signals.  */
  __sigfillset (&action.sa_mask);
  ret = chkr_sigaction (sig, &action, oldact);
  if (oldact)
    {
      /* Must be filled with the saved value. */
      oldact->sa_handler = sig_tab[sig].sa_handler;
      oldact->sa_mask = sig_tab[sig].sa_mask;
    }
  sig_tab[sig] = *act;
  return ret;
}

/* Signals available.  This list is not ordered, duplicating signals is 
   allowed but only the last one is kept in memory.  */
static struct signal_info all_signals[] =
{
#ifdef SIGHUP
  { SIGHUP, FL_TERMINATE, "HUP"},
#endif
#ifdef SIGINT
  { SIGINT, FL_TERMINATE | FL_DEFAULT, "INT"},
#endif
#ifdef SIGQUIT
  { SIGQUIT, FL_TERMINATE | FL_CORE, "QUIT"},
#endif
#ifdef SIGILL
  { SIGILL, FL_TERMINATE | FL_CORE | FL_SYNCHRON | FL_DEFAULT, "ILL"},
#endif
#ifdef SIGTRAP
  { SIGTRAP, FL_TERMINATE | FL_CORE | FL_SYNCHRON | FL_DEFAULT, "TRAP"},
#endif
#ifdef SIGIOT
  { SIGIOT, FL_TERMINATE | FL_CORE | FL_SYNCHRON | FL_DEFAULT, "IOT"},
#endif
#ifdef SIGABRT
  { SIGABRT, FL_TERMINATE | FL_CORE | FL_SYNCHRON | FL_DEFAULT, "ABRT"},
#endif
#ifdef SIGEMT
  { SIGEMT, FL_TERMINATE | FL_CORE | FL_SYNCHRON | FL_DEFAULT, "EMT"},
#endif
#ifdef SIGFPE
  { SIGFPE, FL_TERMINATE | FL_CORE | FL_SYNCHRON | FL_DEFAULT, "FPE"},
#endif
#ifdef SIGKILL
  { SIGKILL, FL_TERMINATE | FL_FORCED | FL_NHANDLED, "KILL"},
#endif
#ifdef SIGBUS
  { SIGBUS, FL_TERMINATE | FL_CORE | FL_SYNCHRON | FL_DEFAULT, "BUS"},
#endif
#ifdef SIGSEGV
  { SIGSEGV, FL_TERMINATE | FL_CORE | FL_SYNCHRON | FL_DEFAULT, "SEGV"},
#endif
#ifdef SIGSYS
  { SIGSYS, FL_TERMINATE | FL_CORE | FL_SYNCHRON | FL_DEFAULT, "SYS"},
#endif
#ifdef SIGPIPE
  { SIGPIPE, FL_TERMINATE | FL_SYNCHRON, "PIPE"},
#endif
#ifdef SIGALRM
  { SIGALRM, FL_TERMINATE, "ALRM"},
#endif
#ifdef SIGTERM
  { SIGTERM, FL_TERMINATE, "TERM"},
#endif
#ifdef SIGUSR1
  { SIGUSR1, FL_TERMINATE, "USR1"},
#endif
#ifdef SIGSTKFLT
  { SIGSTKFLT, FL_TERMINATE | FL_CORE | FL_SYNCHRON | FL_DEFAULT, "STKFLT"},
#endif
#ifdef SIGUSR2
  { SIGUSR2, FL_TERMINATE, "USR2"},
#endif
#ifdef SIGCLD
  { SIGCLD, FL_IGNORE | FL_NHANDLED, "CLD"},
#endif
#ifdef SIGCHLD
  { SIGCHLD, FL_IGNORE | FL_NHANDLED, "CHLD"},
#endif
#ifdef SIGPWR
  { SIGPWR, FL_IGNORE, "PWR"},
#endif
#ifdef SIGVTARLM
  { SIGVTARLM, FL_TERMINATE, "VTALRM"},
#endif
#ifdef SIGPROF
  { SIGPROF, FL_TERMINATE, "PROF"},
#endif
#ifdef SIGIO
  { SIGIO, FL_IGNORE | FL_DEFAULT, "IO"},
#endif
#ifdef SIGPOLL
  { SIGPOLL, FL_IGNORE | FL_DEFAULT, "POLL"},
#endif
#ifdef SIGXCPU
  { SIGXCPU, FL_TERMINATE | FL_SYNCHRON, "XCPU"},
#endif
#ifdef SIGXFSZ
  { SIGXFSZ, FL_TERMINATE | FL_SYNCHRON, "XFSZ"},
#endif
#ifdef SIGWINCH
  { SIGWINCH, FL_IGNORE, "WINCH"},
#endif
#ifdef SIGSTOP
  { SIGSTOP, FL_STOP | FL_FORCED | FL_NHANDLED, "STOP"},
#endif
#ifdef SIGTSTP
  { SIGTSTP, FL_STOP, "TSTP"},
#endif
#ifdef SIGCONT
  { SIGCONT, FL_IGNORE, "CONT"},
#endif
#ifdef SIGTTIN
  { SIGTTIN, FL_STOP, "TTIN"},
#endif
#ifdef SIGTTOU
  { SIGTTOU, FL_STOP, "TTOU"},
#endif
#ifdef SIGURG
  { SIGURG, FL_IGNORE, "URG"},
#endif
#ifdef SIGLOST
  { SIGLOST, FL_TERMINATE, "LOST"},
#endif
#ifdef SIGWAITING
  { SIGWAITING, FL_IGNORE, "WAITING"},
#endif
#ifdef SIGLWP
  { SIGLWP, FL_IGNORE, "LWP" },
#endif
#ifdef SIGFREEZE
  { SIGFREEZE, FL_IGNORE, "FREEZE" },
#endif
#ifdef SIGTHAW
  { SIGTHAW, FL_IGNORE, "THAW" },
#endif
  { 0, 0, 0}
};

/* These signals will emit a warning by default.  */
void
init_sigset_warn (void)
{
  struct signal_info *si;
  
  __sigemptyset (&sigset_warn);
  
  for (si = all_signals; si->number; si++)
    if (si->flags & FL_DEFAULT)
      __sigaddset (&sigset_warn, si->number);

  sigset_warn_initialized = 1;
}

/* This function is called by parse-args.c to parse these option:
 *  --sig-warn=sig	(flag = 1)
 *  --sig-no-warn=sig	(flag = 0)
 */
void
parse_signal_warn (int flag, char *name)
{
 int n,i;
 struct signal_info *si;
 
 if (!sigset_warn_initialized)
   init_sigset_warn ();
 
 if (strcmp (name, "all") == 0)
   {
     if (flag)
       __sigfillset (&sigset_warn);
     else
       __sigemptyset (&sigset_warn);
     return;
   }
 if (name[0] == 'm' && name[1] >= '0' && name[1] <= '9')
   {
     n = atod (name + 1);
     for (i = 1; i < 32; i++)
       if (n & (1 << (i - 1)))
         {
           if (flag)
             __sigaddset (&sigset_warn, i);
           else
             __sigdelset (&sigset_warn, i);
           n &= ~(1 << (i - 1));
         }
   }
 else if (name[0] >= '0' && name[0] <= '9')
   {
     n = atod (name);
     if (n)
       if (flag)
         __sigaddset (&sigset_warn, n);
       else
         __sigdelset (&sigset_warn, n);
   }
 else
   {
     for (n = 0; name[n]; n++)
       {
         /* To upper */
         if (name[n] >= 'a' && name[n] <= 'z')
           name[n] -= 'a' - 'A';
       }
     n = 0;
     for (si = all_signals; si->number; si++)
       if (strcmp (si->name, name) == 0)
         {
           if (flag)
             __sigaddset (&sigset_warn, si->number);
           else
             __sigdelset (&sigset_warn, si->number);
           n = 1;
           break;
         }
   }
 if (!n)
   {
     chkr_report (M_I_BOC_SG_ET);
     chkr_printf (M_UNKNOWN_SIG, name);
   }
}

/* Save signal before calling main().  */
void
save_signals (void)
{
 int i;
 struct sigaction t;
 struct signal_info *si;
 
 if (flag_no_signals)
   return;
 
 for (i = 1; i < NSIGNALS; i++)
   desc_signals[i] = NULL;
 for (si = all_signals; si->number; si++)
    {
      i = si->number;
      if (desc_signals[i] && desc_signals[i]->flags != si->flags)
        chkr_printf ("Incompatibility between SIG%s and SIG%s\n", si->name, desc_signals[i]->name);
      desc_signals[i] = si;
    }
    
 if (!sigset_warn_initialized)
   init_sigset_warn ();
 
 for (i = 1; i < NSIGNALS; i++)
   {
     /* Set FL_WARNS flags */
     if (desc_signals[i] && !(desc_signals[i]->flags & FL_NHANDLED)
         && __sigismember (&sigset_warn, i))
       desc_signals[i]->flags |= FL_WARNS;
       
     chkr_sigaction (i, (struct sigaction*)0, &sig_tab[i]);
     if ((i != SIGSTOP && i != SIGKILL && sig_tab[i].sa_handler != SIG_DFL 
         	&& sig_tab[i].sa_handler != SIG_IGN)
         || (desc_signals[i] && desc_signals[i]->flags & FL_WARNS))
       {
         /* Set the Checker handler.  */
         t = sig_tab[i];
         t.sa_handler = SIG_HANDLER_CAST (chkr_sig_handler);
         
         /* All signals are blocked.  */
         __sigfillset (&t.sa_mask);
         chkr_sigaction (i, &t, (struct sigaction*)0);
       }
   }
}

/* Display everything we know about the signals.  */
void
__chkr_disp_siginfo (void)
{
  struct signal_info *si;
  int i;
  sigset_t blocked;
  sigset_t pending;
  struct sigaction action;
  static int real_actions = 0;	/* To be used with gdb */
  
  sigprocmask (SIG_BLOCK, 0, &blocked);
  sigpending (&pending);
#ifdef CHKR_SAVESTACK
  chkr_load_symtab ();
#endif
  chkr_report (M_C_MES_CK_ET);
  chkr_printf ("#  | name   | state | default action\n");
  chkr_printf ("---+--------+-------+-------------------------------\n");
  for (i = 0; i < NSIGNALS; i++)
    {
      for (si = all_signals; si->number; si++)
        {
          if (si->number != i)
            continue;
          chkr_printf ("%2d | %6s | ", si->number, si->name);
          if (__sigismember (&blocked, si->number))
            chkr_printf ("b");
          else
            chkr_printf ("-");
          if (real_actions)
            chkr_sigaction (si->number, (struct sigaction*)0, &action);
          else
            action = sig_tab[si->number];
          if (action.sa_handler == SIG_DFL)
            chkr_printf ("d");
          else if (action.sa_handler == SIG_IGN)
            chkr_printf ("i");
          else
            chkr_printf ("c");
          if (__sigismember (&pending, si->number))
            chkr_printf ("p");
          else
            chkr_printf ("-");
          if (si->flags & FL_WARNS)
            chkr_printf ("w");
          else
            chkr_printf ("-");
          chkr_printf ("  | ");
          if (si->flags & FL_TERMINATE)
            chkr_printf (M_SIG_TERMINATE);
          if (si->flags & FL_CORE)
            chkr_printf (M_SIG_CORE);
          if (si->flags & FL_STOP)
            chkr_printf (M_SIG_STOP);
          if (si->flags & FL_IGNORE)
            chkr_printf (M_SIG_IGNORE);
          if (si->flags & FL_FORCED)
            chkr_printf (M_SIG_FORCED);
          if (si->flags & FL_SYNCHRON)
            chkr_printf (M_SIG_SYNCHRON);
          chkr_printf ("\n");
#ifdef CHKR_SAVESTACK
	  if (action.sa_handler != SIG_DFL && action.sa_handler != SIG_IGN)
	    chkr_show_addr ((PTR)action.sa_handler);
#endif
        }
    }
}

#endif /* NO_SIGNALS */
