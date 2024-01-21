/* hello */

#include <stdio.h>
#include <pthread.h>
#include <signal.h>

/* This is cute, but it doesn't work. vch */
static const char *const state_names[] = {
#define __pthread_defstate(S,NAME) NAME,
#include "pthread/mit/state.def"
#undef __pthread_defstate
  0
};

void (*dump_thread_info_fn) (struct pthread *, FILE *);
static void pthread_dump_info_to_file ( FILE * );
void pthread_dump_info (void);
#ifdef SIGINFO
static void sig_handler (int );
void pthread_setup_siginfo (void)
#endif

#ifdef __STDC__
static void
dump_thread_info ( struct pthread *thread, FILE *file )
#else;
static void
dump_thread_info (thread, file)
     struct pthread *thread;
     FILE *file;
#endif
{
  /* machdep */
  /* attr */
  /* signals */
  /* wakeup_time */
  /* join */
  fprintf (file, "\tthread @%*lx prio %3d %s",
	   2 * sizeof (thread), thread,
	   thread->pthread_priority,
	   state_names[(int) thread->state]);
  /* show where the signal handler gets run */
  if (thread == pthread_run)
    fprintf (file, "\t\t[ME!]");
  fprintf (file, "\n");
  if (dump_thread_info_fn)
    (*dump_thread_info_fn) (thread, file);
}

#ifdef __STDC__
static void
pthread_dump_info_to_file ( FILE *file )
#else
static void
pthread_dump_info_to_file (file)
     FILE *file;
#endif
{
  pthread_t t;
  for (t = pthread_link_list; t; t = t->pll)
    dump_thread_info (t, file);
}

#ifdef __STDC__
void
pthread_dump_info (void)
#else
void
pthread_dump_info ()
#endif
{
  if (ftrylockfile (stderr) != 0)
    return;
  fprintf (stderr, "process id %ld:\n", (long) getpid ());
  pthread_dump_info_to_file (stderr);
  funlockfile (stderr);
}

#ifdef SIGINFO
#ifdef __STDC__
static void
sig_handler ( int sig )
#else
static void
sig_handler (sig)
     int sig;
#endif	/* __STDC__ */
{
  pthread_dump_info ();
}

#ifdef __STDC__
void
pthread_setup_siginfo (void)
#else
void
pthread_setup_siginfo ()
#endif	/* __STDC__ */
{
  (void) signal (SIGINFO, sig_handler);
}
#endif		/* SIGINFO */
