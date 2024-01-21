/*
 * GPC Version of the ZAP game written at HUT.
 *
 * All external ZAP specific routines are in this file.
 *
 * In addition, it uses the SCREEN module, but that is
 * not ZAP specific...
 *
 * ZAP is a screen-oriented game we wrote in Pascal.
 *
 * It has been slightly influenced by Rogue and Hack...
 * (Although NetHack did not yet exist when this game was written)
 *
 * Juki <jtv@hut.fi>
 */

#include <stdio.h>
#include <sys/file.h>
#include <sys/signal.h>

/* The MACH define does not necessarily indicate that
 * we have a flock() call, but what the heck, this is a
 * game anyway :-)
 */

#if defined(BSD) || defined(_BSD) || defined (MACH) || defined(HAVE_FLOCK)
# undef HAVE_FLOCK
# define HAVE_FLOCK
#endif

hackrestore ()
{
  fprintf (stderr, "HACK RESTORE not implemented");
  sleep (1);
}

savehack ()
{
  fprintf (stderr, "HACK SAVE not implemented");
  sleep (1);
}

dorestore ()
{
  return 0;
}

void
ranini ()
{
  srandom (time(0));
}

rand ()
{
  return random ();
}

int
lockfile (file, lock_it)
     FILE *file;
     int  lock_it;
{
  int fd;
  int res;
  int fun = 0;

  if (! file)
    return;

  fd = fileno (file);

#ifdef HAVE_FLOCK
  if (lock_it)
    fun = LOCK_EX;
  else
    fun = LOCK_UN;
#else
  if (lock_it)
    fun = 1; /* F_LOCK */
  else
    fun = 0; /* F_ULOCK */
#endif

  /* Lock the whole file with exclusive access */
  /* Return 0 on success */
#ifdef HAVE_FLOCK
  res = flock (fd, fun);
#else
  res = lockf (fd, fun, 0);
#endif

  if (res == -1)
    perror ("locking failed");

  return res;
}

void
hack_suspend ()
{
#ifdef SIGSTOP
  kill (getpid (), SIGSTOP);
#endif
}
