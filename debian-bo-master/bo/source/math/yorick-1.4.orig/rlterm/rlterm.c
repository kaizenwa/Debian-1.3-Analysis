/* RLTERM.C
   Trivial shell to add GNU readline capabilities to any line-oriented
   UNIX program.  Instead of starting your program with:

     program arg1 arg2 arg3

   start instead with:

     rlterm program arg1 arg2 arg3

   You can alias program to "rlterm|program" in your shell to always get
   rlterm.  You can modify the behavior of rlterm with a ~/.inputrc file,
   as described in the GNU readline documentation.

   Beware of attempting to redirect stderr when using rlterm; such an
   attempt will probably cause a malfunction.
 */

#ifdef POSIX_TERMIO
#  undef USE_TERMIOS
#  define USE_TERMIOS
#endif

#ifndef USE_TERMIOS
#  ifndef USE_SYS_TERMIO
#    include <termio.h>
#  else
#    include <sys/termio.h>
#  endif
#  define TC_GETTER TCGETA
#  define TC_SETTER TCSETA
#else
#  ifndef USE_SYS_TERMIO
#    include <termios.h>
#  else
#    include <sys/termios.h>
#  endif
#  define TC_GETTER TCGETS
#  define TC_SETTER TCSETS
#endif

#ifndef POSIX_TERMIO
#  define GET_STTY(fd, tiop) (ioctl(fd, TC_GETTER, tiop))
#  define SET_STTY(fd, tiop) (ioctl(fd, TC_SETTER, tiop))
#else
#  define GET_STTY(fd, tiop) (tcgetattr(fd, tiop))
#  define SET_STTY(fd, tiop) (tcsetattr(fd, TCSANOW, tiop))
#endif

/* BSD would use struct termios instead? */
static struct termio tty_settings;

#include <signal.h>

static void child_died();
static void child_died()
{
  /* (See note below about sigaction.)  */
  /* restore initial tty settings -- readline mucks with them */
  SET_STTY(0, &tty_settings);
  exit(0);
}

int main(argc, argv)
     int argc;
     char *argv[];
{
  int pipeline[2];
  if (!pipe(pipeline)) {
    /* get initial tty settings -- readline mucks with them */
    GET_STTY(0, &tty_settings);

    if (fork()) {
      /* we are the rlterm parent process */
      extern char *readline();
      char *line;
      int len;

      close(pipeline[0]);  /* read side of pipe is child's stdin */

      /* If user types ^C, it's meant for the child, not us.  */
      signal(SIGINT, SIG_IGN);
      /* detect child exiting */
#ifndef NO_SIGACTION
      {
	struct sigaction act;
	act.sa_handler= &child_died;
	sigemptyset(&act.sa_mask);
	act.sa_flags= SA_NOCLDSTOP;
	sigaction(SIGCHLD, &act, 0);
      }
#else
      /* if sigaction is not available, SIGCHLD may be produced if the
	 child simply stops (as by ^Z), rather than truly terminating */
      signal(SIGCHLD, &child_died);
#endif

      for (;;) {
	/* get line from user on stdin (fd 0), echoing to stdout (fd 1)
	   -- note the jiggery-pokery with dup above to make stdout right */
	line= readline("");
	if (!line) break;	/* stdin closed somehow, die */
	if (*line) add_history(line);

	/* pass line to target code */
	len= strlen(line);
	line[len]= '\n';	/* replace final \0 by newline */
	/* if write fails, pipe to target has probably closed, so die
	   -- but may also get SIGPIPE if that happens, which will just
	   quietly slit our throat by default */
	if (write(pipeline[1], line, len+1)<0) break;
	free(line);
      }

      /* restore initial tty settings -- readline mucks with them */
      SET_STTY(0, &tty_settings);

    } else {
      /* we are the child process */
      close(pipeline[1]);    /* close write side of pipe */
      dup2(pipeline[0], 0);  /* dup read side of pipe to fd 0 (stdin) */
      close(pipeline[0]);    /* then close it */
      execvp(argv[1], &argv[1]);
    }
    return 0;

  } else {
    /* pipe failed to be created */
    return 1;
  }
}
