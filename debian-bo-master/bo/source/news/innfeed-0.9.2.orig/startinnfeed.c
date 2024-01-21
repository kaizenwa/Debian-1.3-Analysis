/*
 * $Id: startinnfeed.c,v 1.2 1996/11/28 19:22:59 brister Exp $
 *
 * Start innfeed and pass it all the arguments given. Sets up process
 * limits for innfeed.
 */

#define USER "news"
#define INNFEED "/news/bin/innfeed"

#include <pwd.h>                /* getpwent */
#include <stdio.h>              /* fprintf */
#include <errno.h>              /* errno, sys_errlist */
#include <unistd.h>             /* setgid, setuid, execve */
#include <stdlib.h>             /* exit */
#include <sys/types.h>          /* setrlimit */
#include <time.h>               /* setrlimit */
#include <sys/resource.h>       /* setrlimit */

static char *innfeed = INNFEED;

void
main(ac, av, ep)
     int ac;
     char **av;
     char **ep;

{
  struct passwd *pwd;
  struct rlimit rl;
  char *progname;

  /* (try to) unlimit datasize and stacksize for us and our children */
  rl.rlim_cur = rl.rlim_max = RLIM_INFINITY;

  if (setrlimit(RLIMIT_DATA, &rl) == -1)
    (void)fprintf(stderr, "%s: setrlimit(RLIMIT_DATA, RLIM_INFINITY): %s\n",
            *av, sys_errlist[errno]);
  if (setrlimit(RLIMIT_STACK, &rl) == -1)
    (void)fprintf(stderr, "%s: setrlimit(RLIMIT_STACK, RLIM_INFINITY): %s\n",
            *av, sys_errlist[errno]);
  if (setrlimit(RLIMIT_NOFILE, &rl) == -1)
    (void)fprintf(stderr, "%s: setrlimit(RLIMIT_NOFILE, RLIM_INFINITY): %s\n",
            *av, sys_errlist[errno]);

  /* stop being root */
  pwd = getpwnam(USER);
  if (pwd == (struct passwd *)NULL)
    (void)fprintf(stderr, "%s: getpwnam(%s): %s\n", *av, USER,
                  sys_errlist[errno]);
  else if (setgid(pwd->pw_gid) == -1)
    (void)fprintf(stderr, "%s: setgid(%ld): %s\n", *av, pwd->pw_gid,
                  sys_errlist[errno]);
  else if (setuid(pwd->pw_uid) == -1)
    (void)fprintf(stderr, "%s: setuid(%ld): %s\n", *av, pwd->pw_uid,
                  sys_errlist[errno]);
  else 
    {
      progname = av[0];
      av[0] = innfeed;

      if (execve(innfeed, av, ep) == -1)
        (void)fprintf(stderr, "%s: execve: %s\n",
                      progname, sys_errlist[errno]);
    }
  
  exit(1);
}
