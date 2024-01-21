/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "exim.h"



/*************************************************
*              Create a child process            *
*************************************************/

/* This function creates a child process and runs the given command in it. It
sets up a pipe to the standard input of the new process, and returns that to
the caller via fdptr. An optional file descriptor may be supplied for the
standard output and standard error of the subprocess. A value < 0 implies none,
in which case we set up /dev/null. The function returns the pid of the new
process, or -1 if things go wrong.

A new umask is supplied for the process, and an optional new uid and gid are
also available. These are used by the queryprogram router to set an
unprivileged id.

Arguments:
  argv        the argv for exec in the new process
  envp        the envp for exec in the new process
  newumask    umask to set in the new process
  newuid      point to uid for the new process or NULL for no change
  newgid      point to gid for the new process or NULL for no change
  fdptr       pointer to int into which the fd of the stdin of the new process
                is placed
  outfd       a fd for the stdout and stderr of the new process; if -1 is given
                fds to /dev/null are set up

Returns:      the pid of the created process or -1 if anything has gone wrong;
              (via fdptr) the fd of the standard input of the new process
*/

pid_t
child_open(char **argv, char **envp, int newumask, uid_t *newuid, gid_t *newgid,
  int *fdptr, int outfd)
{
int pfd[2];

/* If no output provided, use /dev/null; ensure outfd is not zero. */

if (outfd < 0) outfd = open("/dev/null", O_WRONLY);

if (outfd == 0)
  {
  int fd = dup(outfd);
  close(outfd);
  outfd = fd;
  }

/* Create the pipe and fork the process */

if (pipe(pfd) == 0)
  {
  pid_t pid = fork();

  /* The child process makes the reading end of the pipe into the
  standard input, and outfd into the standard output and error,
  then execs, having closed all other file descriptors. */

  if (pid == 0)
    {
    int fd;

    close(pfd[pipe_write]);     /* Be absolutely sure this one is closed */

    for (fd = mac_maxfd; fd >= 0; fd--)
      if (fd != pfd[pipe_read] && fd != outfd) close(fd);

    if (pfd[pipe_read] != 0)
      {
      dup2(pfd[pipe_read], 0);  /* Make the standard input */
      close(pfd[pipe_read]);
      }

    if (outfd > 0)
      {
      if (outfd != 1) dup2(outfd, 1);
      if (outfd != 2) dup2(outfd, 2);
      if (outfd != 1 && outfd != 2) close(outfd);
      }

    /* Recover the power to setuid if necessary. We shouldn't be
    here not as root if it isn't available. */

    if (geteuid() != root_uid) mac_seteuid(root_uid);

    /* Set the required environment. If changing uid, ensure that
    SIGUSR1 is ignored, as the process won't have the privilege to
    write to the process log. */

    if (newgid != NULL) setgid(*newgid);
    if (newuid != NULL)
      {
      signal(SIGUSR1, SIG_IGN);
      setuid(*newuid);
      }
    umask(newumask);

    /* Now do the exec */

    if (envp == NULL) execv(argv[0], argv);
      else execve(argv[0], argv, envp);

    /* Failed to execv */

    _exit(errno);         /* Note: must be _exit(), NOT exit() */
    }

  /* Parent */

  close(pfd[pipe_read]);
  if (pid > 0)            /* fork succeeded */
    {
    *fdptr = pfd[pipe_write];
    return pid;
    }
  else                    /* fork failed */
    {
    close(pfd[pipe_write]);
    return (pid_t)(-1);
    }
  }
else return (pid_t)(-1);
}



/*************************************************
*           SIGALRM handler for child_close      *
*************************************************/

static volatile BOOL child_sigalrm_seen;

static void
child_alarm_handler(int sig)
{
child_sigalrm_seen = TRUE;
}


/*************************************************
*           Close down child process             *
*************************************************/

/* Wait for the given process to finish. Called when there is only one child in
existence, or when picking up any others en route doesn't matter.

Arguments
  pid:      the pid to wait for
  timeout:  maximum time to wait; 0 means for as long as it takes

Returns:    >= 0          process terminated by exiting; value is process
                            ending status
            < 0 & > -256  process was terminated by a signal; value is the
                            negation of the signal number
            -256          timed out
            -257          other error in wait(); errno still set
*/

int
child_close(pid_t pid, int timeout)
{
int yield;

if (timeout > 0)
  {
  child_sigalrm_seen = FALSE;
  signal(SIGALRM, child_alarm_handler);
  alarm(timeout);
  }

for(;;)
  {
  int status;
  pid_t rc = wait(&status);
  if (rc == pid)
    {
    int lowbyte = status & 255;
    if (lowbyte == 0) yield = (status >> 8) & 255;
      else yield = -lowbyte;
    break;
    }
  if (rc < 0)
    {
    yield = (errno == EINTR && child_sigalrm_seen)? -256 : -257;
    break;
    }
  }

if (timeout > 0)
  {
  alarm(0);
  signal(SIGALRM, SIG_IGN);
  }

return yield;
}

/* End of child.c */
