/* gdbserver for simchecker.
   Copyright 1995 Tristan Gingold
   
   Adapted from a gdb source file.
   
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include <string.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netdb.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <signal.h>
#include <stdio.h>
#include <sys/file.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <errno.h>

int remote_desc;
int piped_desc[2];
int debug_proto;

char *progname;

/* Generally useful subroutines used throughout the program.  */

/* Print the system error message for errno, and also mention STRING
   as the file name for which the error was encountered.
   Then return to command level.  */

void
perror_with_name (string)
     char *string;
{
  extern int sys_nerr;
  extern char *sys_errlist[];
  char *err;
  char *combined;

  if (errno < sys_nerr)
    err = sys_errlist[errno];
  else
    err = "unknown error";

  combined = (char *) alloca (strlen (err) + strlen (string) + 3);
  strcpy (combined, string);
  strcat (combined, ": ");
  strcat (combined, err);

  fprintf (stderr, "%s.\n", combined);
  exit (33);
}

void
dump_frame (unsigned char *buf, char way, int len)
{
 int i;
 putchar (way);
 for (i = 0; i < len; i++)
   {
     if (buf[i] < 32 || buf[i] > 127)
       printf ("\\%d", buf[i]);
     else
       putchar (buf[i]);
   }
 putchar ('\n');
}

void
do_debug_proto (void)
{
 struct pollfd fds[2];
 char buf[2048];
 int len;
 
 fds[0].fd = remote_desc;
 fds[0].events = POLLIN | POLLHUP;
 fds[1].fd = piped_desc[0];
 fds[1].events = POLLIN | POLLHUP;
 while (1)
   {
     if (poll (fds, 2, -1) == -1)
       perror_with_name ("Can't poll");
     if (fds[0].revents & POLLIN)
       {
         len = read (remote_desc, buf, 2048);
         write (piped_desc[0], buf, len);
         dump_frame (buf, '>', len);
       }
     if (fds[1].revents & POLLIN)
       {
         len = read (piped_desc[0], buf, 2048);
         write (remote_desc, buf, len);
         dump_frame (buf, '<', len);
       }
     if (fds[0].revents & POLLHUP || fds[1].revents & POLLHUP)
       break;
   }
}

/* Open a connection to a remote debugger.
   NAME is the filename used for communication.  */

void
remote_open (char *name)
{
#if 0
  struct sgttyb sg;
#endif

  if (!strchr (name, ':'))
    {
      remote_desc = open (name, O_RDWR);
      if (remote_desc < 0)
	perror_with_name ("Could not open remote device");

#if 0
      ioctl (remote_desc, TIOCGETP, &sg);
      sg.sg_flags = RAW;
      ioctl (remote_desc, TIOCSETP, &sg);
#endif
    }
  else
    {
      char *port_str;
      int port;
      struct sockaddr_in sockaddr;
      int tmp;
      struct protoent *protoent;
      int tmp_desc;

      port_str = strchr (name, ':');

      port = atoi (port_str + 1);

      tmp_desc = socket (PF_INET, SOCK_STREAM, 0);
      if (tmp_desc < 0)
	perror_with_name ("Can't open socket");

      /* Allow rapid reuse of this port. */
      tmp = 1;
      setsockopt (tmp_desc, SOL_SOCKET, SO_REUSEADDR, (char *)&tmp,
		  sizeof(tmp));

      sockaddr.sin_family = PF_INET;
      sockaddr.sin_port = htons(port);
      sockaddr.sin_addr.s_addr = INADDR_ANY;

      if (bind (tmp_desc, (void *)&sockaddr, sizeof (sockaddr))
	  || listen (tmp_desc, 1))
	perror_with_name ("Can't bind address");

      tmp = sizeof (sockaddr);
      remote_desc = accept (tmp_desc, (void *)&sockaddr, &tmp);
      if (remote_desc == -1)
	perror_with_name ("Accept failed");

      protoent = getprotobyname ("tcp");
      if (!protoent)
	perror_with_name ("getprotobyname");

      /* Enable TCP keep alive process. */
      tmp = 1;
      setsockopt (tmp_desc, SOL_SOCKET, SO_KEEPALIVE, (char *)&tmp, sizeof(tmp));

      /* Tell TCP not to delay small packets.  This greatly speeds up
	 interactive response. */
      tmp = 1;
      setsockopt (remote_desc, protoent->p_proto, TCP_NODELAY,
		  (char *)&tmp, sizeof(tmp));

      close (tmp_desc);		/* No longer need this */

      signal (SIGPIPE, SIG_IGN); /* If we don't do this, then gdbserver simply
				    exits when the remote side dies.  */
    }

#if 0
  fcntl (remote_desc, F_SETFL, FASYNC);
#endif

  fprintf (stderr, "Remote debugging using %s\n", name);
}


void
main (int argc, char *argv[])
{
  int status, pid;
  char buf[32];
  
  progname = argv[0];
  
  if (argc > 1 && strcmp(argv[1], "-v") == 0)
    {
      debug_proto = 1;
      argc--;
      argv++;
    }
  
  if (argc < 3)
    {
      fprintf (stderr, "Usage: %s [-v] tty prog [args ...]\n", progname);
      exit (1);
    }

  do
    {
      remote_open (argv[1]);
      if (debug_proto)
        {
          if (socketpair (AF_UNIX, SOCK_STREAM, 0, piped_desc))
            perror_with_name ("Can't open socketpair");
        }
      if (fork() == 0)
        {
          /* Child */
          if (debug_proto)
            sprintf (buf, "CHKRGDBSERVER=%d", piped_desc[1]);
          else
            sprintf (buf, "CHKRGDBSERVER=%d", remote_desc);
          putenv (buf);
          execv (argv[2], &argv[2]);
          perror_with_name ("execve");
        }
      if (debug_proto)
        do_debug_proto ();
      pid = wait (&status);
      close (remote_desc);
    }
  while (pid != -1 && WIFEXITED(status) && WEXITSTATUS(status) != 33);
  exit (0);
}
