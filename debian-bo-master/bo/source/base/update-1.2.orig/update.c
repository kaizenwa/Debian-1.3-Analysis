/*
 * Update Daemon for Linux (and possibly other Unixen).
 * Copyright (c) 1996 Torsten Poulin.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $RCSfile: update.c,v $
 * $Author: torsten $
 * $Date: 1996/07/27 11:30:03 $
 * $Revision: 1.2 $
 */

#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <stdlib.h>
#include <signal.h>

/*
 * If NO_BDFLUSH is defined, make this a traditional update
 * daemon that calls sync(). This is the default for systems
 * other than Linux.
 */

#ifdef __linux__
# include <asm/unistd.h>
  _syscall2(int, bdflush, int, func, int, data);
#else
# ifndef NO_BDFLUSH
#   define NO_BDFLUSH
# endif
#endif

#ifdef NO_BDFLUSH
# define ARGS "s:"
#else
# define ARGS "Ss:f:"
# include <syslog.h>
#endif

/*
 * OPEN_MAX is POSIX.1
 */
#ifndef OPEN_MAX
# define OPEN_MAX 256		/* anybody's guess */
#endif

static const char
RCS[] = "@(#) $Revision: 1.2 $";

unsigned int sync_duration = 30;
unsigned int flush_duration = 5;
int use_sync = 0;

void
usage(char *name)
{
  fprintf(stderr,
#ifdef NO_BDFLUSH
	  "Usage: %s [-s secs]\n",
#else
	  "Usage: %s [-S] [-s secs] [-f secs]\n",
#endif
	  name);
  exit (1);
}

/*
 * The update() function never returns.
 * If bdflush() fails, it reverts to sync() instead.
 */

void
update(void)
{
  for (;;)
    {
#ifndef NO_BDFLUSH
      if (use_sync)
	{
#endif
	  sleep(sync_duration);
	  sync();
#ifndef NO_BDFLUSH
	}
      else
	{
	  sleep(flush_duration);
	  if (bdflush(1, 0) < 0)
	    {
	      use_sync = 1;
	      openlog("update", LOG_CONS, LOG_DAEMON);
	      syslog(LOG_INFO, "Switching to sync(2) instead of bdflush(2)");
	      closelog();
	    }
	}
#endif
    }
}

int
main(int argc, char **argv)
{
  int f, c;

  while ((c = getopt(argc, argv, ARGS)) != -1)
    switch (c)
      {
      case 'S': use_sync = 1; break;
      case 's':	sync_duration = atoi(optarg); break;
      case 'f': flush_duration = atoi(optarg); break;
      case '?': usage(argv[0]);
      default: abort();
      }

  if (sync_duration == 0 || flush_duration == 0)
    {
      fprintf(stderr, "%s: argument must be positive integer.\n", argv[0]);
      exit(1);
    }

  if (optind < argc)
    usage(argv[0]);

  /*
   * Prevent people from launching more update daemons.
   * Might as well call sync().
   */
  if (geteuid() != 0)
    {
      sync();
      fprintf(stderr, "%s: should only be run by root.\n", argv[0]);
      exit(1);
    }

  /*
   * Ignore a few signals for good measure.
   */
  signal(SIGTERM, SIG_IGN);
  signal(SIGINT, SIG_IGN);

  if (fork() > 0)
    exit(0);

  /*
   * Become session leader (get rid of controlling terminal).
   */
  setsid();

  /*
   * Make sure we are on a file system that stays mounted.
   */
  chdir("/");

  for (f = 0; f < OPEN_MAX; f++)
    close(f);

  update();

  return 0;
}


