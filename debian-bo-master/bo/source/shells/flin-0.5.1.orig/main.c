/*
   This is a program to display menus on a text tty, good for limited access
   guest accounts, or just for user menus.
   Copyright (C) 1995, 1996  Brian Cully

   This program is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation; either version 2 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program; see the file COPYING. If not write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   please send patches or advice to: `shmit@kublai.com'
 */

/* Include configuration data */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <pwd.h>
#include <syslog.h>
#include <signal.h>

#include "parse.h"
#include "screen.h"
#include "additional.h"
#include "config.h"

extern char delim;
extern char escape;
extern char comment;

extern void *chk_alloc(size_t size);
static void setupfile(const char *);
void handle_children(void);

/*char progname[255]; */
char *progname;
char *menufile;
int exec_logging = 0;
int noclobber = 0;
struct menu *main_menu;

static void usage()
{
   fprintf(stderr,
	   "Usage: %s [-hV] [-d <delimiter>] [-e <escape>] [-c <comment>] [menu file]\n\n\
   -d <delimiter>\tSets the token delimiter character to <delimiter>\n\
   -e <escape>\t\tSets the escape character to <escape>\n\
   -c <comment>\t\tSets the comment character to <comment>\n\
   -h\t\t\tDisplay help and exit\n\
   -l\t\t\tLog all execs to syslog\n\
   -V\t\t\tDisplay version and compile-time options then exit\n\
   [menu file]\t\tMenu file to load from\n\n\
With no options [menu file] is set to `%s'\n",
	   progname, DEFAULT_MENU);
}

static void version()
{
   fprintf(stderr,
	   "Flin version %s, Copyright (C) 1995, 1996 Brian Cully <shmit@kublai.com>\n\
Flin comes with ABSOLUTELY NO WARRANTY. This is free software,\n\
you are welcome to redistribute it and modify it under the\n\
conditions of the GNU General Public License.\n\n\
Compile-time options:\n\
Token delimiter: `%c', Comment delimiter: `%c', Escape delimiter: `%c'\n\
Default menu: `%s'\n",
	   VERSION, DELIM, COMMENT, ESCAPE, DEFAULT_MENU);
}

int main(int argc, char *argv[])
{
   struct menu *guest_menu;
   struct passwd *ppwd, pwd;
   struct sigaction child_act;
   char *filename = DEFAULT_MENU;

   progname = argv[0];

   ppwd = getpwuid(getuid());
   memcpy(&pwd, ppwd, sizeof(struct passwd));

   signal(SIGINT, SIG_IGN);
   signal(SIGTSTP, SIG_IGN);
   signal(SIGQUIT, SIG_IGN);

   child_act.sa_handler = handle_children;
   sigemptyset(&child_act.sa_mask);
   child_act.sa_flags = 0;
   sigaction(SIGCHLD, &child_act, (struct sigaction *) NULL);

   if (argc < 2) {
      setupfile(DEFAULT_MENU);
   } else {
      char c;

      /* Read command line options */

      /* Use our own messages */
      opterr = 0;

      while ((c = getopt(argc, argv, "lhVd:e:c:")) != -1)
	 switch (c) {
	 case 'd':
	    if (strlen(optarg) > 1) {
	       usage();
	       exit(1);
	    }
	    delim = *optarg;
	    break;
	 case 'e':
	    if (strlen(optarg) > 1) {
	       usage();
	       exit(1);
	    }
	    escape = *optarg;
	    break;
	 case 'c':
	    if (strlen(optarg) > 1) {
	       usage();
	       exit(1);
	    }
	    comment = *optarg;
	    break;
	 case 'h':
	    usage();
	    exit(0);
	 case 'l':
	    exec_logging = 1;
	    break;
	 case 'V':
	    version();
	    exit(0);
	 case '?':
	    if (isprint(optopt))
	       fprintf(stderr, "Unknown option: `-%c'.\n", optopt);
	    else
	       fprintf(stderr, "Unknown option character: `\\x%x'.\n", optopt);
	    usage();
	    exit(1);
	 default:
	    abort();
	 }

      if (optind < argc)
	 filename = argv[optind];

   }
   /* Load file into buffer */
   setupfile(filename);

   /* Parse File */
   if (parsefile(&guest_menu, menufile) == -1) {
      fprintf(stderr, "%s: error while parsing\n", progname);
      freemem(guest_menu, menufile);
      exit(1);
   }
   main_menu = guest_menu;

   openlog(pwd.pw_name, 0, LOG_USER);

   /*

      Process SYSTEM rc files

    */
   processrcfile(SYSTEM_RC, SYSTEM);
   if (*progname == '-')
      processrcfile(SYSTEM_LOGIN_RC, SYSTEM);

   /*

      Process USER rc files

    */
   processrcfile(USER_RC, USER);
   if (*progname == '-')
      processrcfile(USER_LOGIN_RC, USER);

   /*
      Looking over peoples' shoulders
    */

   if (exec_logging == 1)
      syslog(LOG_LOCAL1 | LOG_INFO, "%d: Flin started %s",
	     getpid(), filename);

   init_scr();
   display_list(guest_menu);
   close_scr();

   /* Free all allocated memory */
   freemem(guest_menu, menufile);

   if (exec_logging == 1)
      syslog(LOG_LOCAL1 | LOG_INFO, "%d: Flin finished",
	     getpid());

   /* we outtie */
   exit(0);
}

static void setupfile(const char *filen)
{
   int fd;
   struct stat statbuff;

   /* Load Files, get stats, and malloc() buffer, read file into buffer */

   if ((fd = open(filen, O_RDONLY)) == -1) {
      fprintf(stderr, "%s: Couldn't open file %s for reading\n", progname, filen);
      exit(1);
   }
   if (fstat(fd, &statbuff) == -1) {
      fprintf(stderr, "%s: Couldn't get statistics on file %s\n", progname, filen);
      exit(1);
   }
   if (statbuff.st_size == 0) {
      fprintf(stderr, "%s: %s is an empty file\n", progname, filen);
      exit(1);
   }
   if ((menufile = (char *) chk_alloc(statbuff.st_size)) == NULL) {
      fprintf(stderr, "%s: Couldn't allocate %i bytes from heap\n", progname, (int) statbuff.st_size);
      exit(1);
   }
   if (read(fd, menufile, statbuff.st_size) == -1) {
      fprintf(stderr, "%s: Couldn't read file %s\n", progname, filen);
      exit(1);
   }
   close(fd);
}
