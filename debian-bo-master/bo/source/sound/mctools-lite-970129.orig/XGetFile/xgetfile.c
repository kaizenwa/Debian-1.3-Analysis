/* Copyright (C) 1994 - 1996
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

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
 Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include "../config.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/stat.h>
#include <stdlib.h>

#include <McTools/McApp.h>
#include <McTools/McGadget.h>
#include <McTools/McSlider.h>
#include <McTools/McText.h>
#include <McTools/McBitmap.h>
#include <McTools/McString.h>
#include <McTools/McSelector.h>
#include <McTools/McResource.h>
#include <McTools/McFileRequest.h>

#include "xgetfile.h"

McApp *app;
void cleanup(int r);

static void enqueue(unsigned char *command);
static int execute(unsigned char *command);
static void sighndl(int nonsense);
static void usage(void);

typedef struct do_it {
  struct do_it *next;
  char cmd;
} do_it;

static int queue_up = 0;
static int use_queue = 0;
static int verbose = 0;
static do_it *first=NULL;
static do_it *last=NULL;
static McFileRequest *req;

static XrmOptionDescRec userTable[] = {
  { "-path",    "*defaultPath",    XrmoptionSepArg, (caddr_t)NULL },
  { "-pattern", "*defaultPattern", XrmoptionSepArg, (caddr_t)NULL },
  { "-file",    "*defaultFile",    XrmoptionSepArg, (caddr_t)NULL },
  { "-exec",    "*defaultProgram", XrmoptionSepArg, (caddr_t)NULL },
  { "-open",    "*defaultOpen",    XrmoptionSepArg, (caddr_t)NULL },
  { "-quote",   "*defaultQuote",   XrmoptionNoArg,  (caddr_t)"on" },
  { "-popup",   "*defaultPopup",   XrmoptionNoArg,  (caddr_t)"on" },
  { "-queue",   "*queueCommands",  XrmoptionNoArg,  (caddr_t)"on" },
  { "-v",       "*verbose",        XrmoptionNoArg,  (caddr_t)"on" },
  { "-format",  "*dirFormat",      XrmoptionSepArg, (caddr_t)NULL },
};

static int file_proc(unsigned char *, unsigned char *file, unsigned char *);

static unsigned char *path=NULL, *file=NULL, *pattern=NULL, *code=NULL;

void main(int argc, char **argv) {

  myname = argv[0];

  /*
   * Build connection with X server
   */
  app = McCreateApp(&argc, argv, APP_VERSION, APP_CLASS,
		    userTable, sizeof(userTable)/sizeof(userTable[0]));

  if ((argc==2) && ((!strcmp(argv[1],"-?")) || (!strcmp(argv[1],"-h"))))
    usage();

  if (argc>1) {
    fprintf(stderr, _("%s: Too many arguments\n"), myname);
    cleanup(1);
  }

  path=McGetResource(app, "defaultPath");
  file=McGetResource(app, "defaultFile");
  pattern=McGetResource(app, "defaultPattern");
  code=McGetResource(app, "defaultProgram");
  use_queue = McGetSwitch(app, "queueCommands");
  verbose = McGetSwitch(app, "verbose");

  if (use_queue && code)
#ifdef SIGCHLD
    signal(SIGCHLD, sighndl);
#else
    signal(SIGCLD, sighndl);
#endif

  req=McCreateFileRequest(app,
			  McGetSwitch(app, "defaultPopup")?MCF_POPUP:0,
			  NULL, pattern, path, file,
			  McGetResource(app, "defaultOpen"), NULL,
			  file_proc);

  if (!req) cleanup(127); /* Ooops? */

  McAppMainLoop(app);
}

static int file_proc(unsigned char *foo,
		     unsigned char *fullpath, unsigned char *bar) {
  char buf[2048];

  if (!fullpath) {
    cleanup(1);
  }

  if (!code) {
    if (McGetSwitch(app, "defaultQuote")) {
      printf("'%s'\n", fullpath);
    } else {
      puts(fullpath);
    }
    cleanup(0);
  }

  sprintf(buf, code,
	  fullpath, fullpath, fullpath, fullpath,
	  fullpath, fullpath, fullpath, fullpath);
  if (use_queue) {
    enqueue(buf);
  } else {
    if (verbose)
      printf(_("%s: executing `%s'\n"),myname,buf);
    system(buf);
  }

  return 0;
}

void cleanup(int r) {
  if (req) McRemoveFileRequest(&req);
  McFreeApp(app);
  exit(r);
}

static void usage(void) {
  printf(_(
"Usage: %s [-title <title>] [-path <path>] [-pattern <pattern>]\n"
"       [-file <file>] [-exec <cmd>] [queue] [-v]\n"
"\n"
"-title <title>      Title to show in the window\'s title bar\n"
"-path <path>        Default search path\n"
"-pattern <pattern>  Default search pattern\n"
"-file <file>        Default file to select\n"
"-quote              Print file in single quotes on exit\n"
"-popup              pop up as a transient window\n"
"-exec <cmd>         Command to execute with the selected file passed as %%s\n"
"-queue              Flag for -exec: Queue up the selected files and spawn a\n"
"                                    new process for each file when the\n"
"                                    previous process has finished.\n"),
	 myname);

  cleanup(1);
}

static void enqueue(unsigned char *command) {
  do_it *now;

  if (command) {
    if (verbose && queue_up)
      printf(_("%s: Enqueueing `%s'\n"),myname,command);
    now=(do_it *)malloc(sizeof(do_it *)+strlen(command)+1);
    now->next=NULL;
    if (last) last->next=now;
    last=now;
    if (!first) first=now;
    strcpy(&now->cmd,command);
  }

  if (queue_up==0) {
    if ((now=first)) {
      queue_up=1;
      if (verbose)
	printf(_("%s: Forking `%s'\n"), myname, &now->cmd);
      if (execute(&now->cmd)) {
	fprintf(stderr, "%s: ", myname);
	perror(_("Can't fork"));
	queue_up=0;
      }
      if (last==now) last=NULL;
      first=now->next;
      free(now);
    }
  }
}

extern char **environ;
static int execute(unsigned char *command) {
  int child;
  const unsigned char *new_argv[4];

  if ((child=fork())<0) return -1;
  if (child) return 0;

  /* Child side.  */
  new_argv[0] = "sh";
  new_argv[1] = "-c";
  new_argv[2] = command;
  new_argv[3] = NULL;
  /* Exec the shell.  */
  execve ("/bin/sh", (char *const *) new_argv, environ);
  fprintf(stderr,_("%s: Can't exec '%s'"),myname,command);
  perror(" ");
  exit(127);
}

static void sighndl(int nonsense) {
#ifdef SIGCHLD
    signal(SIGCHLD, sighndl);
#else
    signal(SIGCLD, sighndl);
#endif
  waitpid(-1, NULL, WNOHANG);
  queue_up = 0;
  enqueue(NULL);
}
