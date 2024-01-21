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
#include <strings.h>
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <McTools/McApp.h>
#include <McTools/McGadget.h>
#include <McTools/McInfoRequest.h>
#include "xpedit.h"
#include "cd.h"
#include "struct.h"
#include "discid.h"

/*****************************************************************************
 *
 * Pop up the editwin window by firing up an instance of xmdb
 *
 */
extern char **environ;

int editor_pid=0;
static void child_handler(int arg);

extern void create_edit_window(void) {
  unsigned char *cmdline[4];
  unsigned char cmdbuf[1024];

  if (editor_pid || (thiscd.ntracks<=0)) {
    XBell(app->display, 0);
    return;
  }

  if (currentFile)
    sprintf(cmdbuf, "exec xmdb -edit %s -notify %d",
	    currentFile, (int)(mainWindow->framewin));
  else
    sprintf(cmdbuf, "exec xmdb -new %08lx -count %d -notify %d",
	    get_discid(), thiscd.ntracks, (int)(mainWindow->framewin));

  cmdline[0]="sh";
  cmdline[1]="-c";
  cmdline[2]=cmdbuf;
  cmdline[3]=NULL;

  if ((editor_pid=fork())<0) {
    editor_pid=0;
    McError(app, _("Can't fork media editor.\nReason: %s\n"), strerror(errno));
    return;
  }

  if (editor_pid) {
    signal(SIGCHLD, child_handler);
    return;
  }

  execve ("/bin/sh", (char *const *) cmdline, environ);

  /* Oh dear... */
  exit(127);
}

static void child_handler(int arg) {
  waitpid(-1, NULL, WNOHANG);
  editor_pid=0;
}

void close_editor(void) {
  if (editor_pid) {
    kill(editor_pid, SIGTERM);
    editor_pid=0;
  }
}

