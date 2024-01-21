/* Asynchronous processes using ptys */
/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */


#ifdef HAS_SUBPROCESSES

#include "config.h"

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <fcntl.h>
#include <signal.h>

#include <unistd.h>

#include "slang.h"
#include "_slang.h"
#include "slfile.h"

/* On ultrix, pseudo terminals are named /dev/tty?? and /dev/pty??
   where ?? is [pq][0-9a-f] */
      
static char PTY_Name[80], TTY_Name[80];
      
int open_pty ()
{
   int ft, fp;
   char *p, *t, *p1, *t1;
   char *list1 = "pq", *list2 = "0123456789abcdef";
   
   strcpy (PTY_Name, "/dev/pty");
   strcpy (TTY_Name, "/dev/tty");
   
   p = PTY_Name + strlen (PTY_Name); t = TTY_Name + strlen (TTY_Name);

   for (p1 = list1; *p1 != 0; p1++)
     {
	*p++ = *p1;  *t++ = *p1;
	for (t1 = list2; *t1 != 0; t1++)
	  {
	     *p++ = *t1; *p = 0;
	     *t++ = *t1; *t = 0;
	     
	     if ((fp = open (PTY_Name, O_RDWR)) >= 0)
	       {
		  if ((ft = open (TTY_Name, O_RDWR)) >= 0)
		    {
		       close (ft);
		       return fp;
		    }
		  close (fp);
	       }
	     t--; p--;
	  }
	t--; p--;
     }
   
   return -1;
}

	     
   

int child_code (char *name)
{
   execl (name, NULL);
   exit(0);
}

/* returns the child pid upon success or -1 upon error.  */
static int create_process (char *name, int *fd)
{
   int child_pid;
   int ft;
   
   *fd = open_pty();
   if (*fd < 0) return -1;
   
   
   if (fcntl (*fd, F_SETFL, O_NDELAY) < 0)
     {
	fprintf(stderr, "fcntl 1 failed.\n");
     }

	  
   if (0 == (child_pid = fork()))
     {
	setsid ();
	if ((ft = open (TTY_Name, O_RDWR)) < 0)
	  {
	     fprintf(stderr, "Child: unable to open tty");
	  }
	dup2(ft, fileno(stderr));
	dup2(ft, fileno(stdout));
	dup2(ft, fileno(stdin));
	child_code (name);
     }
   
   return child_pid;
}

void child_signal(int sig)
{
}


int SLcreate_child_process (char *name)
{
   SL_File_Table_Type *t;
   FILE *fp;
   int fd, pid;
   
   if ((t = get_file_table_entry()) == NULL) return (-1);
   
   if (-1 == (pid = create_process (name, &fd))) return -1;
     
   signal (SIGCHLD, child_signal);
   
   if ((fp = fdopen (fd, "r+")) == NULL)
     {
	close (fd);
	return -1;
     }
   t->fp = fp;
   t->fd = fd;
   t->flags = SL_WRITE | SL_READ | SL_PROCESS;
   t->pid = pid;
   SLang_push_integer (pid);
   return ((int) (t - SL_File_Table));
}
   
   
#endif  /* HAS_SUBPROCESSES */
