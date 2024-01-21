/* fd manager.
   Copyright 1994, 1995 Tristan Gingold
		  Written December 1994 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
/* NOTE: this code is not really interesting...
   It only checks the user doesn't try to use handler reserved by Checker.
   It can also display the current status of each file desciptor.  */

#include <limits.h>
#include <unistd.h>
#include <fcntl.h>
#include "checker.h"
#include "errlist.h"
#include "message.h"

/* Values for fd_record.state  */
#define FD_FREE 0	/* This fd is available.  */
#define FD_OPEN 1	/*         has been opened.  */
#define FD_HERITED 2	/*         has been herited through exec(2).  */
#define FD_RESERVED 3	/*         is reserved by Checker for its own use.  */
#define FD_CLOSE 4	/*         has been closed.  */

/* There is one STRUCT FD_RECORD by file descriptor.  */
struct fd_record
{
 PTR history[HISTORY_DEPTH];	/* The history for the fd (procs which opened/closed it).  */
 int state;		/* See FD_* above.  */
 int mode;		/* ie fcntl (fd, F_FETFL).  */
 char *comment;		/* a comment, for fd_alloc_reserved ().  */
};

/* Number of file descriptor.  */
static int max_open;

/* The array of structure.  */
static struct fd_record *fds;

/* Initialize fd_record.  It mainly detects the herited fds (already open
   descriptor when ran.  This function must *not* use sys_malloc.  */
void
init_fds (void)
{
 int i;
 int mode;

#ifdef CHKR_OPEN_MAX
 max_open = CHKR_OPEN_MAX;
#else
# ifdef _SC_OPEN_MAX
 max_open = sysconf (_SC_OPEN_MAX);
# else
#  ifdef OPEN_MAX
 max_open = OPEN_MAX;
#  else
#   ifdef _POSIX_OPEN_MAX
 max_open = _POSIX_OPEN_MAX;
#   else
 max_open = 32;
#   endif
#  endif
# endif
#endif

 fds = (struct fd_record *) sys_malloc (max_open * sizeof (struct fd_record)); 
 for (i = 0; i < max_open; i++)
   {
     mode = fcntl(i, F_GETFL);
     if (mode == -1)
       fds[i].state = FD_FREE;
     else
       {
         fds[i].state = FD_HERITED;
         fds[i].mode = mode;
       }
     fds[i].history[0] = (PTR)0;
     fds[i].comment = "";
   }
}

/* This function is called by os-syscall.c, when read(), write(), fcntl() ...
   are called.  This means that a fd is used.  It only checks that this fd
   is not reserved by Checker.  */
int
fd_used_by_prog (int fd)
{
  if (fd < 0 || fd > max_open)
    {
      chkr_perror (M_I_EFD_SC_ET);
      return 0;
    }

  /* If Checker has reserved this fd, this is not good... */
  if (fds[fd].state == FD_RESERVED)
    {
      chkr_perror (M_I_BFD_SC_ET);
      return 0;
    }
  return 1;
}

/* This is called by linux-syscall.c, when the system has allocated a new fd
   (ie when the user calls open(), create(), socket()...  */
int
fd_returned_by_system (int fd)
{
  if (fd < 0 || fd > max_open)
    {
      chkr_perror (M_I_EFD_SC_ET);
      return 0;
    }

  /* If Checker has reserved this fd, there is a problem... */
  if (fds[fd].state == FD_RESERVED)
    {
      chkr_perror (M_I_SFD_SC_ET);
      return 0;
    }
  else
    {
      /* Set the fields. */
      fds[fd].state = FD_OPEN;
      fds[fd].mode = fcntl (fd, F_GETFL);
#ifdef CHKR_SAVESTACK
      chkr_get_history (fds[fd].history, 0, HISTORY_DEPTH);
#endif
    }
  return 1;
}

/* This is called by linux-syscall.c when the user calls close().  */
int
fd_closed (int fd)
{
  if (fd < 0 || fd > max_open)
    {
      chkr_perror (M_I_EFD_SC_ET);
      return 0;
    }

  fds[fd].state = FD_CLOSE;
  fds[fd].history[0] = (PTR)0;
#ifdef CHKR_SAVESTACK
  chkr_get_history (fds[fd].history, 0, HISTORY_DEPTH);
#endif
  return 1;
}

/* This is called when the user calls dup() or dup2().  */
int
fd_duped (int fdres, int fdd)
{
  if (fdres < 0 || fdres > max_open
      || fdd < 0 || fdd > max_open)
    {
      chkr_perror (M_I_EFD_SC_ET);
      return 0;
    }

  if (fds[fdres].state == FD_RESERVED)
    chkr_perror (M_I_SFD_SC_ET);
  else
    {
      fds[fdres].state = FD_OPEN;
      fds[fdres].mode = fcntl (fdres, F_GETFL);
#ifdef CHKR_SAVESTACK
      chkr_get_history (fds[fdres].history, 0, HISTORY_DEPTH);
#endif
    }
  return 1;
}

/* This is used internally by Checker to reserved a fd.  */
int
fd_alloc_reserved (char *comment)
{
 int i;
 /* At first look for a never allocated fd.  Reserved fds are a hot point, 
  *  since the user must not be troubled by these fds.  */
 for (i = max_open - 1; i >= 0; i--)
   if (fds[i].state == FD_FREE)
     {
       fds[i].state = FD_RESERVED;
       fds[i].comment = comment; 
       return i;
     }
 /* Then for a closed fd. */
 for (i = max_open - 1; i >= 0; i--)
   if (fds[i].state == FD_CLOSE)
     {
       fds[i].state = FD_RESERVED;
       fds[i].comment = comment; 
       return i;
     }
  return -1;
}

/* Release a reserved fd.  */
void
fd_free_reserved (int fd)
{
  fds[fd].state = FD_FREE;
}

/* Disp information about the fds.  */
void
__chkr_disp_fd (void)
{
 int i;
#ifdef CHKR_SAVESTACK
 int j;
 chkr_load_symtab ();
#endif

 chkr_report (M_C_MES_CK_ET);
 chkr_printf ("| fd  | state    | mode\n");
 for (i = 0; i < max_open; i++)
   {
     if (fds[i].state != FD_FREE)
       {
         chkr_printf ("| %03d | ", i);
         switch (fds[i].state)
           {
         case FD_OPEN:
           chkr_printf ("open     | ");
           break;
         case FD_HERITED:
           chkr_printf ("herited  | ");
           break;
         case FD_RESERVED:
           chkr_printf ("reserved (`%s') | ", fds[i].comment);
           break;
         default:
           chkr_printf ("<unknown>| ");
           }
         if (fds[i].state == FD_OPEN || fds[i].state == FD_HERITED)
           {
             if (fds[i].mode & O_RDWR)
               chkr_printf ("RDWR ");
             else if (fds[i].mode & O_RDONLY)
               chkr_printf ("RDONLY ");
             else if (fds[i].mode & O_WRONLY)
               chkr_printf ("WRONLY ");
             if (fds[i].mode & O_NONBLOCK)
               chkr_printf ("NONBLOCK ");
             if (fds[i].mode & O_APPEND)
               chkr_printf ("APPEND ");
             if (fds[i].mode & O_SYNC)
               chkr_printf ("SYNC ");
           }
         chkr_printf ("\n");
#ifdef CHKR_SAVESTACK
         if (fds[i].state == FD_OPEN || fds[i].state == FD_CLOSE)
           {
             if (fds[i].state == FD_OPEN)
               chkr_printf (M_FILE_OPENED_BY);
             else
               chkr_printf (M_FILE_CLOSED_BY);
             for (j = 0; fds[i].history[j]; j++)
               chkr_show_addr (fds[i].history[j]);
           }
#endif
       }
   }
}
