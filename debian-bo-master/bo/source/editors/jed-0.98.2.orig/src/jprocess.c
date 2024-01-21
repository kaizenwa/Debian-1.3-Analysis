/* -*- mode: C; mode: fold; -*- */
#include "config.h"
#include "jed-feat.h"

#include <stdio.h>

#if JED_HAS_SUBPROCESSES
/* Everything else here is in this '#if' */

/*{{{ Include Files */

#include <string.h>
#include <errno.h>
#include <signal.h>

#include <slang.h>

#include "jdmacros.h"

#ifdef HAVE_UNISTD_H
# include <unistd.h>
# include <fcntl.h>
#endif

#include <sys/types.h>
#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif


#include "buffer.h"
#include "ins.h"
#include "ledit.h"
#include "misc.h"
#include "jprocess.h"
#include "paste.h"
#include "sig.h"

/*}}}*/

int Num_Subprocesses;
int Max_Subprocess_FD;
int Subprocess_Read_fds [MAX_PROCESSES][2];   /* 0 is actual fd, 1 is our rep */

volatile int Child_Status_Changed_Flag;/* if this is non-zero, editor
					* should call the appropriate
					* function below to call slang
					* handlers.
					*/
typedef struct /*{{{*/
{
   int flags;			       /* This is zero if the process is gone
					* and the status is nolonger avail */
#define PROCESS_RUNNING		1
#define PROCESS_STOPPED		2
#define PROCESS_ALIVE		3
#define PROCESS_EXITED		4
#define PROCESS_SIGNALLED	8
   int return_status;		       /* This value depends on the flags */
   
   int status_changed;		       /* non-zero if status changed. */
   int rd, wd;			       /* read/write descriptors */
   int pid;			       /* real process pid */
   int output_type;
#define PROCESS_USE_BUFFER	1
#define PROCESS_USE_SLANG	2
#define PROCESS_SAVE_POINT	4
#define PROCESS_AT_POINT	8
   Buffer *buffer;		       /* buffer associated with process */
   SLang_Name_Type *slang_fun;	       /* function to pass output to */
   SLuser_Object_Type *umark;	       /* marks point of last output */
   
   SLang_Name_Type *status_change_fun; /* call this if process status changes 
					* The function should be declared like
					* define fun (pid, flags);
					* The flags parameter corresponds to
					* the flags field in this struct and 
					* the pid is NOT the pid of this struct
					*/
} Process_Type;

/*}}}*/


static Process_Type Processes[MAX_PROCESSES];

static Process_Type *get_process (int fd) /*{{{*/
{
   Process_Type *p;
   
   if ((fd >= 0) && (fd < MAX_PROCESSES)
       && (p = &Processes[fd], p->flags != 0)) return p;
   
   msg_error ("process does not exist.");
   return NULL;
}

/*}}}*/

static void call_slang_status_change_hook (Process_Type *p) /*{{{*/
{
   Buffer *cbuf = CBuf;
   if ((p->status_change_fun == NULL) || (p->buffer == NULL)) return;
   
   cbuf->locked++;
   switch_to_buffer (p->buffer);
   SLang_push_integer ((int) (p - Processes));
   SLang_push_integer (p->flags);
   SLexecute_function (p->status_change_fun);
   touch_screen ();
   if (CBuf != cbuf) switch_to_buffer (cbuf);
   cbuf->locked--;
}

/*}}}*/

#if 1
int jed_signal_process (int *fd, int *sig) /*{{{*/
{
   Process_Type *p;
   if (NULL == (p = get_process (*fd))) return -1;

   kill (p->pid, *sig);
   return 0;
}

/*}}}*/
#endif

static void close_rd_and_wd (Process_Type *p) /*{{{*/
{   
   if (p->rd != -1) 
     {
	close (p->rd);
	p->rd = -1;
     }
   if (p->wd != -1)
     {
	close (p->wd); 
	p->wd = -1;
     }
}

/*}}}*/


/* This routine is called to clean up after the process has exited.  
 * After getting the exit status, we call a slang hook and if the
 * process is dead, adjust the process arrays to delete the process.
 */
static void get_process_status (Process_Type *p) /*{{{*/
{
   int i;
   int fd, slfd;
   
   /* Call slang to let it know what happened.  Do it first before we 
    * really shut it down to give the hook a chance to query the state of
    * it before it returns.
    */
   call_slang_status_change_hook (p);
   if (p->flags & PROCESS_ALIVE) return;

   /* Process is dead.  So perform clean up. */
   close_rd_and_wd (p);
   
   if (p->buffer != NULL) p->buffer->subprocess = 0;
   slfd = (int) (p - Processes);
   
   fd = Subprocess_Read_fds [slfd][0];
   
   p->flags = 0;
   if (p->umark != NULL) jed_free_user_object_mark (p->umark);
   /* Adjust the array of read descriptors */
   i = 0;
   while (i < Num_Subprocesses)
     {
	if (Subprocess_Read_fds[i][0] == fd)
	  {
	     while (i < Num_Subprocesses - 1)
	       {
		  Subprocess_Read_fds[i][0] = Subprocess_Read_fds[i + 1][0];
		  Subprocess_Read_fds[i][1] = Subprocess_Read_fds[i + 1][1];
		  i++;
	       }
	     break;
	  }
	i++;
     }
   
   Num_Subprocesses--;
   
   if (Max_Subprocess_FD == fd)
     {
	i = 0;
	fd = -1;
	while (i < Num_Subprocesses)
	  {
	     if (Subprocess_Read_fds[i][0] > fd) fd = Subprocess_Read_fds[i][0];
	     i++;
	  }
	Max_Subprocess_FD = fd;
     }
}

/*}}}*/

int jed_close_process (int *fd) /*{{{*/
{
   Process_Type *p;
   
   if (NULL == (p = get_process (*fd))) return -1;
   
   close_rd_and_wd (p);
   
   kill (-p->pid, SIGINT);
   
   /* This is probably a bad idea.  It is better to check to see if it still 
    * around and the set a flag indicating that the user wants it killed.
    */
   
   /* Did we kill it? Make sure. */
   kill (-p->pid, SIGKILL);

   if (p->buffer != NULL) p->buffer->subprocess = 0;
   
   /* This next function wraps things up --- no need to.  Let handler do it. */
   /* get_process_status (p); */
   return 0;
}

/*}}}*/

void jed_kill_process (int fd) /*{{{*/
{
   /* This function is called when the buffer is going to be destroyed */
   Processes[fd].buffer = NULL;
   jed_close_process (&fd);
}

/*}}}*/

void jed_get_child_status (void) /*{{{*/
{
   Process_Type *p, *pmin;
   
   Child_Status_Changed_Flag--;
   pmin = Processes; p = pmin + Num_Subprocesses;
   
   get_process_input (&Number_Zero);

   while (p > pmin)
     {
	p--;
	if (p->status_changed)
	  {
	     p->status_changed--;
	     get_process_status (p);
	  }
     }
}

/*}}}*/

static void child_signal_handler (int sig) /*{{{*/
{
   int status;
   int return_status;
   int pid;
   Process_Type *p, *pmax;
   int save_errno = errno;
   
   (void) sig;

   while (1)
     {
	pid = (int) waitpid (-1, &status, WNOHANG | WUNTRACED);
	if (pid == -1) 
	  {
	     if (errno == ECHILD) break;
	     continue;
	  }
	
	if (pid == 0) break;
	
	return_status = 0;
	
	if (WIFEXITED (status))
	  {
	     return_status = WEXITSTATUS (status);
	     status = PROCESS_EXITED;
	  }
	else if (WIFSIGNALED (status))
	  {
	     status = PROCESS_SIGNALLED;
	     return_status = WTERMSIG (status);
	  }
	else if (WIFSTOPPED (status))
	  {
	     status = PROCESS_STOPPED;
	  }
	/* What else?? */
	
	p = Processes;
	pmax = p + Num_Subprocesses;
	while (p < pmax)
	  {
	     if (p->pid == pid)
	       {
		  p->flags = status;
		  p->status_changed++;
		  p->return_status = return_status;
		  break;
	       }
	     p++;
	  }
     }
   SLsignal_intr (SIGCHLD, child_signal_handler);
   errno = save_errno;
   Child_Status_Changed_Flag++;
}

/*}}}*/

static int open_process (char *pgm, char **argv) /*{{{*/
{
   int spfd, val;
   int pd;
   int fds0[2], fds1[2];
   int pid, i;
   Process_Type *p;
   SLuser_Object_Type *uo;
   
   pd = 0; while ((pd < MAX_PROCESSES) && Processes[pd].flags) pd++;
   if (pd == MAX_PROCESSES) return -1;
   p = &Processes[pd];

   SLMEMSET ((char *) p, 0, sizeof (Process_Type));
   
   if (0 == pipe (fds0))
     {
	if (-1 == pipe (fds1))
	  {
	     close (fds0[0]);
	     close (fds0[1]);
	     return -1;
	  }
     }
   else return -1;
   
   if (NULL == (uo = jed_make_user_object_mark (SLANG_IVARIABLE))) return -1;
   
   SLsignal_intr (SIGCHLD, child_signal_handler);

   if ((pid = fork ()) < 0)
     {
	close (fds1[0]); close (fds1[1]);
	close (fds0[0]); close (fds0[1]);
	p->flags = 0;
	jed_free_user_object_mark (uo);
	return -1;
     }
   p->pid = pid;
   
   /* Make the child its own process group leader.  Do it here too because
    * we are not sure which one will run first.  We have to do this because
    * if not, a ^G will be sent to ALL child subprocesses possibly killing 
    * them unless they catch the signal.  This call means that the INTR signal
    * will not be sent to any child processes sent by this fork.
    */
   setpgid(pid, pid);

   
   if (pid == 0)
     {
	char ch;
	/* child code */
	/* Wait here for the parent to initialize its structures */
	/* This will block */
	read (fds0[0], &ch, 1);  
	
	for (i = 0; i < 32; i++) SLsignal_intr (i, SIG_DFL);
	
	close (fds0[1]);	       /* close write end of 0 */
	close (fds1[0]);	       /* close read end of 1 */
	
	if ((dup2(fds0[0], 0) < 0)     /* stdin */
	    || (dup2 (fds1[1], 1) < 0) /* stdout */
	    || (dup2 (fds1[1], 2) < 0)) /* stderr */
	  {
	     fprintf (stderr, "Dups failed!\n");
	     exit (-1);
	  }
	
	
	if (execvp (pgm, argv) < 0)
	  {
	     fprintf (stderr, "exec failed!\n");
	     exit (-1);
	  }
     }
   
   /* parent */
   close (fds0[0]);		       /* close read of child read */
   close (fds1[1]);		       /* close write of child write */

   p->flags = PROCESS_RUNNING;
   spfd = p->rd = fds1[0];
   p->wd = fds0[1];
   
   Subprocess_Read_fds[Num_Subprocesses][0] = spfd;
   Subprocess_Read_fds[Num_Subprocesses][1] = pd;
   if (spfd > Max_Subprocess_FD) Max_Subprocess_FD = spfd;
   Num_Subprocesses += 1;
   
   val = fcntl (spfd, F_GETFL, 0);
   val |= O_NONBLOCK;
   fcntl (spfd, F_SETFL, val);

   CBuf->subprocess = pd + 1;

   /* Processing options */
   p->buffer = CBuf;
   p->output_type = PROCESS_USE_BUFFER;
   p->umark = uo;
   
   /* Tell child it is ok to go. */
   write (p->wd, "&", 1);
   
   return pd;
}

/*}}}*/


/* This function is only called when we are reading characters from the 
 * keyboard.  Keyboard input has the highest priority and this is called only
 * if there is no input ready.
 */
void read_process_input (int fd) /*{{{*/
{
   unsigned char buf[513];	       /* last byte for 0 char */
   int n;
   Buffer *b = CBuf, *pbuf;
   Process_Type *p;
   int otype, total;
   
   /* Should never happen */
   if (NULL == (p = get_process (fd))) 
     {
	return;
     }
   
   otype = p->output_type;
   pbuf = p->buffer;
   
   if (pbuf != NULL)
     {
	switch_to_buffer (pbuf);
	pbuf->locked++;
     }
   
   total = 0;
   if (otype & PROCESS_SAVE_POINT) push_spot ();
   while ((n = read (p->rd, buf, 512)) > 0)
     {
	total += n;
	if (p->buffer == NULL) continue;
	
	if (otype & PROCESS_USE_BUFFER) 
	  {
	     if (0 == (otype & PROCESS_AT_POINT)) eob ();
	     ins_chars (buf, n);
	     jed_move_user_object_mark (p->umark);
	  }
	else if (otype == PROCESS_USE_SLANG)
	  {
	     buf[n] = 0;
	     SLang_push_string ((char *) buf);
	     SLang_push_integer ((int) (p - Processes));
	     SLexecute_function (p->slang_fun);    /* function to pass output to */
	  }
     }
   
   if (otype & PROCESS_SAVE_POINT) pop_spot ();
   else if (otype & PROCESS_USE_BUFFER) move_window_marks (0);
   
   if (p->buffer != NULL)
     {
	if (b != CBuf) switch_to_buffer (b);
	pbuf->locked--;
     }
   if (total) touch_screen ();
}

/*}}}*/

int jed_send_process (int *fd, char *str) /*{{{*/
{
   Process_Type *p = get_process (*fd);
   if ((p == NULL) || (p->wd == -1)) return -1;
   write (p->wd, str, strlen(str));
   return 0;
}

/*}}}*/

void jed_send_process_eof (int *fd) /*{{{*/
{
   Process_Type *p = get_process (*fd);
   if (p == NULL) return;
   
   if (p->wd != -1) close (p->wd);
   p->wd = -1;
}

/*}}}*/

void jed_set_process (int *pd, char *what, char *s) /*{{{*/
{
   Process_Type *p;
   SLang_Name_Type *f;
   
   if (NULL == (p = get_process (*pd))) return;
   if (!strcmp (what, "output"))
     {
	if (*s == '.') p->output_type = PROCESS_AT_POINT | PROCESS_USE_BUFFER;
	else if (*s == '@') p->output_type = PROCESS_SAVE_POINT | PROCESS_USE_BUFFER;
	else if (*s && (NULL != (f = SLang_get_function (s))))
	  {
	     p->output_type = PROCESS_USE_SLANG;
	     p->slang_fun = f;
	  }
	else p->output_type = PROCESS_USE_BUFFER;
     }
   else if (!strcmp (what, "signal"))
     {
	if (*s && (NULL != (f = SLang_get_function (s))))
	  {
	     p->status_change_fun = f;
	  }
     }
}

/*}}}*/

void jed_get_process_mark (int *fd) /*{{{*/
{
   Process_Type *p;
   if (NULL == (p = get_process (*fd))) return;
   
   SLang_push_user_object (p->umark);
}

/*}}}*/

int jed_open_process (int *np) /*{{{*/
{
   int fd = -1;
   char *argv[502];
   int n = *np;

   if (CBuf->subprocess)
     {
	msg_error ("There is already a process attached to this buffer.");
	return -1;
     }
   
   if ((n > 500) || (n < 0))
     {
	msg_error ("Arguments out of range.");
	return -1;
     }
   
   n++;				       /* for argv0 since *np does not include
					* it. 
					*/
   argv[n] = NULL;
   while (n--)
     {
	if (SLpop_string (&argv[n]))
	  {
	     n++;
	     goto free_return;
	  }
     }
   n = 0;
   
   if ((fd = open_process(argv[0], argv)) < 0)
     {
	msg_error ("Unable to open process.");
     }
   
   /* free up the argument strings */
   free_return:
   
   while (n <= *np)
     {
	SLFREE (argv[n]);
	n++;
     }
   
   return fd;
}

/*}}}*/

void jed_block_child_signal (int block) /*{{{*/
{
   static sigset_t new_mask, old_mask;
   
   if (block)
     {
	sigemptyset (&new_mask);
	sigaddset (&new_mask, SIGCHLD);
	(void) sigprocmask (SIG_BLOCK, &new_mask, &old_mask);
	return;
     }
   
   (void) sigprocmask (SIG_SETMASK, &old_mask, NULL);
}

/*}}}*/

/* Jed only calls these in pairs so that this should be fine. */
FILE *jed_popen (char *cmd, char *type) /*{{{*/
{
   FILE *pp;
   
   jed_block_child_signal (1);
   pp = popen (cmd, type);
   if (pp == NULL)
     jed_block_child_signal (0);
   return pp;
}

/*}}}*/

int jed_pclose (FILE *fp) /*{{{*/
{
   int ret;
   
   if (fp == NULL)
     return -1;
   
   ret = pclose (fp);
   jed_block_child_signal (0);
   
   return ret;
}

/*}}}*/

#if 0
/* These are my versions of popen/pclose.  For some reason, the popen/pclose 
 * do not work on SunOS when there are subprocesses.  I think it has 
 * something to do with the way pclose is waiting.
 * See Steven's book for more information.
 */
#ifndef OPEN_MAX
#define OPEN_MAX 256
#endif
static pid_t Popen_Child_Pids[OPEN_MAX];
FILE *jed_popen(char *cmd, char *type) /*{{{*/
{
   int i, pfd[2], fd;
   pid_t pid;
   FILE	*fp;

   if (((*type != 'r') && (*type != 'w')) || (*(type + 1) != 0))
     {
	errno = EINVAL;		/* required by POSIX.2 */
	return(NULL);
     }

   if (pipe(pfd) < 0) return(NULL);	/* errno set by pipe() or fork() */
   if ((pid = fork()) < 0) return(NULL);
   
   if (pid == 0) 
     {				       /* child */
	if (*type == 'r') 
	  {
	     close(pfd[0]);
	     if (pfd[1] != 1) dup2(pfd[1], 1);
	     close(pfd[1]);
	  }
	else
	  {
	     close(pfd[1]);
	     if (pfd[0] != 0)
	       {
		  dup2(pfd[0], STDIN_FILENO);
		  close(pfd[0]);
	       }
	  }
	
	/* POSIX requires that all streams open by previous popen
	 * be closed.
	 */
	for (i = 0; i < OPEN_MAX; i++)
	  {
	     if (Popen_Child_Pids[i] > 0) close(i);
	  }

	execl("/bin/sh", "sh", "-c", cmd, (char *) 0);
	_exit(127);
     }
   
   /* parent */
   if (*type == 'r') 
     {
	close(pfd[1]);
	if (NULL == (fp = fdopen(pfd[0], type)))
	  return(NULL);
     } 
   else 
     {
	close(pfd[0]);
	if (NULL == (fp = fdopen(pfd[1], type)))
	  return(NULL);
     }
   
   fd = fileno (fp);
   if (fd >= OPEN_MAX)
     {
#ifdef EMFILE
	errno = EMFILE;
#endif
	fclose (fp);
	return NULL;
     }
   Popen_Child_Pids [fd] = pid;
   return(fp);
}

/*}}}*/
int jed_pclose(FILE *fp) /*{{{*/
{
   int	fd, stat;
   pid_t pid;
   int ret;

   fd = fileno(fp);
   if ((fd >= OPEN_MAX) ||
       (0 == (pid = Popen_Child_Pids[fd]))) 
     return -1;

   Popen_Child_Pids [fd] = 0;
   
   if (fclose(fp) == EOF) return(-1);

   /* This is the part that the SunOS pclose was apparantly screwing up. */
   while (pid != (ret = waitpid(pid, &stat, 0)))
     {
	if ((ret == -1) && (errno != EINTR))
	  return -1;
     }
   
   ret = WEXITSTATUS(stat);
   if (WIFEXITED (stat))
     return ret;
   
   return -1;
}

/*}}}*/
#endif

#endif 				       /* JED_HAS_SUBPROCESSES */
