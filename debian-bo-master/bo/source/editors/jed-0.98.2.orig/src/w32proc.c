#include "config.h"
#include "jed-feat.h"

#include <stdio.h>
#include <slang.h>

#include "jdmacros.h"

#include <stdlib.h>

#include "jprocess.h"
#include "buffer.h"
#include "ins.h"
#include "ledit.h"
#include "misc.h"
#include "paste.h"

int Num_Subprocesses;
LONG Has_Subprocess_Input;
CRITICAL_SECTION Critical_Section;

volatile int Child_Status_Changed_Flag;/* if this is non-zero, editor
					* should call the appropriate
					* function below to call slang
					* handlers.
					*/

typedef struct 
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
   HANDLE rd, wd;		       /* read/write handles */
   HANDLE hprocess;		       /* real process handle */
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
   HANDLE input_event;
   unsigned char *input_buf;
   int input_bufsize;
} Process_Type;


static Process_Type Processes[MAX_PROCESSES];

static Process_Type *get_process (int fd)
{
   Process_Type *p;

   if ((fd >= 0) && (fd < MAX_PROCESSES)
       && (p = &Processes[fd], p->flags != 0)) return p;

   msg_error ("process does not exist.");
   return NULL;
}


static void call_slang_status_change_hook (Process_Type *p)
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

/* This routine is called to clean up after the process has exited.
 * After getting the exit status, we call a slang hook and if the
 * process is dead, adjust the process arrays to delete the process.
 */

static void get_process_status (Process_Type *p)
{
   int i;
   /* Call slang to let it know what happened.  Do it first before we
    * really shut it down to give the hook a chance to query the state of
    * it before it returns.
    */
   call_slang_status_change_hook (p);
   if (p->flags & PROCESS_ALIVE) return;

   /* Process is dead.  So perform clean up. */

   CloseHandle(Input_Events[(int)(p - Processes)]);
   CloseHandle(p->rd);
   CloseHandle(p->wd);
   if (p->input_buf) SLFREE(p->input_buf);
   if (p->buffer != NULL) p->buffer->subprocess = 0;

   p->flags = 0;
   if (p->umark != NULL) jed_free_user_object_mark (p->umark);

   /* Adjust the array of read descriptors */
   i = 0;
   while (i < Num_Subprocesses)
     {
	if (Input_Events[i] == p->input_event)
	  {
	     while (i < Num_Subprocesses - 1)
	       {
		  Input_Events[i] = Input_Events[i + 1];
		  i++;
	       }
	     break;
	  }
	i++;
     }
   
   Num_Subprocesses--;
}

int jed_close_process (int *fd)
{
   Process_Type *p;

   if (NULL == (p = get_process (*fd))) return -1;

   TerminateProcess(p->hprocess, 0);

   if (p->buffer != NULL) p->buffer->subprocess = 0;

   /* This next function wraps things up --- no need to.  Let handler do it. */
   /* get_process_status (p); */
   return 0;
}

void jed_kill_process (int fd)
{
   /* This function is called when the buffer is going to be destroyed */
   Processes[fd].buffer = NULL;
   jed_close_process (&fd);
}

void jed_get_child_status (void)
{
   Process_Type *p, *pmin;
   
   Child_Status_Changed_Flag--;
   pmin = Processes; p = pmin + Num_Subprocesses;
   
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

static DWORD thread_func(int fd)
{
   char buf[513];	       /* last byte for 0 char */
   DWORD n;
   int bufsize;
   Process_Type *p = get_process(fd);

   while(ReadFile(p->rd, buf, 512, &n, NULL))
      {
	 EnterCriticalSection(&Critical_Section);
	 bufsize = p->input_bufsize + n;
	 p->input_buf = SLREALLOC(p->input_buf, bufsize + 1);
	 MEMCPY((char *)&(p->input_buf[p->input_bufsize]), buf, n);
	 p->input_bufsize = bufsize;
	 SetEvent(Input_Events[fd]);
	 LeaveCriticalSection(&Critical_Section);
      }
   
   p->flags = PROCESS_EXITED;
   p->status_changed++;
   Child_Status_Changed_Flag++;
   return 0;
}

void read_process_input(int fd)
{
   Buffer *b = CBuf, *pbuf;
   int otype;
   Process_Type *p;

   if (NULL == (p = get_process (fd))) return;
   
   EnterCriticalSection(&Critical_Section);
   
   otype = p->output_type;
   pbuf = p->buffer;
   
   if (pbuf != NULL && (otype != PROCESS_USE_SLANG))
      {
	 switch_to_buffer (pbuf);
	 pbuf->locked++;
      }
   
   if (otype & PROCESS_SAVE_POINT) push_spot ();
   
   if (otype & PROCESS_USE_BUFFER) 
      {
	 if (0 == (otype & PROCESS_AT_POINT)) eob ();
	 ins_chars (p->input_buf, p->input_bufsize);
	 jed_move_user_object_mark (p->umark);
      }
   else if (otype == PROCESS_USE_SLANG)
      {
	 p->input_buf[p->input_bufsize] = 0;
	 SLang_push_string ((char *) p->input_buf);
	 SLang_push_integer ((int) (p - Processes));
	 SLexecute_function (p->slang_fun);    /* function to pass output to */
      }
   
   if (otype & PROCESS_SAVE_POINT) 
     pop_spot ();
   else 
     if (otype & PROCESS_USE_BUFFER) 
        move_window_marks (0);
	 
   if ((p->buffer != NULL) && (otype != PROCESS_USE_SLANG))
      {
	 if (b != CBuf) switch_to_buffer (b);
	 pbuf->locked--;
      }

   SLFREE(p->input_buf);
   p->input_buf = NULL;
   p->input_bufsize = 0;
   ResetEvent(p->input_event);
   LeaveCriticalSection(&Critical_Section);
   touch_screen ();
}
   

static int open_process (char *pgm, char **argv)
{
   int pd;
   HANDLE fds0[2], fds1[2];
   Process_Type *p;
   char **arg, *cmd_line;
   SLuser_Object_Type *uo;
   STARTUPINFO si;
   PROCESS_INFORMATION pi;
   SECURITY_ATTRIBUTES sa;
   DWORD d, id_thread;
   
   
   pd = 0; while ((pd < MAX_PROCESSES) && Processes[pd].flags) pd++;
   if (pd == MAX_PROCESSES) return -1;
   p = &Processes[pd];
   
   MEMSET ((char *) p, 0, sizeof (Process_Type));
   
   sa.nLength = sizeof(SECURITY_ATTRIBUTES);
   sa.bInheritHandle = TRUE;
   sa.lpSecurityDescriptor = NULL;
   
   if (CreatePipe(&fds0[0], &fds0[1], &sa, 0))
      {
	 if (!CreatePipe(&fds1[0], &fds1[1], &sa, 0))
	    {
	       CloseHandle(fds0[0]);
	       CloseHandle(fds0[1]);
	       return -1;
	    }
      }
   else
     return -1;
   
   if (NULL == (uo = jed_make_user_object_mark (SLANG_IVARIABLE))) return -1;
   
   si.cb = sizeof(STARTUPINFO);
   si.lpReserved = NULL;
   si.lpReserved2 = NULL;
   si.cbReserved2 = 0;
   si.lpDesktop = NULL;
   si.lpTitle = NULL;
   si.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW; 
   si.wShowWindow = SW_MINIMIZE;
   si.hStdInput = fds0[0];
   si.hStdOutput = fds1[1]; 
   si.hStdError = fds1[1];
   
   d = strlen(pgm) + 1;
   arg = argv;
   while (*arg != NULL)
      {
	 d += strlen(*arg) + 1;
	 arg++;
      }

   if (NULL == (cmd_line = SLMALLOC(d))) return -1;
   
   cmd_line[0] = 0;

   arg = argv;
   while (*arg != NULL)
      {
	 strcat(cmd_line, *arg);
	 arg++;
      }

   if (CreateProcess(NULL, cmd_line, NULL, NULL, TRUE, CREATE_NEW_PROCESS_GROUP | CREATE_NEW_CONSOLE, NULL, NULL, &si, &pi))
      {
	 CloseHandle(fds0[0]);
	 CloseHandle(fds1[1]);

	 p->flags = PROCESS_RUNNING;
	 p->rd = fds1[0];
	 p->wd = fds0[1];
	 p->hprocess = pi.hProcess;
	 
	 Num_Subprocesses += 1;
	 
	 CBuf->subprocess = pd + 1;

	 /* Processing options */
	 p->buffer = CBuf;
	 p->output_type = PROCESS_USE_BUFFER;
	 p->umark = uo;
	 p->input_bufsize = 0;
	 p->input_buf = NULL;

	 p->input_event = CreateEvent(NULL, TRUE, FALSE, NULL);
	 Input_Events[pd] = p->input_event;
	 CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)thread_func, (LPVOID) pd, 0, &id_thread);
      }
   else
      {
	 msg_error("exec failed!\n");
	 CloseHandle(fds0[0]);
	 CloseHandle(fds0[1]);
	 CloseHandle(fds1[0]);
	 CloseHandle(fds1[1]);
	 
	 jed_free_user_object_mark (uo);
	 return -1;
      }
   
   return pd;
}

int jed_send_process (int *fd, char *str)
{
   DWORD d;
   
   Process_Type *p = get_process (*fd);
   if ((p == NULL) || (p->wd == NULL)) return -1;
   WriteFile(p->wd, str, strlen(str), &d, NULL);
   return 0;
}

void jed_send_process_eof (int *fd)
{
   Process_Type *p = get_process (*fd);
   if (p == NULL) return;

   if (p->rd != NULL) CloseHandle(p->rd);
   p->wd = NULL;
}

void jed_set_process (int *pd, char *what, char *s)
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

void jed_get_process_mark (int *fd)
{
   Process_Type *p;
   if (NULL == (p = get_process (*fd))) return;
   
   SLang_push_user_object (p->umark);
}

int jed_open_process (int *np)
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

