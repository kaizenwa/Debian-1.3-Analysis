/* fork for WIN32.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include "winsup.h"

/* Timeout to wait for child to start, parent to init child, etc.  */
/* FIXME: Once things stabilize, bump up to a few minutes.  */
#define FORK_WAIT_TIMEOUT (120 * 1000 /* 120 seconds */)

/* Error return code from WaitForSingleObject.  */
#define WAIT_ERROR_RC 0xffffffff

/* Mutexes and events necessary for child startup.  */
static HANDLE fork_mutex;
static HANDLE forkee_stopped, forker_stopped;

/* Initialize the fork mechanism.  */

void
fork_init ()
{
  SECURITY_ATTRIBUTES sa;
  sa.nLength = sizeof (sa);
  sa.lpSecurityDescriptor = 0;
  sa.bInheritHandle = 0;

  fork_mutex = CreateMutexA (&sa, FALSE, "cygwin.fork-mutex");

  forker_stopped = CreateEventA (&sa, TRUE, TRUE, "cygwin.forker-stopped");
  forkee_stopped = CreateEventA (&sa, TRUE, TRUE, "cygwin.forkee-stopped");

  /* If we didn't obtain all the resources we need to fork, allow the program
     to continue, but record the fact that fork won't work.  */
  if (fork_mutex == NULL
      || forker_stopped == NULL
      || forkee_stopped == NULL)
    {
      system_printf ("fork_init: unable to allocate fork() resources.\n");
      system_printf ("fork_init: fork() disabled.\n");
      fork_mutex = NULL;
    }
}

/* Undo what fork_init does.

   If a task is shutting down, this isn't really necessary, but maybe one day
   we'll want to allow cygwin.dll to be attached to and detached from, so we
   should provide a clean interface to release all resources we obtain.  */

void
fork_terminate ()
{
  CloseHandle (forker_stopped);
  CloseHandle (forkee_stopped);
  CloseHandle (fork_mutex);
}

/* Add .exe to PROG and see if that exists.  If not, return PROG
   (converted from posix to win32 rules if necessary).
   The result is always copied into BUF.  */

static const char *
perhaps_suffix (const char *prog, char *buf)
{
  int l = strlen (prog);

  strcpy (buf, path_conv (prog).get_win32 ());
  int l2 = strlen (buf);

  if (l < 5 || strcasecmp (prog + l - 4, ".exe"))
    {
      int res;

      strcat (buf, ".exe");
      res = GetFileAttributesA (buf);
      if (res > 0 && !(res & FILE_ATTRIBUTE_DIRECTORY))
	return buf;
    }

  /* .exe not found */
  buf[l2] = 0;
  return buf;
}

static const char *
find_exec_1 (const char *name, char *buf)
{
  int r, len;
  char tmp[MAX_PATH];

  if (strchr (name, '/') || strchr (name, '\\'))
    return perhaps_suffix (name, buf);

  if (isalpha (name[0]) && name[1] == ':')
    return perhaps_suffix (name, buf);

  char *path = getenv ("PATH");
  if (!path)
    return perhaps_suffix (name, buf);

  /* Rather than force the issue of what the path delimiter should be,
     we do this:  If a ';' is present in the path list use ';', otherwise
     if the first path begins with <letter>: (in which case it can be the
     only element since if it wasn't a ';' would be present) use ';',
     otherwise use ':'.  */
  const char path_delim = posix_path_list_p (path) ? ':' : ';';

  /* Win32 systems always check . first, but PATH may not be set up to
     do this.  FIXME: This can be simplified, but leave til later.  */
  strcpy (tmp, name);
  strcpy (buf, path_conv (tmp).get_win32 ());
  len = strlen (buf);
  r = GetFileAttributesA (buf);
  if (r > 0 && !(r & FILE_ATTRIBUTE_DIRECTORY))
    return buf;
  buf[len] = 0;
  r = GetFileAttributesA (buf);
  if (r > 0 && !(r & FILE_ATTRIBUTE_DIRECTORY))
    return buf;

  while (*path)
    {
      char *d;

      d = tmp;

      while (*path && *path != path_delim)
	*d++ = *path++;
      *d++ = '/';
      strcpy (d, name);

      strcpy (buf, path_conv (tmp).get_win32 ());
      int len = strlen (buf);

      strcat (buf, ".exe");
      r = GetFileAttributesA (buf);
      if (r > 0 && !(r & FILE_ATTRIBUTE_DIRECTORY))
	return buf;

      buf[len] = 0;
      r = GetFileAttributesA (buf);
      if (r > 0 && !(r & FILE_ATTRIBUTE_DIRECTORY))
	return buf;

      if (*path == path_delim)
	path++;			/* skip over delim */
    }

  return perhaps_suffix (name, buf);
}

/* Find executable NAME, maybe by appending .exe to it.
   NAME.exe is preferred if it is found.
   $PATH is used if NAME has a relative path.
   The result is always BUF.  */

const char *
find_exec (const char *name, char *buf)
{
  const char *res = find_exec_1 (name, buf);
  debug_printf ("%s = find_exec (%s)\n", buf, name);
  return res;
}

#if 0
void
print_checksum (int idx, register void *low, register void *high)
{
  int pi;
  register  int sum = 0;
  small_printf ("CK %d %x %x ", idx, low, high);

  for (int *pi = (int *)low; pi < (int *)high; pi++)
    {
      sum += *pi;
    }
  small_printf ("%x\n", sum);
}
#endif

/* Copy memory from parent to child.
   The result is a boolean indicating success.  */

static int
copy (HANDLE child, void *low, void *high, int)
{
  DWORD done;
  int res;

  debug_printf ("fork copy: child handle %d, low %p, high %p\n",
		child, low, high);

  int lump = 1024*64;
  for (char *p = (char *) low; p < (char *) high; p += lump)
    {
      int todo = MIN ((char *)high - p, lump);

      res = WriteProcessMemory (child, p, p, todo, &done);
      if (!res || todo != done)
	{
	  if (!res)
	    __seterrno ();
	  /* This happens when it shouldn't so there's a bug in our fork
	     implementation somewhere.  Print a message on the console to
	     call people's attention to the failure until we get it
	     straightened out.  */
	  small_printf ("fork copy: failed, %p..%p, res is %d, done is %d\n",
			low, high, res, done);
	  /* Call debug_printf as well to make sure message gets in log
	     file if there is one.  */
	  debug_printf ("fork copy: failed\n");
	  return 0;
	}
    }

#if 0
  print_checksum (idx, low, high);
#endif
  debug_printf ("fork copy: done\n");
  return 1;
}

/* Main guts of fork implementation.
   The result is the standard result of fork.  */

static int
cygwin_fork_helper1 (void *proc_data_start,
		     void *proc_data_end,
		     void *proc_bss_start,
		     void *proc_bss_end)
{
  int res;
  int x, rc;

  if (u->self->split_heap_p)
    {
      small_printf ("The heap has been split, CYGWIN can't fork this process.\n");
      small_printf ("Increase the heap_chunk_size in the registry and try again.\n");
      set_errno (ENOMEM);
      syscall_printf ("-1 = fork (), split heap\n");
      return -1;
    }

  /* If we couldn't allocate the mutex or event handles, fork is disabled.  */
  if (fork_mutex == NULL)
    {
      /* We can't set errno to a random value.
	 See the Posix book for possible values to use.
	 ??? I think for our uses we can use ENOSYS at any time.
	 Perhaps that would be a better value here.  */
      set_errno (EAGAIN);
      syscall_printf ("-1 = fork (), no mutex\n");
      return -1;
    }

  /* Don't start the fork until we have the lock.  */
  rc = WaitForSingleObject (fork_mutex, FORK_WAIT_TIMEOUT);
  switch (rc)
    {
    case WAIT_ERROR_RC:
      small_printf ("fork parent: WaitForSingleObject (mutex) failed, win32 error %d\n",
		    GetLastError ());
      set_errno (EAGAIN);
      syscall_printf ("-1 = fork (), wait failed\n");
      return -1;
    case WAIT_TIMEOUT:
      small_printf ("fork parent: WaitForSingleObject (mutex) timed out\n");
      set_errno (EAGAIN);
      syscall_printf ("-1 = fork (), wait timed out\n");
      return -1;
    default:
      debug_printf ("fork_helper: %d = WaitForSingleObject (...)\n", rc);
      break;
    }

  pinfo *child = s->p.allocate_pid ();

  if (!child)
    {
      set_errno (EAGAIN);
      syscall_printf ("-1 = fork (), process table full\n");
      ReleaseMutex (fork_mutex);
      return -1;
    }

  /* This will help some of the confusion.  */
  fflush (stdout);

  debug_printf ("fork_helper: parent pid is %d, child pid is %d\n",
		u->self->get_pid (),
		child->get_pid ());

  /* Initialize things that are done later in dll_crt0_1 that aren't done
     for the forkee.  */
  /*  child->reent_data =  u->self->reent_data;*/
  child->progname = u->self->progname;

  /* Copy all the handles we need in the child.  */
  u->self->hmap.dup_for_fork (&child->hmap);

  PROCESS_INFORMATION pi = {0};

  STARTUPINFO si = {0};
  si.cb = sizeof (STARTUPINFO);

  int c_flags =  NORMAL_PRIORITY_CLASS | CREATE_SUSPENDED /* | CREATE_NEW_CONSOLE*/;

  syscall_printf ("CreateProcessA (%s, %s,0,0,1,%x, 0,0,%p,%p)\n",
		  u->self->progname, u->self->progname, c_flags, &si, &pi);

  rc = CreateProcessA (u->self->progname, /* image to run */
		       u->self->progname, /* what we send in arg0 */
		       0,		  /* process security attrs */
		       0,		  /* thread security attrs */
		       TRUE,		  /* inherit handles from parent */
		       c_flags,
		       0,	/* use parent's environment */
		       0,	/* use current drive/directory */
		       &si,
		       &pi);

  if (!rc)
    {
      __seterrno ();
      syscall_printf ("-1 = fork(), CreateProcessA failed\n");
      child->inuse_p = 0;
      ReleaseMutex (fork_mutex);
      return -1;
    }

  ResetEvent (forker_stopped);
  ResetEvent (forkee_stopped);
  
  debug_printf ("fork_helper: about to call setjmp\n");
  x = setjmp (u->self->restore);
  debug_printf ("fork_helper: setjmp returned %d\n", x);

#if 0
  if (0)
    {
      int k;
      unsigned char *p = (unsigned char *)(u->restore[32/4]);
      for (k = 0; k < 11; k++)
	small_printf ("set reg %d %x\n", k *4 , u->restore[k]);
      for (k = 0; k < 11; k++)
	small_printf ("[%02x]", p[k]);
      small_printf ("the res was %d\n", x);
    }
#endif
  
  if (x == 0)
    {
      /* Parent.  */

      dump_jmp_buf (u->self->restore);

#if 0 /* for debugging */
      s->base[0][0] = proc_data_start;
      s->base[0][1] = proc_data_end;
      s->base[1][0] = proc_bss_start;
      s->base[1][1] = proc_bss_end;
      s->base[2][0] = u->base;
      s->base[2][1] = u->ptr;
      s->base[3][0] = &x;
      s->base[3][1] = u->initial_sp; 
#endif 

      /* Tell the child it's being forked and its pid.
	 Remember, *u gets copied to the child's address space.  */
      u->forkee = child->get_pid ();

      /* Fill in fields in the child's process table entry.  */
      child->ppid = u->self->get_pid ();
      child->hThread = pi.hThread;      
      child->hProcess = pi.hProcess;
      child->dwProcessId = pi.dwProcessId;
      child->uid = u->self->uid;
      child->gid = u->self->gid;

      /* Initialize the child's .data and .bss.  */
      rc = copy (pi.hProcess, (char *)proc_data_start, (char *)proc_data_end, 0);
      if (rc)
	rc = copy (pi.hProcess, (char *)proc_bss_start, (char *)proc_bss_end, 1);
      if (! rc)
	{
	  small_printf ("fork_helper: copy of data/bss failed\n");
	  set_errno (EAGAIN);
	  syscall_printf ("-1 = fork(), data/bss copy failed\n");
	  TerminateProcess (child->hProcess, 1);
	  child->inuse_p = 0;
	  u->forkee = 0;
	  ReleaseMutex (fork_mutex);
	  return -1;
	}

      u->forkee = 0;

      /* FIXME: Need to record that task was started by cygwin32 task.
	 This info will then used by the exit() handling code to know whether
	 to reset inuse_p.  Normally inuse_p isn't reset until wait() is called
	 but if the task wasn't started by a cygwin32 task, wait() will never
	 be called and the process table will fill up.  */

      /* FIXME: Exit handling code also needs to record that the task ended
	 so the ps command will know about zombies.  */

      /* Start thread, and wait for it to initialize itself.  */
      rc = ResumeThread (child->hThread);
      if (rc != 1)
	{
	  /* Can't resume the thread.  Not sure why this would happen unless
	     there's a bug in the system.  Things seem to be working OK now
	     though, so flag this with EAGAIN, but print a message on the
	     console.  */
	  small_printf ("fork_helper: ResumeThread failed, rc = %d\n", rc);
	  set_errno (EAGAIN);
	  syscall_printf ("-1 = fork(), ResumeThread failed\n");
	  TerminateProcess (child->hProcess, 1);
	  child->inuse_p = 0;
	  ReleaseMutex (fork_mutex);
	  return -1;
	}
      debug_printf ("fork_helper: child started\n");

      /* We don't want to wait forever here.  If there's a problem somewhere
	 it'll hang the entire system (since all forks are mutex'd).  If we
	 time out, set errno = EAGAIN and hope the app tries again.  */
      rc = WaitForSingleObject (forkee_stopped, FORK_WAIT_TIMEOUT);
      if (rc == WAIT_ERROR_RC || rc == WAIT_TIMEOUT)
	{
	  if (rc == WAIT_ERROR_RC)
	    small_printf ("fork_helper: WaitForSingleObject failed, win32 error %d\n",
			  GetLastError ());
	  else
	    small_printf ("fork_helper: WaitForSingleObject timed out\n");
	  set_errno (EAGAIN);
	  syscall_printf ("-1 = fork(), WaitForSingleObject failed\n");
	  TerminateProcess (child->hProcess, 1);
	  child->inuse_p = 0;
	  ReleaseMutex (fork_mutex);
	  return -1;
	}

      SuspendThread (child->hThread);

      /* Now fill in the stack and heap - this has to be done after 
	 the child is started.  */
      rc = copy (child->hProcess, u->base, u->ptr, 2);
      if (rc)
	rc = copy (child->hProcess, &x, u->initial_sp, 3);
      if (! rc)
	{
	  small_printf ("fork_helper: copy of stack/heap failed\n");
	  set_errno (EAGAIN);
	  syscall_printf ("-1 = fork(), stack/heap copy failed\n");
	  TerminateProcess (child->hProcess, 1);
	  child->inuse_p = 0;
	  ReleaseMutex (fork_mutex);
	  return -1;
	}

      /* Start the child up again.  */
      SetEvent (forker_stopped);
      ResumeThread (child->hThread);

      ReleaseMutex (fork_mutex);
      res = child->get_pid ();
    }
  else
    {
      /* We arrive here via a longjmp from "crt0".  */
      debug_printf ("fork_helper: child is running\n");

      u->self = s->p[x];
      debug_printf ("fork child: self %p, pid %d, ppid %d\n",
		    u->self, x, u->self->ppid);

      /* Tell our parent we've started.  */
      SetEvent (forkee_stopped);

      /* Wait for the parent to fill in our stack and heap.
	 Don't wait forever here.  If our parent dies we don't want to clog
	 the system.  If the wait fails, we really can't continue so exit.  */
      int rc = WaitForSingleObject (forker_stopped, FORK_WAIT_TIMEOUT);
      switch (rc)
	{
	case WAIT_ERROR_RC:
	  small_printf ("fork child: WaitForSingleObject failed, win32 error %d\n",
			GetLastError ());
	  ExitProcess (1);
	case WAIT_TIMEOUT:
	  small_printf ("fork child: WaitForSingleObject timed out\n");
	  ExitProcess (1);
	default:
	  break;
	}

#if 0
      print_checksum (4,   s->base[0][0], s->base[0][1]);
      print_checksum (5,   s->base[1][0], s->base[1][1]);
      print_checksum (6,   s->base[2][0], s->base[2][1]);
      print_checksum (7,   s->base[3][0], s->base[3][1]);
#endif

      res = 0;
    }

  syscall_printf ("%d = fork()\n", res);
  return res;
}

/* This hack uses setjmp/longjmp to ensure that the parent's
   registers are all available in the child. We could use
   GetThreadContext/SetThreadContext instead, but I'm sure this
   is much faster. */

static int
__fork ()
{
  jmp_buf b;
  int r;

  if ((r = setjmp (b)) != 0)
    {
      r = r == -2 ? -1 : r == -1 ? 0 : r;
      return r;
    }

  r = cygwin_fork_helper1 (u->data_start,
			   u->data_end,
			   u->bss_start,
			   u->bss_end);

  /* Must convert result to get it through setjmp ok.  */
  longjmp (b, r == -1 ? -2 : r == 0 ? -1 : r);
}

/* Utility to dump a setjmp buf.  */

void
dump_jmp_buf (jmp_buf buf)
{
#ifdef __i386__
  debug_printf ("jmp_buf: eax 0x%x, ebx 0x%x, ecx 0x%x, edx 0x%x\n",
		buf[0], buf[1], buf[2], buf[3]);
  debug_printf ("jmp_buf: esi 0x%x, edi 0x%x, ebp 0x%x, esp 0x%x\n",
		buf[4], buf[5], buf[6], buf[7]);
  short *s = (short *) &buf[9];
  debug_printf ("jmp_buf: es 0x%x, fs 0x%x, gs 0x%x, ss 0x%x\n",
		s[0], s[1], s[2], s[3]);
  debug_printf ("jmp_buf: eip: 0x%x\n", buf[8]);
#endif
}

extern "C"
int
vfork () 
{
  return fork ();
}

extern "C"
int
fork ()
{
  /* FIXME: Was there a real reason for this?  */
  alloca (100);

  int r = __fork ();
  return r;
}
