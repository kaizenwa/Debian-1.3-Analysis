/* Posix wait routines.  

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include "winsup.h"
#include "sys/wait.h"

#define WAIT_ERROR_RC 0xffffffff

static pid_t wait_for_single (pid_t, int *, int);
static pid_t wait_for_any (int *, int);

/* This is called _wait and not wait because the real wait is defined
   in libc/syscalls/syswait.c.  It calls us.  */

pid_t
_wait (int *status)
{
  return waitpid (-1, status, 0);
}

/* Come here when we know that the child has stopped.  */

static int
wait_found (pinfo *child, int *status, int options)
{
  DWORD result;
  int unixresult;
  int pid = child->get_pid ();

  if (!GetExitCodeProcess (child->hProcess, &result))
    {
      small_printf ("wait_found: GetExitCodeProcess failed!\n");
      /* Assume child did `exit (1)'.  */
      result = 1;
    }

  child->record_death ();

  if (! CloseHandle (child->hProcess))
    {
      small_printf ("wait_found: CloseHandle process failed\n");
    }
  if (! CloseHandle (child->hThread))
    {
      small_printf ("wait_found: CloseHandle thread failed\n");
    }

  if (result & 0x010000)
    {
      /* we had a signal - see exceptions.c */
      unixresult = (result >> 8) & 0xff;
    }
  else
    {
      /* Normal exit */
      unixresult = ((result & 0xff) << 8);
    }

  if (status)
    *status = unixresult;

  debug_printf ("wait_found: %d = waitpid (-1, %p, %d) (0x%x)\n",
		pid, status, options, unixresult);
  return pid;
}

static pid_t
wait_for_single (pid_t intpid, int *status, int options)
{
  pinfo *c;
  DWORD timeout;
  int rc;

  c = s->p[intpid];
  if (!c)
    {
      debug_printf ("wait_for_single: c was null!\n");
      set_errno (ECHILD);
      rc = -1;
      goto done;
    }

  debug_printf ("wait_for_single: child pid %d\n", c->get_pid ());

  if (options & WNOHANG)
    timeout = 0;
  else
    timeout = INFINITE;

  rc = WaitForSingleObject (c->hProcess, timeout);
  debug_printf ("wait_for_single: %d = WaitForSingleObject (%d, %d)\n",
		rc, c->hProcess, timeout);
  switch (rc)
    {
    case WAIT_ERROR_RC:
      /* We shouldn't set errno to any random value if we can help it.
	 See the Posix manual for a list of valid values for `errno'.  */
      debug_printf ("wait_for_single: %d = GetLastError ()\n",
		    GetLastError ());
      set_errno (EINVAL);
      rc = -1;
      break;
    case WAIT_OBJECT_0:
      return wait_found (c, status, options);
    case WAIT_ABANDONED:
    case WAIT_TIMEOUT:
    default:
      set_errno (ECHILD);
      rc = -1;
      break;
    }

done:
  syscall_printf ("%d = waitpid (%d, %p, %d)\n", rc, intpid, status, options);
  return rc;
}

/* Wait for any child to complete.
   WIN32 doesn't provide this in the API so we have to cope.  A list of
   (presumed) active procinfo is maintained and we (try to) keep that
   up to date.  */

static pid_t
wait_for_any (int *status, int options)
{
  int nprocinfo;
  DWORD  timeout;
  int i,rc;
  HANDLE *ctable;
  pinfo **ptable;

  pinfo *me = this_procinfo ();

  /* Count how many items there are.  If we come across a child that never
     exec'd, pick that one.  */
  nprocinfo = 0;
  for (i = 0; i < s->p.size (); i++)
    {
      pinfo *p = &s->p.vec[i];

      if (! p->inuse_p)
	continue;

      /* This happens if the task wasn't created by another cygwin task.  */
      if (p->ppid == p->get_pid ())
	continue;

      /* Is it a child of ours?  */
      if (p->ppid != me->get_pid ())
	continue;

      /* Found one of our children.  */
      nprocinfo++;
    }

  ctable = (HANDLE *)(alloca (nprocinfo * (sizeof (HANDLE *))));
  ptable = (pinfo **)(alloca (nprocinfo * (sizeof (pinfo **))));

  /* Build a table of procinfo handles so we can run
     WaitForMultipleObjects.  If on the way we find that
     a child has finished, then we take advantage of that.  */
  nprocinfo = 0;
  for (i = 0; i < s->p.size (); i++)
    {
      pinfo *p = &s->p.vec[i];

      if (p->inuse_p
	  && p->ppid != p->get_pid ()
	  && p->ppid == me->get_pid ())
	{
	  ptable[nprocinfo] = p;
	  ctable[nprocinfo++] = p->hProcess;

	  debug_printf ("wait_for_any: child pid %d, handle %d\n",
			p->get_pid (), p->hProcess);
	}
    }

  if (nprocinfo == 0)
    {
      /* Ran though the table, and there were no procinfo there. */
      set_errno (ECHILD);
      rc = -1;
      goto done;
    }

  timeout = options & WNOHANG ? 0 : INFINITE;

  rc = WaitForMultipleObjects (nprocinfo, ctable, 0, timeout);

  if (rc == WAIT_ERROR_RC)
    {
      small_printf ("wait_for_any: WaitForMultipleObjects failed, win32 error %d\n",
		    GetLastError ());
      small_printf ("wait_for_any: Trying again.\n");
      rc = WaitForMultipleObjects (nprocinfo, ctable, 0, timeout);
      if (rc == WAIT_ERROR_RC)
	small_printf ("wait_for_any: That didn't help, win32 error %d\n",
		      GetLastError ());
    }

  debug_printf ("wait_for_any: %d = WaitForMultipleObjects (...)\n", rc);

  if (rc == WAIT_ERROR_RC)
    {
      /* perhaps one of them has closed ? try and poll singly */
      for (i = 0; i < nprocinfo; i++)
	{
	  rc = WaitForMultipleObjects (1, ctable+i, 0, 0);
	  if (rc != WAIT_ERROR_RC)
	    {
	      small_printf ("wait_for_any: Single polling worked!\n");
	      rc = wait_found (ptable[i], status, options);
	      goto done;
	    }
	}
    }

  if (rc == WAIT_ERROR_RC)
    {
      /* We shouldn't set errno to any random value if we can help it.
	 See the Posix manual for a list of valid values for `errno'.  */
      debug_printf ("wait_for_any: %d = GetLastError ()\n", GetLastError ());
      set_errno (EINVAL);
      rc = -1;
    }
  else if (rc >= WAIT_OBJECT_0 && rc < WAIT_OBJECT_0 + nprocinfo)
    {
       rc = wait_found (ptable[rc - WAIT_OBJECT_0], status, options);
    }
  else
    {
      set_errno (ECHILD);
      rc = -1;
    }

 done:
  syscall_printf ("%d = waitpid (-1, %p, %d)\n", rc, status, options);
  return rc;
}

/* ??? waitpid () can return 0 in some circumstances.
   We don't handle that.  */

pid_t
waitpid (pid_t intpid, int *status, int options)
{
  /* We don't handle intpid == 0.  */

  if (intpid == 0)
    {
      small_printf ("waitpid (0, x, y) not supported\n");
      set_errno (ENOSYS);
      syscall_printf ("-1 = waitpid (%d, %p, %d) (ENOSYS)\n",
		      intpid, status, options);
      return (pid_t) -1;
    }

  if (intpid == -1)
    return wait_for_any (status, options);
  else
    return wait_for_single (intpid, status, options);
}
