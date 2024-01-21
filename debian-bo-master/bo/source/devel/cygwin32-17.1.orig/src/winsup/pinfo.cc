/* process table support for WIN32.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or its performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include "winsup.h"

/* Initialize the process table.
   This is done once when the dll is first loaded.  */

void
pinfo_list::init ()
{
  pid_base = 1000;

  /* We assume the shared data area is already initialized to zeros.
     Note that SIG_DFL is zero.  */
}

/* Reset everything in a process table entry.
   This is called when allocating a new entry.  */

void
pinfo::clearout ()
{
  hmap.clearout ();
  split_heap_p = 0;
  progname[0] = 0;
  sig_mask = 0;
  strace_mask_ptr = 0;
  memset (sigs, 0, sizeof (sigs));
}

/* Initialize the process table entry for the current task.
   This is not called for fork'd tasks, only exec'd ones.  */

/* FIXME: Process table lookups may need to be sped up some day.  */

/* FIXME: For the moment we use the presence of env var PID to determine
   whether we've been invoked by a cygwin task and what our PID is.  This
   isn't bullet proof as it doesn't handle the case of cygwin invoking dos
   invoking cygwin.  A better way would be for spawn_guts to start the task
   suspended, fill in the process table with our win32 "Process Id", and then
   resume us; if we don't find our Process Id in the table, we were invoked by
   a non cygwin app.  */

void
pinfo_init ()
{
  int pid;
  char buf[20];

  int n = GetEnvironmentVariableA ("PID", buf, sizeof (buf));

  if (n)
    {
      /* We were exec'd, so use same process table entry.
	 This means, for example, that `pid' already contains the
	 right value.  */
      pid = atoi (buf);
      u->self = s->p[pid];
      u->self->cygwin_parent_p = 1;
    }
  else
    {
      /* Invent our own pid.  */
      pid = s->p.allocate_pid ()->get_pid ();
      u->self = s->p[pid];
      u->self->cygwin_parent_p = 0;
      u->self->init_self ();
    }

  u->self->dwProcessId = GetCurrentProcessId ();

  /* Make `strace_mask' accessable to other programs.
     One example is `cygwin' which is used to change the value of strace_mask
     while the program is running.  */
  u->self->strace_mask_ptr = &u->strace_mask;
}

/* [] operator.  This is the mechanism for table lookups.  */

pinfo *
pinfo_list::operator [] (pid_t arg)
{
  /* Lookup pid in table.  */
  for (int i = 0; i < size (); i++)
    {
      if (vec[i].pid == arg)
	return vec + i;
    }
  return 0;
}

pinfo *
this_procinfo ()
{
  return u->self;
}

pinfo *
procinfo (int pid)
{
  return s->p[pid];
}

/* Find an empty slot in the process table.  */

pinfo *
pinfo_list::get_empty_pinfo ()
{
  /* We could use a linked list, and maybe eventually we will.  The process
     table is in the shared data space and thus is susceptible to corruption.
     The amount of time spent scanning the table is presumably quite small
     compared with the total time to create a process.  */

  for (int i = 0; i < size (); i++)
    {
      if (!vec[i].inuse_p)
	{
	  vec[i].clearout ();
	  debug_printf ("pinfo_list::get_empty_pinfo: found empty slot %d\n", i);
	  return vec + i;
	}
    }

  /* The process table is full.  */
  debug_printf ("pinfo_list::get_empty_pinfo: process table is full\n");
  return 0;
}

/* Allocate a process table entry.  */

pinfo *
pinfo_list::allocate_pid ()
{
  /* This is currently only done by fork which has its own mutex, so we
     don't need one.  */
  // int r = WaitForSingleObject (???);

  pinfo *p = get_empty_pinfo ();
  p->pid = pid_base++;
  p->inuse_p = 1;

  // ReleaseMutex (???);
  return p;
}

/* Called when initializing the top self process from what we can
   find in the environment.  */

void
pinfo::init_self ()
{
  /* can't guess at parent, so pretend it's self */
  ppid = get_pid ();

  clearout ();
  /* Work out a real handle on self so that other processes can get
     to it */
#if 1
  int res =  DuplicateHandle (GetCurrentProcess (),
			     GetCurrentThread (),
			     GetCurrentProcess (),
			     &hThread,
			     0,
			     1, /* Want kids to see thread id */
			     DUPLICATE_SAME_ACCESS);

  res =  DuplicateHandle (GetCurrentProcess (),
			 GetCurrentProcess (),
			 GetCurrentProcess (),
			 &hProcess,
			 0,
			 1,	/* Want kids to see thread id */
			 DUPLICATE_SAME_ACCESS);
#else
  hThread = GetCurrentThread ();
  hProcess = GetCurrentProcess ();
#endif
/*  debug_printf ("self thrad is %d %d, self is %x\n", res, hThread, this);*/
  inuse_p = 1;


}

int
pinfo::get_pid ()
{
  return pid;
}

void
pinfo::record_death ()
{
  inuse_p = 0;
}

void
pinfo::terminate ()
{
  if (! cygwin_parent_p)
    {
      /* Undo what init_self does.  */
      CloseHandle (hProcess);
      CloseHandle (hThread);
    }
}

void
pinfo::init_from_exec ()
{
  /* Close all the close_on exec things and duplicate the rest into
     the child's space  */

  /*  memset (&reent_data, 0, sizeof (reent_data));*/
  debug_printf ("in init_from_exec\n");

  hmap.dup_for_exec ();
}
