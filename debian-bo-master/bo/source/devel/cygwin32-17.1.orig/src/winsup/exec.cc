/* exec system call support.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include "winsup.h"

static int
file_exists (const char *name)
{
  int res;
  res = GetFileAttributesA (path_conv (name).get_win32 ());
  return res > 0 && !(res & FILE_ATTRIBUTE_DIRECTORY);
}

/* This is called _execve and not execve because the real execve is defined
   in libc/posix/execve.c.  It calls us.  */

pid_t
_execve (const char *path, char *const argv[], char *const envp[])
{
  pinfo *pi = this_procinfo ();

  syscall_printf ("_execve (%s, %s, %x)\n", path, argv[0], envp);

  /* First check file without .exe extension.  If it doesn't exist, try
     with .exe.  */

  if (! file_exists (path))
    {
      /* Can't find it, try again with .exe.  If PATH already has a .exe
	 extension we'll execute foo.exe.exe.  We define that as being
	 correct to keep things simple.  */

      syscall_printf ("can't find %s, trying with .exe appended\n", path);
      int l = strlen (path);
      char *copy = (char *) alloca (l+5);
      memcpy (copy, path, l);
      memcpy (copy + l, ".exe", 5);
      if (! file_exists (copy))
	{
	  syscall_printf ("-1 = _execve (%s, ...) not found\n", path);
	  set_errno (ENOENT);
	  return -1;
	}
      path = copy;
    }

  /* We do not pass _P_SEARCH_PATH here.  execve doesn't search PATH.  */
  spawn_guts (path, argv, envp, pi->get_pid ());

  /* Errno should be set by spawn_guts.  */
  return -1;
}
