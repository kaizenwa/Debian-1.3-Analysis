/* spawn for WIN32.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/* FIXME: Header file inclusion needs to be standardized.  */
#include "winsup.h"
#include "paths.h"

/* Utility for spawn_guts.  */

static HANDLE
handle (int n)
{
  hinfo &h = u->self->hmap[n];

  if (!h.is_open ())
    return INVALID_HANDLE_VALUE;
  if (h.h->close_exec_p)
    return INVALID_HANDLE_VALUE;
  return h.h->get_handle ();
}

/* Cover function for CreateProcess.

   This function is used by both the routines that search $PATH and those
   that do not.  This should work out ok as according to the documentation,
   CreateProcess only searches $PATH if PROG has no directory elements.

   Spawning doesn't fit well with Posix's fork/exec (one can argue the merits
   of either but that's beside the point).  If we're exec'ing we want to
   record the child pid for fork.  If we're spawn'ing we don't want to do
   this.  It is up to the caller to handle both cases.

   The result is the process id.  The handle of the created process is
   stored in H.
*/

void
spawn_guts (const char *prog_arg, char *const *argv,
	    char *const envp[], int pid)
{
  int i;
  char *copy;
  BOOL rc;

  syscall_printf ("spawn_guts (%s)\n", prog_arg);

  if (argv == 0)
    {
      syscall_printf ("spawn_guts: argv is 0\n");
      exit (1);
    }

  /* CreateProcess takes one long string that is the command line (sigh).
     We need to quote any argument that has whitespace or embedded "'s.  */
  {
    int clen;

    /* See how much space we need.  Note that "'s are doubled. */
    clen = 0;
    if (argv[0] == 0)
      {
	/* Hmm - nasty - no prog name, - fill one up for us */
	small_printf ("NO ARG0 !!\n");
      }
    for (i = 0; argv[i]; i++) 
      {
        int needquote = 0;
        char *s;
	/* Zero length args need quotes. */
	if (!argv[i][0])
	  needquote = 1;

        for (s = argv[i];  *s; s++) 
          {
	    if (*s == '"')
	      clen++;
            if (*s == ' ' || *s == '\t' || *s == '"')
	      needquote = 1;
            clen++;
          }
        if (needquote)  
	  clen += 2;		/* space for quotes */
        clen++;			/* space for separating space */
      }

    copy = (char *) alloca (clen + 1);
    clen = 0;

    for (i = 0; argv[i]; i++) 
      {
        int needquote = 0;
        char *s;

	/* Extra space after `in' intentional.  */
        syscall_printf ("_spawn () argv[%d] in  `%s'\n", i, argv[i]);

	if (!argv[i][0])
	  needquote = 1;

        for (s = argv[i]; *s; s++) 
	  if (*s == ' ' || *s == '\t' || *s == '"') 
	    needquote = 1;

        if (needquote) 
	  copy[clen++] = '"';

        for (s = argv[i]; *s; s++) 
          {
            if (*s=='"')
	      copy[clen++] = '"';
            copy[clen++] = *s;
          }

        if (needquote)  
	  copy[clen++] = '"'; 
        copy[clen++] = ' ';
        copy[clen] = 0;
        syscall_printf ("_spawn () argv[%d] out `%s'\n", i, argv[i]);
      }
  }

  /* And dup all the things we want to copy across */
  /* FIXME: This doesn't dup anything.  What it does do is close files
     with close-on-exec set.  That doesn't seem right though, what if the
     exec fails?  */
  u->self->init_from_exec ();

  /* We have to translate PATH, etc. back so a dos image can grok them.  */

  /* Result, passed to CreateProcess.  */
  char *nlist;
  {
    int tc;
    for (tc = 0; envp[tc]; tc++)
      continue;
    char **newenv = (char **) alloca ((tc+1) * sizeof (char *));
    memcpy (newenv, envp, (tc+1) * sizeof (char *));
    int tl = 0;
    int srci;
    int dsti;

    for (srci = dsti = 0; newenv[srci]; srci++)
      {
	/* Skip this one, set later.  */
	if (strncmp (newenv[srci], "PID=", 4) == 0)
	  continue;

	/* Set to non-zero when env var ready to be copied to dst.  */
	int done = 0;

	if (! s->mount.posix_path_p ())
	  done = -1;

	for (int ei = 0; !done && conv_path_names[ei]; ei++) 
	  {
	    /* Found an env name, turn from unix style into dos style.  */
	    int len = strlen (conv_path_names[ei]);

	    if (strncmp (newenv[srci], conv_path_names[ei], len) == 0)
	      {
		char b[2000];
		char *d = b;
		char *src = newenv[srci] + len;

		debug_printf ("spawn_guts: translating %s\n", newenv[srci]);

		/* Split out the colons.  */
		while (1)
		  {
		    char *e = strchr (src, ':');
		    if (e)
		      {
			*e = 0;
			conv_to_win32_path (src, d);
			d += strlen (d);
			src = e + 1;
			*d++ = ';';
		      }
		    else
		      {
			conv_to_win32_path (src, d);
			d += strlen (d);
			*d++ = 0;
			break;
		      }
		  }

		newenv[dsti] = (char *) alloca (d - b + 1 + len);
		memcpy (newenv[dsti], conv_path_names[ei], len);
		memcpy (newenv[dsti] + len, b, d - b + 1);
		debug_printf ("spawn_guts: setting %s\n", newenv[dsti]);
		tl += strlen (newenv[dsti]) + 1;
		dsti++;
		done = 1;
	      }
	  }

	/* Handle env var if not done yet.  */
	if (done <= 0)
	  {
	    newenv[dsti] = newenv[srci];
	    tl += strlen (newenv[dsti]) + 1;
	    dsti++;
	  }
      }
    newenv[dsti] = 0;
    debug_printf ("spawn_guts: env count=%d bytes=%d\n", dsti, tl);

    /* Create an environment block suitable for passing to CreateProcess.  */
    nlist = (char *) alloca (tl + 2 + 20);
    char *ptr = nlist;
    for (i = 0; newenv[i]; i++)
      {
	int len = strlen (newenv[i]);
	memcpy (ptr, newenv[i], len+1);
	ptr = ptr + len + 1;
      }
    __small_sprintf (ptr, "PID=%d", pid);
    ptr += strlen (ptr);
    *ptr++ = 0; 
    *ptr++ = 0;
  }

  /* Follow any symlinks.  */

  char real_path[MAX_PATH];
  if (symlink_follow (prog_arg, real_path) == -1)
    return;

  path_conv prog_tmp (real_path);
  const char *prog = prog_tmp.get_win32 ();

  /* Check if it's a script.  */

  SECURITY_ATTRIBUTES sa;  
  sa.nLength = sizeof (sa);
  sa.lpSecurityDescriptor = 0;
  sa.bInheritHandle = TRUE;

  HANDLE hnd = CreateFileA (prog, 
			    GENERIC_READ,
			    FILE_SHARE_READ | FILE_SHARE_WRITE,
			    &sa,
			    OPEN_EXISTING,
			    FILE_ATTRIBUTE_NORMAL,
			    0);
  if (hnd == INVALID_HANDLE_VALUE)
    {
      __seterrno ();
      return;
    }

  DWORD done;
  char magic[2];

  if (! ReadFile (hnd, magic, sizeof (magic), &done, 0))
    {
      CloseHandle (hnd);
      __seterrno ();
      return;
    }
  CloseHandle (hnd);

  /* FIXME: Change this to look for #! and invoke the requested shell.  */
  if (done >= 2 && magic[0] != 'M' && magic[1] != 'Z')
    {
      /* This is a script.. so we'll run bash on it */
      debug_printf ("spawn_guts: %s is a script\n", prog_arg);

      char *sh_path_buf = (char *) alloca (MAX_PATH);
      char *sh_path = (char *) find_exec ("sh", sh_path_buf);
      char *f = (char *) alloca (strlen (copy) + strlen (sh_path) + 2);
      strcpy (f, sh_path);
      strcat (f, " ");
      strcat (f, copy);
      copy = f;

      debug_printf ("spawn_guts: copy is now %s\n", copy);
      prog = sh_path;
    }

  PROCESS_INFORMATION pi = {0};

  STARTUPINFO si = {0};
  si.lpReserved = NULL;
  si.lpReserved2 = NULL;
  si.cbReserved2 = 0;
  si.lpDesktop = NULL;
  si.dwFlags = STARTF_USESTDHANDLES;
  si.hStdInput = handle (0);
  si.hStdOutput = handle (1);
  si.hStdError = handle (2);
  si.cb = sizeof (si);

  /* We print the translated program and arguments here so the user can see
     what was done to it.  */
  syscall_printf ("_spawn_guts (%s, %s)\n", prog, copy);

  rc = CreateProcessA (prog,	/* image name - with full path */
		       copy,	/* what was passed to exec */
		       0,	/* process security attrs */
		       0,	/* thread security attrs */
		       1,	/* inherit handles from parent */
		       NORMAL_PRIORITY_CLASS,
		       nlist,	/* use our own environment */
		       0,	/* use current drive/directory */
		       &si,
		       &pi);

  /* Set errno now so that debugging messages from it appear before our
     final debugging message [this is a general rule for debugging
     messages].  */
  if (!rc)
    __seterrno ();

  /*  p->init_from_exec (pi.hProcess, pi.hThread);*/
  /* We print the original program name here so the user can see that too.  */
  syscall_printf ("%d = _spawn (%s, %s)\n",
		  rc ? pi.dwProcessId : -1,
		  prog_arg, copy);

  if (rc)
    {
      /* We keep our original process, so that the parent can wait for
	 us,  and we wait for the child. */
      /* FIXME: An explanation of why we have to do things this way
	 is in order.  */
      /* FIXME: If the child closes the write side of a pipe that the
	 parent is reading, will the parent's read () return?  We
	 will still have the write side open!  */

      /* Start the child running */

      CloseHandle (pi.hThread);

      WaitForSingleObject (pi.hProcess, INFINITE);
      /* Then find out what stopped it */
      DWORD res;
      GetExitCodeProcess (pi.hProcess,  &res);
      syscall_printf ("returning process says %x\n", res);
      CloseHandle (pi.hProcess);
      /* and return with it */
      ExitProcess (res);
    }
}
