/* dll's crt0.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NxOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.

   This bit of crt0 lives in the dll.
 */

#include <ctype.h>
#include <unistd.h>
#include "winsup.h"
#include "glob/glob.h"
#include "exceptions.h"
#include "version.h"

per_process *u;

extern "C"
{
  /* Used for the libc stuff which uses environ,
     we copy this into the real crt0.  */
  char **environ;
};

static const int cygwin_dll_version_major = CYGWIN_DLL_VERSION_MAJOR;
static const int cygwin_dll_version_minor = CYGWIN_DLL_VERSION_MINOR;

/* Return number of elements in a microsoft style env list.  
   
   The "environment block" is a null terminated block of strings
   (two nulls terminate the block).  */

static int
num_ms_env_vars (const char *rawenv)
{
  int i = 0;
  int count = 0;
  while (1)
    {
      if (rawenv[i] == 0)
	{
	  count++;
	  if (rawenv[i + 1] == 0)
	    break;
	}
      i++;
    }
  return count;
}

/* Build argv from string passed from Windows.  */

static void
build_argv (char *in, char **out, int len)
{
  int count = 0;
  char *e = in + len;
  char *s;

  for (s = in; s < e; s++)
    {
      debug_printf ("build_argv: fill iteration %s\n", s);

      /* Delete leading spaces.  */
      while (*s == ' ' || *s == '\t')
	s++;

      if (!*s)
	break;

      /* Got to an arg.  */

      char *d;
      out[count] = d = s;
      
      if (*s == '"')
	{
	  s++;
	  while (s[0] && (s[0] != '"' || (s[0] == '"' && s[1] == '"')))
	    {
	      if (*s== '"')
		s++;
	      *d++ = *s++;
	    }
	}
      else
	{
	  while (*s && *s != ' ' && *s != '\t')
	    {
	      *d++ = *s++;
	    }
	}
      *d++ = 0;
      debug_printf ("build_argv: argv[%d]=%s\n", count, out[count]);
      count++;
    }
  out[count] = 0;

  /* FIXME: Verify count == len?  */
}

/* Compute argc.
   Windows passes just a string which we need to break up into argc/argv.
   This pass computes argc.  */

static int
compute_argc (char *in)
{
  char *src;
  int count=0;

  debug_printf ("compute_argc (%s)\n", in);

  for (src = in; *src; src++)
    {
      while (*src == ' ' || *src == '\t' )
	src++;
      
      if (!*src)
	break;
      
      debug_printf ("compute_argc: src is now %s\n", src);
      if (*src == '"')
	{
	  src++;
	  while (src[0])
	    {
	      if (src[0] == '"') 
		{
		  src++;
		  if (src[0] != '"') break;
		}
	      
	      src++;
	    }
	}
      else
	{
	  while (*src != ' ' && *src != '\t' && *src != 0)
	    src++;
	}
      count++;
    }
  debug_printf ("compute_argc: count is %d\n", count);
  return count;
}

/* Perform various start up sanity checks.  */

static void
checkout ()
{
#ifdef __PPC__
  jmp_buf x;

  if (sizeof (x) != 32*8)
    {
      small_printf ("Configuration error with setjmp\n");
    }
#endif
}

static void
globify (int *acp, char ***avp)
{
  /* Yes I know I could use references, but they hide the side effects */
  int ac = *acp;
  char **av = *avp;
  glob_t globs[ac];
  int rc[ac];
  int i;
  int newac = 0;
  int donesomething = 0;

  for (i = 0; i < ac; i++)
    {
      if (strpbrk (av[i], "?*[") != NULL)
	{
	  globs[i].gl_offs = 0;
	  rc[i] = glob (av[i], 
			GLOB_NOCHECK | GLOB_NOESCAPE,
			0,
			&globs[i]);
	  newac += rc[i] == 0 ? globs[i].gl_pathc : 1;
	  donesomething = rc[i] == 0;
	}
      else
	{
	  rc[i] = -1; /* glob not called */
	  ++newac;
	}
    }
  
  if (donesomething)
    {
      char **newav = (char **)malloc (sizeof (char **) * (newac + 1));
      int d = 0;
      for (i = 0; i < ac; i++)
	{
	  if (rc[i] == 0)
	    {
	      debug_printf ("glob: `%s' converted to:", av[i]);
	      for (int j = 0; j < globs[i].gl_pathc; j++)
		{
		  debug_printf (" %s", globs[i].gl_pathv[j]);
		  newav[d++] = globs[i].gl_pathv[j];
		}
	      debug_printf ("\n");
	    }
	  else
	    {
	      debug_printf ("glob: `%s' not converted, glob rc = 0x%x\n",
			    av[i], rc[i]);
	      newav[d++] = av[i];
	    }
	}
      newav[d] = 0;
      *acp = newac;
      *avp = newav;
    }
}

/* Utility to probe the stack.  */

static void
recur (void *p, void *)
{
  char b[1000];
  int x;

  if (&x > p)
    recur (p, b);
}

/* List of names which are converted from dos to unix
   on the way in and back again on the way out.

   PATH needs to be here because CreateProcess uses it and gdb uses
   CreateProcess.  Having too many more here is dubious.  */

const char *conv_path_names[] =
{
  "PATH=",
  0
};

/* Take over from libc's crt0.o and start the application.  */

static void
dll_crt0_1 (per_process *uptr)
{
  int argc;
  char **argv;
  int i;
  struct _reent reent_data;
  /* According to onno@stack.urc.tue.nl, the exception handler record must
     be on the stack.  */
  /* FIXME: Verify forked children get their exception handler set up ok.  */
  struct exception_list cygwin_except_entry;

  /* Sanity check to make sure developers didn't change the per_process
     struct without updating SIZEOF_PER_PROCESS [it makes them think twice
     about changing it].  */
  if (sizeof (per_process) != SIZEOF_PER_PROCESS)
    {
      small_printf ("per_process sanity check failed\n");
      ExitProcess (1);
    }

  /* Make sure that the app and the dll are in sync.
     magic_biscuit != 0 if using the old style version numbering scheme.  */
  if (uptr->magic_biscuit != SIZEOF_PER_PROCESS
      || uptr->version_major != cygwin_dll_version_major)
    {
      small_printf ("CYGWIN DLL and APP out of sync, you'll have to relink, sorry.\n");
      small_printf ("magic numbers (app %d, dll %d)\n",
		    uptr->magic_biscuit, 0);
      if (uptr->magic_biscuit == 0)
	small_printf ("cygwin version numbers (app %d.%d, dll %d.%d)\n",
		      uptr->version_major, uptr->version_minor,
		      cygwin_dll_version_major, cygwin_dll_version_minor);
      ExitProcess (1);
    }

  /* Set the local copy of the pointer into the user space.  */
  u = uptr;

  /* Enable stracing as soon as possible.  */
  strace_init ();

  /* Initialize the cygwin32 subsystem if this is the first process,
     or attach to the shared data structure if it's already running.  */
  shared_init ();

  /* Initialize SIGSEGV handling, etc.
     The exception handler references data in the shared area, so this can't
     be done until after shared_init.  */
  /* FIXME: For the forkee, our stacked copy of cygwin_except_entry will
     get overwritten, but presumably with the same data.  Not sure what
     happens in the forkee if a SIGSEGV happens before now.  */
  init_exceptions (&cygwin_except_entry);

  /* Initialize the fork mechanism.  */
  fork_init ();

  /* Initialize the heap.  */
  heap_init ();

  /* If this is a fork, then do that now.  All the other stuff will
     have been set up by the task that forked us.  */

  if (u->forkee)
    {
      environ = *u->envptr;

      /* When a forked child starts, its stack is quite small.
	 However, when it longjmp's, the stack becomes that of its parent
	 which is arbitrarily large.  This can cause the child to crash so
	 probe the stack out.  */
      /* FIXME: Fix hardcoding of size after migration.  */
      recur ((char *)&i - 0x20000, 0);

      debug_printf ("child about to longjmp\n");
      dump_jmp_buf (u->self->restore);

      /* The corresponding setjmp is in fork.cc.  */
      longjmp (u->self->restore, u->forkee);
    }

  /* Perform various startup sanity checks.  */
  checkout ();

  /* Initialize our process table entry.  */
  pinfo_init ();

  /* Initialize the file descriptor table.  */
  hmap_init ();

  /* Initialize uid, gid.  */
  uinfo_init ();

  syscall_printf ("Application CYGWIN version: %d.%d\n",
		  u->version_major, u->version_minor);
  syscall_printf ("CYGWIN DLL version        : %d.%d\n",
		  cygwin_dll_version_major, cygwin_dll_version_minor);

  /* Nasty static stuff needed by newlib  - point to a
     local copy of the reent stuff, and initialize it. */
  *(u->impure_ptr_ptr) = &reent_data;
  _impure_ptr = &reent_data;
  memset (&reent_data, 0, sizeof (reent_data));
  reent_data._errno = 0;
  reent_data._stdin =  reent_data.__sf + 0;
  reent_data._stdout = reent_data.__sf + 1;
  reent_data._stderr = reent_data.__sf + 2;

  /* Scan the command line and build argv.  */
  char *line = GetCommandLineA ();
  int len = strlen (line);
  char *mine = (char *)alloca (len + 2);
  memcpy (mine, line, len);
  mine[len] = 0;
  mine[len+1] = 0;
  argc = compute_argc (mine);
  argv =(char **) alloca ((argc + 1) * sizeof (char *));
  build_argv (mine, argv, len);

  /* Expand *.c, etc.  */
  if (! u->self->cygwin_parent_p)
    globify (&argc, &argv);

  /* Deal with environment variables.  */
  {
    const char * const rawenv = GetEnvironmentStrings ();

    int envc = num_ms_env_vars (rawenv);

    debug_printf ("Processing %d env vars ...\n", envc);

    /* Allocate space for these (+ the trailing NULL).  */
    *u->envptr = (char **)alloca ((envc + 1) * sizeof (char *));

    /* Current directory information is recorded as variables of the
       form "=X:=X:\foo\bar; these must be changed into something legal
       (we could just ignore them but maybe an application will
       eventually want to use them).  */
    int i = 0;
    int n = 0;
    while (rawenv[i])
      {
	char *c;
	c = (char *)alloca (strlen (rawenv + i) + 1);
	strcpy (c, rawenv + i);
	if (c[0] == '=')
	  c[0] = '!';
	
	(*u->envptr)[n] = c;
	
	/* Skip to the next one.  */
	while (rawenv[i])
	  ++i;
	++i;
	++n;
      }
    (*u->envptr)[n] = 0;

    /* Amazingly, NT has a case sensitive environment name list,
       only sometimes.
       eg, it's normal to have NT set your "Path" to something.
       Later, you set "PATH" to something else.  This alters "Path".
       But if you try and do a naive getenv on "PATH" you'll get nothing. 
       
       So we upper case the labels here to prevent confusion later. */
    
    for (i = 0; i < n; i++)
      {
	char *p;
	for (p = (*u->envptr)[i]; *p != '=' && *p; p++)
	  if (islower (*p))
	    *p = toupper (*p);
      }

    /* If we're using posix paths, we do a special thing for the PATH
       [and potentially others].  They might be in native format so we turn
       them into their posix equivalent.  */

    if (s->mount.posix_path_p ())
      {
	for (i = 0; i < n; i++)
	  {
	    for (int j = 0; conv_path_names[j]; j++) 
	      {
		char *src = (*u->envptr)[i];
		int namelen = strlen (conv_path_names[j]);

		if (strncasecmp (src, conv_path_names[j], namelen) != 0)
		  continue;

		/* Turn all the items in here from c:<foo>;<bar> into their 
		   mounted equivalents - if they have one.  */
		char *value = src + namelen;
		char *envvar = (char *) alloca (win32_to_posix_path_list_buf_size (value));
		char *outenv = envvar;

		debug_printf ("need to convert %d %s\n", i, src);

		memcpy (outenv, conv_path_names[j], namelen);
		outenv += namelen;

#if 0 /* Would rather not have this, but may have to.  */
		/* PATH in win32 always has a virtual ":" at the front
		   (the current directory is always searched first).  Ensure
		   PATH has a leading ":".  */
		if (strcmp (conv_path_names[j], "PATH=") == 0)
		  {
		    if (*value != ';'
			&& !(value[0] == '.' && value[1] == ';'))
		      *outenv++ = ':';
		  }
#endif

		win32_to_posix_path_list (value, outenv);

		debug_printf ("env var converted to %s\n", envvar);
		(*u->envptr)[i] = envvar;
	      }
	  }
      }

    /* FIXME: Sometimes the dll uses GetEnvironmentVariable,
       sometimes it uses getenv.  Need to pick *one*.  */
    environ = *u->envptr;
    FreeEnvironmentStringsA (rawenv);
  }

  /* Save the program name.  It's used in debugging messages and by
     the fork code (forking spawns a copy of us).  Copy it into a temp and
     then into the final spot because debugging messages use
     u->self->progname.
     This call references $PATH so we can't do this until the environment
     vars are set up.  */
  /* FIXME: What if argv[0] is relative, $PATH changes, and then the program
     tries to do a fork?  */
  {
    char tmp[MAX_PATH];

    find_exec (argv[0], tmp);
    strcpy (u->self->progname, tmp);
  }

  /* Convert argv[0] to posix rules if we're using them and it's currently
     blatantly win32 style.  */
  if (s->mount.posix_path_p ()
      && (strchr (argv[0], ':') || strchr (argv[0], '\\')))
    {
      char *new_argv0 = (char *) alloca (MAX_PATH);
      conv_to_posix_path (argv[0], new_argv0);
      argv[0] = new_argv0;
    }

  if (u->strace_mask & (_STRACE_STARTUP | _STRACE_ALL))
    {
      for (i = 0; i < argc; ++i)
	__sys_printf ("argv[%d] = %s\n", i, argv[i]);
      for (i = 0; (*u->envptr)[i]; ++i)
	__sys_printf ("envp[%d] = %x %s\n", i, (*u->envptr)[i], (*u->envptr)[i]);
    }

  syscall_printf ("CYGWIN Release %d.%d, compiled %s %s\n",
		  cygwin_dll_version_major, cygwin_dll_version_minor,
		  __DATE__, __TIME__);

  exit (u->main (argc, argv, environ));
}

/* Wrap the real one, otherwise gdb gets confused about
   two symbols with the same name, but different addresses.

   UPTR is a pointer to global data that lives on the libc side of the
   line [if one distinguishes the application from the dll].  */

void
dll_crt0 (per_process *uptr)
{
  dll_crt0_1 (uptr);
}

void
__do_global_dtors ()
{
  int i;
  void (**pfunc)() = u->dtors;

  /* Run dtors backwards, so skip the first entry and find how many
     there are, then run them.  */
  
  if (pfunc) 
    {
      for (i = 1; pfunc[i]; i++) 
	;
      for (i = i - 1; i > 0; i--)
	{
	  (pfunc[i])();
	}
    }
}

void
__do_global_ctors ()
{
  int i;
  void (**pfunc)() = u->ctors;
  
  for (i = 1; pfunc[i]; i++)
    {
      (pfunc[i])();
    }
  atexit (__do_global_dtors);
}

void
__main ()
{
  if (! u->run_ctors_p)
    {
      u->run_ctors_p = 1;
      __do_global_ctors ();
    }
}

void
_exit (int n)
{
  syscall_printf ("_exit (%d)\n", n);
  syscall_printf ("Terminating.\n");
  close_all_files ();
  u->self->terminate ();
  shared_terminate ();
  fork_terminate ();
  ExitProcess (n);
}

void
api_fatal (const char *msg)
{
  small_printf ("cygwin: %s\n", msg);
  ExitProcess (1);
}

/* This is needed to terminate the list of inport stuff */
/* FIXME: Doesn't the ppc port handle this differently?  Standardize.  */
asm (".section .idata$3\n" ".long 0,0,0,0,0,0,0,0");
